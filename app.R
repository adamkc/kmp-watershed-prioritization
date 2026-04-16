# KMP Watershed Prioritization Tool
#
# Upload a CSV -> validate -> classify columns -> join to bundled
# KMP-zone HUC boundaries. Weight sliders drive a live composite
# score; the map is a choropleth of the composite, and the Ranked
# HUCs tab shows the sortable ranking plus per-metric bin scores.
#
# Run locally:
#   shiny::runApp()    # from the project root in RStudio, or
#   Rscript -e "shiny::runApp(launch.browser = TRUE)"

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(DT)

source("R/input.R")
source("R/columns.R")
source("R/score.R")

options(shiny.maxRequestSize = 30 * 1024^2)

# Derive a Shiny-safe input ID from a (possibly messy) column name.
slider_id <- function(column_name) {
  paste0("w_", gsub("[^A-Za-z0-9]+", "_", column_name))
}


# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "KMP Watershed Prioritization",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 360,

    fileInput("csv_file", label = "Upload input CSV",
              accept = c(".csv", "text/csv"),
              placeholder = "No file selected"),

    uiOutput("status_panel"),
    uiOutput("controls_panel")
  ),

  navset_card_tab(
    id = "main_tabs",

    nav_panel(
      title = "Map",
      leafletOutput("map", height = "600px")
    ),

    nav_panel(
      title = "Ranked HUCs",
      div(class = "d-flex justify-content-between align-items-center my-2",
          p(class = "text-muted small mb-0",
            "Click any column header to sort. Per-metric bin scores (1-5) shown right of the composite."),
          downloadButton("download_ranking", "Download CSV",
                         class = "btn-sm btn-outline-primary")),
      DTOutput("ranking_table")
    ),

    nav_panel(
      title = "Columns",
      p(class = "text-muted small mt-2",
        "Automatic classification of every column in the uploaded CSV."),
      tableOutput("column_table")
    ),

    nav_panel(
      title = "Diagnostics",
      uiOutput("diagnostics_panel")
    )
  )
)


# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Reactive pipeline: upload -> validate -> classify -> bins -----------

  raw_df <- reactive({
    req(input$csv_file)
    tryCatch(
      read_input_csv(input$csv_file$datapath),
      error = function(e) {
        showNotification(paste("Could not read CSV:", conditionMessage(e)),
                         type = "error", duration = 15)
        NULL
      }
    )
  })

  validated <- reactive({
    req(raw_df())
    tryCatch(
      validate_input(raw_df()),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 15)
        NULL
      }
    )
  })

  joined <- reactive({
    req(validated())
    v <- validated()
    tryCatch({
      bnd <- load_boundaries(v$huc_level)
      join_input_to_boundaries(v$data, bnd, v$huc_level)
    }, error = function(e) {
      showNotification(paste("Geometry join failed:", conditionMessage(e)),
                       type = "error", duration = 15)
      NULL
    })
  })

  classification <- reactive({
    req(validated())
    classify_columns(validated()$data)
  })

  bin_scores <- reactive({
    v <- validated(); cls <- classification()
    req(v, cls)
    compute_bin_scores(v$data, cls)
  })


  # ---- Weight slider UI (rebuilt on each new upload) -----------------------

  output$controls_panel <- renderUI({
    cls <- classification()
    if (is.null(cls)) return(NULL)
    scoring <- cls[cls$use_in_score, ]

    tagList(
      tags$hr(),
      div(class = "d-flex justify-content-between align-items-center mb-2",
          tags$strong(sprintf("Weights (%d metrics)", nrow(scoring))),
          actionButton("reset_weights", "Reset",
                       class = "btn-sm btn-outline-secondary")),

      lapply(seq_len(nrow(scoring)), function(i) {
        nm <- scoring$name[i]
        sid <- slider_id(nm)
        n_miss  <- scoring$n_missing[i]
        zero_inf <- scoring$zero_inflated[i]
        zero_pct <- if (scoring$n[i] > 0)
          round(100 * scoring$n_zero[i] / scoring$n[i]) else 0

        annotations <- c(
          if (n_miss > 0)  sprintf("%d HUCs missing", n_miss) else NULL,
          if (zero_inf)    sprintf("%d%% zeros", zero_pct) else NULL
        )

        div(class = "mb-1",
          sliderInput(sid, label = nm, min = 0, max = 5,
                      value = 1, step = 0.1, width = "100%"),
          if (length(annotations) > 0) {
            div(style = "font-size: 0.72rem; color: #b45309; margin-top: -10px; margin-bottom: 6px;",
                paste("\u26A0", paste(annotations, collapse = "  \u00B7  ")))
          }
        )
      })
    )
  })

  observeEvent(input$reset_weights, {
    cls <- classification()
    req(cls)
    scoring <- cls[cls$use_in_score, ]
    for (nm in scoring$name) {
      updateSliderInput(session, slider_id(nm), value = 1)
    }
  })

  weights <- reactive({
    cls <- classification(); req(cls)
    scoring <- cls[cls$use_in_score, ]
    vals <- vapply(scoring$name, function(nm) {
      v <- input[[slider_id(nm)]]
      if (is.null(v)) 1 else as.numeric(v)
    }, numeric(1))
    setNames(vals, scoring$name)
  })

  composite <- reactive({
    bs <- bin_scores(); w <- weights()
    req(bs, w)
    composite_score(bs, w)
  })

  # Find the first "label" column from classification (usually HUC name).
  label_col_name <- reactive({
    cls <- classification(); req(cls)
    lbl <- cls$name[cls$type == "label"]
    if (length(lbl) == 0) NA_character_ else lbl[1]
  })

  ranking <- reactive({
    v <- validated(); req(v)
    comp <- composite()
    lname <- label_col_name()
    name_vals <- if (!is.na(lname)) v$data[[lname]] else rep(NA_character_, length(comp))

    data.frame(
      rank    = rank_hucs(comp),
      huccode = v$data$huccode,
      name    = name_vals,
      score   = round(comp, 3),
      stringsAsFactors = FALSE
    )
  })


  # ---- Sidebar status panel ------------------------------------------------

  output$status_panel <- renderUI({
    v <- validated()
    if (is.null(v)) return(
      tags$div(class = "text-muted small mt-2",
               "Upload a CSV with a `huccode` column to begin.")
    )
    j <- joined()
    if (is.null(j)) return(NULL)
    missed <- j$n_input - j$n_matched

    tagList(
      tags$div(class = "mt-2",
        tags$div(tags$strong("HUC level: "),
                 tags$span(class = "badge bg-primary",
                           paste0("HUC", v$huc_level))),
        tags$div(class = "mt-1",
                 tags$strong("Rows: "), nrow(v$data)),
        tags$div(tags$strong("Matched: "),
                 sprintf("%d of %d", j$n_matched, j$n_input)),
        if (missed > 0) tags$div(class = "text-warning small mt-1",
                                 sprintf("%d row(s) unmatched.", missed))
      )
    )
  })


  # ---- Map: base tiles + reactive choropleth -------------------------------

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -122.8, lat = 41.7, zoom = 9)
  })

  observe({
    j <- joined()
    req(j, !is.null(j$sf), nrow(j$sf) > 0)
    rk <- ranking(); req(rk)

    sf_wgs <- sf::st_transform(j$sf, 4326)
    huc_col <- paste0("huc", validated()$huc_level)

    # Align ranking to polygons by huccode.
    idx <- match(sf_wgs[[huc_col]], rk$huccode)
    sf_wgs$composite   <- rk$score[idx]
    sf_wgs$rank_pos    <- rk$rank[idx]
    sf_wgs$display_nm  <- rk$name[idx]

    comp_vals <- sf_wgs$composite
    pal <- colorNumeric("YlOrRd", domain = comp_vals, na.color = "#cccccc")

    n_huc <- sum(!is.na(sf_wgs$rank_pos))
    labels <- sprintf(
      "<strong>%s</strong><br/>Rank: %d of %d<br/>Score: %.2f",
      ifelse(is.na(sf_wgs$display_nm), sf_wgs$name, sf_wgs$display_nm),
      sf_wgs$rank_pos, n_huc, sf_wgs$composite
    ) |> lapply(htmltools::HTML)

    leafletProxy("map") |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        data = sf_wgs,
        weight = 1, color = "#333",
        fillColor = ~pal(composite),
        fillOpacity = 0.75,
        label = labels,
        labelOptions = labelOptions(direction = "auto", textsize = "12px"),
        highlightOptions = highlightOptions(
          weight = 3, color = "#000", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal, values = comp_vals,
        title = "Composite<br/>score", opacity = 0.85
      ) |>
      fitBounds(
        lng1 = sf::st_bbox(sf_wgs)[["xmin"]],
        lat1 = sf::st_bbox(sf_wgs)[["ymin"]],
        lng2 = sf::st_bbox(sf_wgs)[["xmax"]],
        lat2 = sf::st_bbox(sf_wgs)[["ymax"]]
      )
  })


  # ---- Ranked HUCs tab -----------------------------------------------------

  output$ranking_table <- renderDT({
    rk <- ranking(); req(rk)
    bs <- bin_scores(); req(bs)

    full <- cbind(rk, bs)

    datatable(
      full,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, "asc")),
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      )
    )
  })

  output$download_ranking <- downloadHandler(
    filename = function() {
      sprintf("kmp_ranking_%s.csv", format(Sys.time(), "%Y%m%d_%H%M"))
    },
    content = function(file) {
      write.csv(cbind(ranking(), bin_scores()), file, row.names = FALSE)
    }
  )


  # ---- Columns tab ---------------------------------------------------------

  output$column_table <- renderTable({
    cls <- classification(); req(cls)
    cls[, c("name", "type", "n", "n_missing", "n_unique",
            "n_zero", "min", "max", "zero_inflated", "use_in_score")]
  }, digits = 4)


  # ---- Diagnostics tab -----------------------------------------------------

  output$diagnostics_panel <- renderUI({
    v <- validated()
    if (is.null(v)) return(
      tags$div(class = "text-muted small mt-2",
               "Upload a CSV to see diagnostics.")
    )
    j <- joined(); cls <- classification()
    if (is.null(j) || is.null(cls)) return(NULL)

    scoring <- cls[cls$use_in_score, ]
    zero_flagged <- cls[cls$zero_inflated & cls$use_in_score, "name"]
    missing_cols <- cls[cls$n_missing > 0 & cls$use_in_score,
                        c("name", "n_missing")]

    tagList(
      tags$h5("Join diagnostics", class = "mt-3"),
      tags$ul(
        tags$li(sprintf("Input rows: %d", j$n_input)),
        tags$li(sprintf("Matched to %s boundaries: %d",
                        paste0("HUC", v$huc_level), j$n_matched)),
        tags$li(sprintf("Unmatched IDs in input: %d",
                        length(j$unmatched_ids))),
        tags$li(sprintf("Boundary features with no input row: %d",
                        j$n_unused_geom))
      ),
      if (length(j$unmatched_ids) > 0) tagList(
        tags$h6("Unmatched huccodes"),
        tags$code(paste(head(j$unmatched_ids, 20), collapse = ", "))
      ),

      tags$h5("Scoring columns", class = "mt-3"),
      tags$p(sprintf("%d numeric columns get weight sliders.",
                     nrow(scoring))),

      if (length(zero_flagged) > 0) tagList(
        tags$h6(class = "text-warning", "Zero-inflated (>=50% zeros)"),
        tags$p(class = "small",
               "Jenks may compress these into fewer classes and rescale. ",
               "Differences between zero and non-zero values drive the score."),
        tags$ul(lapply(zero_flagged, tags$li))
      ),

      if (nrow(missing_cols) > 0) tagList(
        tags$h6(class = "text-muted", "Columns with missing values"),
        tags$p(class = "small",
               "Missing values drop that metric from the HUC's composite; ",
               "the composite is the weighted average over non-missing metrics."),
        tags$ul(lapply(seq_len(nrow(missing_cols)), function(i) {
          tags$li(sprintf("%s  -  %d HUCs missing",
                          missing_cols$name[i], missing_cols$n_missing[i]))
        }))
      )
    )
  })
}


shinyApp(ui, server)
