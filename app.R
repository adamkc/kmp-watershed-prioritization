# KMP Watershed Prioritization Tool
#
# v1 skeleton: upload a CSV, validate it, auto-detect the HUC level,
# join to bundled KMP-zone boundaries, and render the matched HUCs
# on an interactive map. Also shows the per-column classification.
#
# Scoring, weight sliders, and sensitivity analysis are stubbed for
# the next iteration.
#
# Run locally:
#   shiny::runApp()    # from the project root in RStudio, or
#   Rscript -e "shiny::runApp(launch.browser = TRUE)"

library(shiny)
library(bslib)
library(leaflet)
library(sf)

# Explicit sourcing (also auto-loaded by Shiny from R/ in recent versions).
source("R/input.R")
source("R/columns.R")

# Allow up to 30 MB uploads; user CSVs are tiny but we leave headroom.
options(shiny.maxRequestSize = 30 * 1024^2)


# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "KMP Watershed Prioritization",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 340,

    fileInput(
      "csv_file",
      label = "Upload input CSV",
      accept = c(".csv", "text/csv"),
      placeholder = "No file selected"
    ),

    uiOutput("status_panel"),

    tags$hr(),
    tags$div(
      class = "text-muted small",
      tags$strong("Coming next:"),
      tags$ul(
        tags$li("Weight slider per metric"),
        tags$li("Live composite score + choropleth"),
        tags$li("Monte Carlo sensitivity analysis"),
        tags$li("Exportable report")
      )
    )
  ),

  navset_card_tab(
    nav_panel(
      title = "Map",
      leafletOutput("map", height = "600px")
    ),
    nav_panel(
      title = "Columns",
      tags$p(class = "text-muted small mt-2",
             "Automatic classification of every column in the uploaded CSV. ",
             "Numeric columns with 5 or fewer unique values are treated as ordinal. ",
             "Zero-inflated columns are flagged -- Jenks classification may need ",
             "special handling there."),
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

  # ---- Reactive pipeline ---------------------------------------------------

  raw_df <- reactive({
    req(input$csv_file)
    tryCatch(
      read_input_csv(input$csv_file$datapath),
      error = function(e) {
        showNotification(
          paste("Could not read CSV:", conditionMessage(e)),
          type = "error", duration = 15
        )
        NULL
      }
    )
  })

  validated <- reactive({
    req(raw_df())
    tryCatch(
      validate_input(raw_df()),
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error", duration = 15
        )
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
      showNotification(
        paste("Geometry join failed:", conditionMessage(e)),
        type = "error", duration = 15
      )
      NULL
    })
  })

  classification <- reactive({
    req(validated())
    classify_columns(validated()$data)
  })


  # ---- Outputs -------------------------------------------------------------

  output$status_panel <- renderUI({
    v <- validated()
    if (is.null(v)) return(
      tags$div(class = "text-muted small",
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
        tags$div(tags$strong("Matched to geometry: "),
                 sprintf("%d of %d", j$n_matched, j$n_input)),
        if (missed > 0) {
          tags$div(class = "text-warning small mt-1",
                   sprintf("%d row(s) did not match any KMP-zone HUC%d.",
                           missed, v$huc_level))
        }
      )
    )
  })

  output$map <- renderLeaflet({
    # Base map always available, even before upload.
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -122.8, lat = 41.7, zoom = 9)
  })

  observe({
    j <- joined()
    req(j, !is.null(j$sf), nrow(j$sf) > 0)

    sf_wgs <- sf::st_transform(j$sf, 4326)

    leafletProxy("map") |>
      clearShapes() |>
      addPolygons(
        data = sf_wgs,
        weight = 1,
        color = "#333",
        fillColor = "#4a90e2",
        fillOpacity = 0.5,
        label = ~name,
        highlightOptions = highlightOptions(
          weight = 3, color = "#000", fillOpacity = 0.7, bringToFront = TRUE
        )
      ) |>
      fitBounds(
        lng1 = sf::st_bbox(sf_wgs)[["xmin"]],
        lat1 = sf::st_bbox(sf_wgs)[["ymin"]],
        lng2 = sf::st_bbox(sf_wgs)[["xmax"]],
        lat2 = sf::st_bbox(sf_wgs)[["ymax"]]
      )
  })

  output$column_table <- renderTable({
    cls <- classification()
    req(cls)
    cls[, c("name", "type", "n", "n_missing", "n_unique",
            "n_zero", "min", "max", "zero_inflated", "use_in_score")]
  }, digits = 4)

  output$diagnostics_panel <- renderUI({
    v <- validated()
    if (is.null(v)) return(
      tags$div(class = "text-muted small mt-2", "Upload a CSV to see diagnostics.")
    )
    j <- joined()
    cls <- classification()
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
      tags$p(sprintf("%d numeric columns will get weight sliders.",
                     nrow(scoring))),

      if (length(zero_flagged) > 0) tagList(
        tags$h6(class = "text-warning", "Zero-inflated (>=50% zeros)"),
        tags$p(class = "small",
               "Jenks may not produce meaningful breaks here. ",
               "These columns will need special handling."),
        tags$ul(lapply(zero_flagged, tags$li))
      ),

      if (nrow(missing_cols) > 0) tagList(
        tags$h6(class = "text-muted", "Columns with missing values"),
        tags$p(class = "small",
               "These will show a missing-count annotation on their sliders."),
        tags$ul(lapply(seq_len(nrow(missing_cols)), function(i) {
          tags$li(sprintf("%s  -  %d HUCs missing",
                          missing_cols$name[i], missing_cols$n_missing[i]))
        }))
      )
    )
  })
}


shinyApp(ui, server)
