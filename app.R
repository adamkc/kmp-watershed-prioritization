# KMP Watershed Prioritization Tool
#
# Three-step workflow:
#   1. Sub-zone         (dropdown; currently just "Full KMP")
#   2. Metrics source   (prebuilt scenario, custom metric list, or CSV upload)
#   3. Weights          (live sliders; reactive map + ranked table)
#
# Steps 1 and 2 collapse after a selection so the user ends up focused
# on the sliders. Scenarios live in data/scenarios.yaml so non-developers
# can add or refine them without touching code.
#
# Run locally:
#   shiny::runApp()    # from the project root in RStudio
#   Rscript -e "shiny::runApp(launch.browser = TRUE)"

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(DT)

source("R/input.R")
source("R/columns.R")
source("R/score.R")
source("R/scenarios.R")

options(shiny.maxRequestSize = 30 * 1024^2)

# Derive a Shiny-safe input ID from a (possibly messy) column name.
slider_id <- function(column_name) {
  paste0("w_", gsub("[^A-Za-z0-9]+", "_", column_name))
}

# KMP zone outline, loaded once at app start.
KMP_BOUNDARY <- sf::st_read("data/kmp_boundary.geojson", quiet = TRUE)
KMP_BBOX     <- sf::st_bbox(KMP_BOUNDARY)

# Master metrics + scenario catalog, loaded once.
MASTER_RAW      <- read_input_csv("data/kmp_metrics.csv")
MASTER_VALID    <- validate_input(MASTER_RAW)
MASTER_DATA     <- MASTER_VALID$data
MASTER_LEVEL    <- MASTER_VALID$huc_level
MASTER_CLASS    <- classify_columns(MASTER_DATA)
MASTER_METRICS  <- MASTER_CLASS$name[MASTER_CLASS$use_in_score]

SCENARIOS       <- load_scenarios("data/scenarios.yaml")
SCENARIO_CHECK  <- validate_scenarios(SCENARIOS, MASTER_METRICS)

# Sub-zone catalog. Single entry for now -- room to add KMP sub-units later.
SUBZONES <- list(
  list(id = "full_kmp", name = "Full KMP", description = "All HUC12s in the KMP zone.")
)
SUBZONE_CHOICES <- setNames(
  sapply(SUBZONES, `[[`, "id"),
  sapply(SUBZONES, `[[`, "name")
)


# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "KMP Watershed Prioritization",
  theme = bs_theme(bootswatch = "flatly"),

  tags$head(tags$style(HTML("
    .slider-row { display: flex; align-items: center; gap: 10px; margin-bottom: 2px; }
    .slider-name { width: 46%; flex-shrink: 0; font-size: 0.8rem;
                   line-height: 1.15; word-break: break-word; }
    .slider-annot { font-size: 0.68rem; color: #b45309; margin-top: 2px; }
    .slider-control { flex: 1; min-width: 0; }
    .slider-control .form-group { margin-bottom: 0; }
    .slider-control label.control-label { display: none; }
    .slider-control .irs { margin-top: 0; margin-bottom: 0; }
    .slider-control .irs--shiny .irs-single,
    .slider-control .irs--shiny .irs-min,
    .slider-control .irs--shiny .irs-max { font-size: 0.68rem; }
    .step-summary { font-size: 0.8rem; color: #5a6268; margin-top: 2px; }
    .scenario-btn { text-align: left; white-space: normal; }
    .accordion-button { font-weight: 600; }
  "))),

  sidebar = sidebar(
    width = 380,
    uiOutput("workflow_ui")
  ),

  navset_card_tab(
    id = "main_tabs",

    nav_panel(
      title = "Map",
      leafletOutput("map", height = "620px")
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
      title = "Diagnostics",
      uiOutput("diagnostics_panel")
    )
  )
)


# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Flag any scenario metric mismatches at startup so maintainers notice.
  if (!SCENARIO_CHECK$ok) {
    for (msg in SCENARIO_CHECK$issues) warning(msg, call. = FALSE)
  }

  # ---- Workflow state ------------------------------------------------------

  rv <- reactiveValues(
    subzone_id     = "full_kmp",
    source_type    = NULL,           # "scenario" | "custom" | "upload"
    source_label   = "not yet chosen",
    active_metrics = character(0),
    init_weights   = numeric(0),
    uploaded       = NULL            # list(data, huc_level) from upload path
  )


  # ---- Active dataset (master by default; uploaded replaces it) ------------

  active_data <- reactive({
    if (identical(rv$source_type, "upload") && !is.null(rv$uploaded)) {
      rv$uploaded$data
    } else {
      MASTER_DATA
    }
  })

  active_huc_level <- reactive({
    if (identical(rv$source_type, "upload") && !is.null(rv$uploaded)) {
      rv$uploaded$huc_level
    } else {
      MASTER_LEVEL
    }
  })

  active_classification <- reactive({
    classify_columns(active_data())
  })

  active_joined <- reactive({
    d <- active_data(); lvl <- active_huc_level()
    req(d, lvl)
    bnd <- load_boundaries(lvl)
    join_input_to_boundaries(d, bnd, lvl)
  })

  active_bin_scores <- reactive({
    compute_bin_scores(active_data(), active_classification())
  })


  # ---- Workflow UI (renders the 3-step accordion + sliders) ----------------

  output$workflow_ui <- renderUI({
    step1_title <- sprintf("Step 1. Sub-zone \u00B7 %s",
                           names(SUBZONE_CHOICES)[SUBZONE_CHOICES == rv$subzone_id])
    step2_title <- sprintf("Step 2. Metrics \u00B7 %s", rv$source_label)
    n_active <- length(rv$active_metrics)
    step3_title <- sprintf("Step 3. Weights \u00B7 %d metric%s",
                           n_active, if (n_active == 1) "" else "s")

    # Which panels are open? Default: step 1. After selection: step 3.
    open_panels <- if (is.null(rv$source_type)) "step1" else "step3"

    tagList(
      accordion(
        id = "steps",
        open = open_panels,

        # --- Step 1: sub-zone ---
        accordion_panel(
          value = "step1",
          title = step1_title,
          selectInput("subzone", label = NULL,
                      choices = SUBZONE_CHOICES,
                      selected = rv$subzone_id, width = "100%"),
          p(class = "text-muted small mb-0",
            "Sub-zones narrow the analysis to a specific portion of the KMP ",
            "zone. For now the only option is Full KMP; more will be added ",
            "as they are defined.")
        ),

        # --- Step 2: metrics source ---
        accordion_panel(
          value = "step2",
          title = step2_title,

          p(class = "text-muted small",
            "Choose a prebuilt scenario, build a custom metric list, or ",
            "upload your own CSV."),

          div(class = "d-grid gap-2 mb-3",
            # One button per scenario from the YAML catalog.
            lapply(SCENARIOS, function(s) {
              actionButton(paste0("pick_scenario_", s$id),
                           label = s$name,
                           class = "btn-outline-primary scenario-btn",
                           width = "100%")
            }),
            actionButton("pick_custom", "Custom metric list...",
                         class = "btn-outline-primary scenario-btn",
                         width = "100%"),
            actionButton("pick_upload", "Upload CSV...",
                         class = "btn-outline-secondary scenario-btn",
                         width = "100%")
          ),

          # File input surfaces only when the user clicked "Upload CSV".
          if (identical(rv$source_type, "upload") || !is.null(input$pick_upload)) {
            conditionalPanel(
              "true",
              fileInput("csv_file", label = "Upload CSV",
                        accept = c(".csv", "text/csv"),
                        placeholder = "No file selected")
            )
          }
        ),

        # --- Step 3: weights ---
        accordion_panel(
          value = "step3",
          title = step3_title,

          if (length(rv$active_metrics) == 0) {
            p(class = "text-muted small",
              "Pick a scenario or metric list in Step 2 to see sliders here.")
          } else {
            tagList(
              div(class = "d-flex gap-2 mb-3",
                  actionButton("weights_all_1", "All to 1",
                               class = "btn-sm btn-outline-secondary flex-fill"),
                  actionButton("weights_all_0", "All to 0",
                               class = "btn-sm btn-outline-secondary flex-fill")),

              lapply(rv$active_metrics, function(nm) {
                sid <- slider_id(nm)
                init_val <- rv$init_weights[[nm]] %||% 1

                # Pull annotation data from classification (whichever data source).
                cls <- active_classification()
                row_idx <- which(cls$name == nm)
                n_miss  <- if (length(row_idx)) cls$n_missing[row_idx] else 0
                zero_inf <- if (length(row_idx)) cls$zero_inflated[row_idx] else FALSE
                zero_pct <- if (length(row_idx) && cls$n[row_idx] > 0)
                  round(100 * cls$n_zero[row_idx] / cls$n[row_idx]) else 0

                annotations <- c(
                  if (isTRUE(n_miss > 0))   sprintf("%d missing", n_miss) else NULL,
                  if (isTRUE(zero_inf))     sprintf("%d%% zeros", zero_pct) else NULL
                )

                div(class = "slider-row",
                  div(class = "slider-name",
                      nm,
                      if (length(annotations) > 0) {
                        div(class = "slider-annot",
                            paste0("\u26A0 ",
                                   paste(annotations, collapse = "  \u00B7  ")))
                      }),
                  div(class = "slider-control",
                      sliderInput(sid, label = NULL,
                                  min = 0, max = 5, value = init_val,
                                  step = 0.1, width = "100%"))
                )
              })
            )
          }
        )
      )
    )
  })


  # ---- Event handlers: scenario / custom / upload pickers ------------------

  apply_scenario <- function(scenario) {
    if (is.null(scenario)) return()
    # Intersect scenario metrics with what's actually in the current data.
    cls <- active_classification()
    valid <- intersect(names(scenario$weights), cls$name[cls$use_in_score])
    rv$source_type    <- "scenario"
    rv$source_label   <- scenario$name
    rv$active_metrics <- valid
    rv$init_weights   <- scenario$weights[valid]
    accordion_panel_close(id = "steps", values = c("step1", "step2"))
    accordion_panel_open(id  = "steps", values = "step3")
  }

  # One observer per scenario button (registered at startup; closures
  # capture each scenario object by local scoping).
  observe({
    for (s in SCENARIOS) {
      local({
        scen <- s
        observeEvent(input[[paste0("pick_scenario_", scen$id)]], {
          apply_scenario(scen)
        }, ignoreInit = TRUE)
      })
    }
  })

  # --- Custom metric list: open modal ---
  observeEvent(input$pick_custom, {
    cls <- active_classification()
    metric_opts <- cls$name[cls$use_in_score]

    showModal(modalDialog(
      title = "Select metrics to include",
      easyClose = TRUE, size = "l",
      p(class = "text-muted small",
        "Check the metrics you want in the composite score. Weights ",
        "default to 1 and are adjustable in Step 3."),
      checkboxGroupInput("custom_metrics", label = NULL,
                         choices = metric_opts,
                         selected = rv$active_metrics),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("custom_ok", "Use selected metrics",
                     class = "btn-primary")
      )
    ))
  })

  observeEvent(input$custom_ok, {
    chosen <- input$custom_metrics
    if (is.null(chosen) || length(chosen) == 0) {
      showNotification("Pick at least one metric.", type = "warning")
      return()
    }
    rv$source_type    <- "custom"
    rv$source_label   <- sprintf("Custom list (%d metrics)", length(chosen))
    rv$active_metrics <- chosen
    rv$init_weights   <- setNames(rep(1, length(chosen)), chosen)
    removeModal()
    accordion_panel_close(id = "steps", values = c("step1", "step2"))
    accordion_panel_open(id  = "steps", values = "step3")
  })

  # --- Upload CSV path ---
  observeEvent(input$pick_upload, {
    # The conditional file input rendered in renderUI will now show; the
    # user still needs to actually pick a file. We don't collapse yet.
    rv$source_type  <- "upload"
    rv$source_label <- "Upload pending"
  })

  observeEvent(input$csv_file, {
    req(input$csv_file)
    raw <- tryCatch(read_input_csv(input$csv_file$datapath),
                    error = function(e) {
                      showNotification(paste("Could not read CSV:",
                                             conditionMessage(e)),
                                       type = "error", duration = 15)
                      NULL
                    })
    req(raw)
    v <- tryCatch(validate_input(raw),
                  error = function(e) {
                    showNotification(conditionMessage(e),
                                     type = "error", duration = 15)
                    NULL
                  })
    req(v)

    rv$uploaded       <- v
    cls <- classify_columns(v$data)
    scoring <- cls$name[cls$use_in_score]
    rv$source_type    <- "upload"
    rv$source_label   <- sprintf("Uploaded (%d metrics)", length(scoring))
    rv$active_metrics <- scoring
    rv$init_weights   <- setNames(rep(1, length(scoring)), scoring)
    accordion_panel_close(id = "steps", values = c("step1", "step2"))
    accordion_panel_open(id  = "steps", values = "step3")
  })

  # --- Sub-zone selector ---
  observeEvent(input$subzone, {
    rv$subzone_id <- input$subzone
  }, ignoreInit = TRUE)


  # ---- Weight bulk-set ------------------------------------------------------

  apply_uniform_weight <- function(val) {
    for (nm in rv$active_metrics) {
      updateSliderInput(session, slider_id(nm), value = val)
    }
  }
  observeEvent(input$weights_all_1, apply_uniform_weight(1))
  observeEvent(input$weights_all_0, apply_uniform_weight(0))


  # ---- Composite + ranking reactive pipeline -------------------------------

  weights <- reactive({
    active <- rv$active_metrics
    if (length(active) == 0) return(setNames(numeric(0), character(0)))
    vals <- vapply(active, function(nm) {
      v <- input[[slider_id(nm)]]
      if (is.null(v)) rv$init_weights[[nm]] %||% 1 else as.numeric(v)
    }, numeric(1))
    setNames(vals, active)
  })

  composite <- reactive({
    bs <- active_bin_scores(); w <- weights()
    req(bs)
    if (length(w) == 0) return(rep(NA_real_, nrow(bs)))
    composite_score(bs, w)
  })

  label_col_name <- reactive({
    cls <- active_classification()
    lbl <- cls$name[cls$type == "label"]
    if (length(lbl) == 0) NA_character_ else lbl[1]
  })

  ranking <- reactive({
    d <- active_data(); req(d)
    comp <- composite()
    lname <- label_col_name()
    name_vals <- if (!is.na(lname)) d[[lname]] else rep(NA_character_, length(comp))

    data.frame(
      rank    = rank_hucs(comp),
      huccode = d$huccode,
      name    = name_vals,
      score   = round(comp, 3),
      stringsAsFactors = FALSE
    )
  })


  # ---- Map: base tiles + boundary + reactive choropleth --------------------

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        data = KMP_BOUNDARY, group = "kmp_boundary",
        fill = TRUE, fillColor = "#e9ecef", fillOpacity = 0.25,
        color = "#1f4f8b", weight = 2, opacity = 0.85,
        label = "KMP zone"
      ) |>
      fitBounds(
        lng1 = KMP_BBOX[["xmin"]], lat1 = KMP_BBOX[["ymin"]],
        lng2 = KMP_BBOX[["xmax"]], lat2 = KMP_BBOX[["ymax"]]
      )
  })

  observe({
    j <- active_joined()
    req(j, !is.null(j$sf), nrow(j$sf) > 0)
    rk <- ranking(); req(rk)

    sf_wgs <- sf::st_transform(j$sf, 4326)
    huc_col <- paste0("huc", active_huc_level())
    idx <- match(sf_wgs[[huc_col]], rk$huccode)
    sf_wgs$composite  <- rk$score[idx]
    sf_wgs$rank_pos   <- rk$rank[idx]
    sf_wgs$display_nm <- rk$name[idx]

    comp_vals <- sf_wgs$composite
    all_na <- all(is.na(comp_vals))
    n_huc <- sum(!is.na(sf_wgs$rank_pos))

    labels <- if (all_na) {
      sprintf("<strong>%s</strong><br/><em>No weights active</em>",
              ifelse(is.na(sf_wgs$display_nm), sf_wgs$name, sf_wgs$display_nm))
    } else {
      sprintf("<strong>%s</strong><br/>Rank: %d of %d<br/>Score: %.2f",
              ifelse(is.na(sf_wgs$display_nm), sf_wgs$name, sf_wgs$display_nm),
              sf_wgs$rank_pos, n_huc, sf_wgs$composite)
    } |> lapply(htmltools::HTML)

    top3_mask <- !is.na(sf_wgs$rank_pos) & sf_wgs$rank_pos <= 3
    top3 <- sf_wgs[top3_mask, ]
    top3_pts <- if (nrow(top3) > 0) {
      suppressWarnings(sf::st_point_on_surface(top3))
    } else NULL

    proxy <- leafletProxy("map") |>
      clearGroup("hucs") |> clearGroup("top3") |> clearControls()

    if (all_na) {
      proxy <- proxy |>
        addPolygons(
          data = sf_wgs, group = "hucs",
          weight = 1, color = "#333",
          fillColor = "#cccccc", fillOpacity = 0.5,
          label = labels,
          labelOptions = labelOptions(direction = "auto", textsize = "12px"),
          highlightOptions = highlightOptions(
            weight = 3, color = "#000", fillOpacity = 0.7, bringToFront = TRUE
          )
        )
    } else {
      pal <- colorNumeric("YlOrRd", domain = comp_vals, na.color = "#cccccc")
      proxy <- proxy |>
        addPolygons(
          data = sf_wgs, group = "hucs",
          weight = 1, color = "#333",
          fillColor = ~pal(composite),
          fillOpacity = 0.8,
          label = labels,
          labelOptions = labelOptions(direction = "auto", textsize = "12px"),
          highlightOptions = highlightOptions(
            weight = 3, color = "#000", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) |>
        addLegend(
          position = "bottomright", pal = pal, values = comp_vals,
          title = "Composite<br/>score", opacity = 0.85
        )
    }

    proxy <- proxy |>
      fitBounds(
        lng1 = sf::st_bbox(sf_wgs)[["xmin"]], lat1 = sf::st_bbox(sf_wgs)[["ymin"]],
        lng2 = sf::st_bbox(sf_wgs)[["xmax"]], lat2 = sf::st_bbox(sf_wgs)[["ymax"]]
      )

    if (!is.null(top3_pts) && nrow(top3_pts) > 0) {
      proxy |> addLabelOnlyMarkers(
        data = top3_pts, group = "top3",
        label = sprintf("#%d", top3$rank_pos),
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textOnly = FALSE,
          style = list(
            "font-size" = "13px", "font-weight" = "700",
            "color" = "#111", "padding" = "2px 7px",
            "background" = "rgba(255,255,255,0.95)",
            "border" = "1px solid #333", "border-radius" = "11px",
            "box-shadow" = "0 1px 2px rgba(0,0,0,0.25)"
          )
        )
      )
    }
  })


  # ---- Ranked HUCs tab -----------------------------------------------------

  output$ranking_table <- renderDT({
    rk <- ranking(); req(rk)
    bs <- active_bin_scores(); req(bs)
    # Only include bin-score columns for currently active metrics.
    keep_cols <- intersect(rv$active_metrics, names(bs))
    bs_active <- if (length(keep_cols) > 0) bs[, keep_cols, drop = FALSE] else bs[, 0]
    datatable(
      cbind(rk, bs_active),
      rownames = FALSE,
      options = list(
        pageLength = 25, scrollX = TRUE,
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
      bs <- active_bin_scores()
      keep_cols <- intersect(rv$active_metrics, names(bs))
      bs_active <- if (length(keep_cols) > 0) bs[, keep_cols, drop = FALSE] else bs[, 0]
      write.csv(cbind(ranking(), bs_active), file, row.names = FALSE)
    }
  )


  # ---- Diagnostics tab -----------------------------------------------------

  output$diagnostics_panel <- renderUI({
    j <- active_joined(); cls <- active_classification()
    if (is.null(j) || is.null(cls)) return(
      tags$div(class = "text-muted small mt-2", "Waiting for data.")
    )

    zero_flagged <- cls$name[cls$zero_inflated & cls$use_in_score]
    missing_cols <- cls[cls$n_missing > 0 & cls$use_in_score,
                        c("name", "n_missing")]

    # Scenario validation issues surface here too.
    scenario_block <- if (!SCENARIO_CHECK$ok) tagList(
      tags$h5(class = "text-danger mt-3", "Scenario catalog issues"),
      tags$ul(lapply(SCENARIO_CHECK$issues, tags$li))
    ) else NULL

    tagList(
      scenario_block,

      tags$h5("Active source", class = "mt-3"),
      tags$ul(
        tags$li(sprintf("Source: %s",
                        if (is.null(rv$source_type)) "(none yet)" else rv$source_type)),
        tags$li(sprintf("Label: %s", rv$source_label)),
        tags$li(sprintf("Active metrics: %d", length(rv$active_metrics)))
      ),

      tags$h5("Join diagnostics", class = "mt-3"),
      tags$ul(
        tags$li(sprintf("Input rows: %d", j$n_input)),
        tags$li(sprintf("Matched to HUC%d boundaries: %d",
                        active_huc_level(), j$n_matched)),
        tags$li(sprintf("Unmatched IDs in input: %d",
                        length(j$unmatched_ids))),
        tags$li(sprintf("Boundary features with no input row: %d",
                        j$n_unused_geom))
      ),

      if (length(j$unmatched_ids) > 0) tagList(
        tags$h6("Unmatched huccodes"),
        tags$code(paste(head(j$unmatched_ids, 20), collapse = ", "))
      ),

      if (length(zero_flagged) > 0) tagList(
        tags$h6(class = "text-warning mt-3", "Zero-inflated columns (>=50% zeros)"),
        tags$ul(lapply(zero_flagged, tags$li))
      ),

      if (nrow(missing_cols) > 0) tagList(
        tags$h6(class = "text-muted mt-3", "Columns with missing values"),
        tags$ul(lapply(seq_len(nrow(missing_cols)), function(i) {
          tags$li(sprintf("%s  -  %d missing",
                          missing_cols$name[i], missing_cols$n_missing[i]))
        }))
      )
    )
  })
}


shinyApp(ui, server)
