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

# Cold-start profiler. Wrap eager loads with .kmp_time("label", expr) and
# read STARTUP_TIMINGS in the Diagnostics tab. Phases also message() to
# stderr so they show up in the browser console under shinylive.
STARTUP_TIMINGS <- list()
.kmp_time <- function(label, expr) {
  t0  <- Sys.time()
  out <- force(expr)
  dt  <- as.numeric(Sys.time() - t0, units = "secs")
  STARTUP_TIMINGS[[label]] <<- dt
  message(sprintf("[startup] %-32s %6.2fs", label, dt))
  out
}
.KMP_T0 <- Sys.time()

source("R/input.R")
source("R/columns.R")
source("R/score.R")
source("R/scenarios.R")
source("R/metrics.R")
source("R/sensitivity.R")
source("R/report.R")

options(shiny.maxRequestSize = 30 * 1024^2)

# Chrome + shinylive download workaround (Chromium issue 468227). Under
# shinylive the file is served by the service worker; Chrome's handling of
# the anchor's `download` attribute conflicts with that and rejects the
# download ("file wasn't available on site"). Stripping the attribute lets
# Chrome follow the URL and honour the Content-Disposition filename, and
# it still works in Firefox. Overriding the name here fixes every
# downloadButton in the UI at once.
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Derive a Shiny-safe input ID from a (possibly messy) column name.
slider_id <- function(column_name) {
  paste0("w_", gsub("[^A-Za-z0-9]+", "_", column_name))
}

# Per-tab DRAFT marker. Sits at the top of each nav_panel content area
# so the DRAFT status is obvious no matter which tab the user lands on.
draft_tab_header <- function(extra_text = NULL) {
  div(class = "draft-tab-header",
      tags$span(class = "draft-pill", "DRAFT"),
      tags$em(extra_text %||%
              "Output is illustrative -- under active development."))
}

# Inline "?" help icon with a bslib tooltip. Use in label strings
# to explain a control without cluttering the label text.
help_icon <- function(text, placement = "top") {
  bslib::tooltip(
    tags$span(
      style = "display: inline-block; width: 16px; height: 16px;
               border-radius: 50%; background: #6c757d; color: white;
               font-size: 11px; font-weight: 700; text-align: center;
               line-height: 16px; margin-left: 4px; cursor: help;
               vertical-align: middle;",
      "?"
    ),
    text,
    placement = placement
  )
}

# KMP zone outline, loaded once at app start.
KMP_BOUNDARY <- .kmp_time("read kmp_boundary.geojson",
  sf::st_read("data/kmp_boundary.geojson", quiet = TRUE))
KMP_BBOX     <- sf::st_bbox(KMP_BOUNDARY)

# CA + OR state outlines for the report-map locator inset.
CONTEXT_STATES <- .kmp_time("read context_states.geojson", tryCatch(
  sf::st_read("data/context_states.geojson", quiet = TRUE),
  error = function(e) NULL
))

# Master metrics + scenario catalog, loaded once.
MASTER_RAW      <- .kmp_time("read kmp_metrics.csv",
  read_input_csv("data/kmp_metrics.csv"))
MASTER_VALID    <- .kmp_time("validate_input",       validate_input(MASTER_RAW))
MASTER_DATA     <- MASTER_VALID$data
MASTER_LEVEL    <- MASTER_VALID$huc_level
MASTER_CLASS    <- .kmp_time("classify_columns",     classify_columns(MASTER_DATA))
MASTER_METRICS  <- MASTER_CLASS$name[MASTER_CLASS$use_in_score]

SCENARIOS       <- .kmp_time("load_scenarios",       load_scenarios("data/scenarios.yaml"))
SCENARIO_CHECK  <- .kmp_time("validate_scenarios",   validate_scenarios(SCENARIOS, MASTER_METRICS))
METRICS_META    <- .kmp_time("load_metrics_meta",    load_metrics_meta("data/metrics.yaml"))

# Sub-zone catalog: several grouping axes, one active at a time. Step 1
# narrows the analysis by one scheme. Selector values are encoded
# "<axis>:<value>" so a single selectInput (with optgroups) drives the
# filter in active_data():
#   full_kmp        whole zone
#   huc6:<code>     HUC6 sub-zone (temporary placeholder units)
#   region:<name>   ecological region, ecoregion-derived
#   forest:<name>   national forest (USFS admin boundary; any-overlap)
# region/forest membership is precomputed by
# scripts/prepare_subzone_groupings.R into data/huc10_groupings.csv.
HUC6_BOUNDARIES <- .kmp_time("read kmp_huc6.geojson",
  sf::st_read("data/kmp_huc6.geojson", quiet = TRUE))
HUC6_BOUNDARIES <- HUC6_BOUNDARIES[order(HUC6_BOUNDARIES$huc6), ]

huc6_choices <- setNames(
  paste0("huc6:", as.character(HUC6_BOUNDARIES$huc6)),
  sprintf("%s (HUC6 %s)", HUC6_BOUNDARIES$name, HUC6_BOUNDARIES$huc6)
)

# Region + forest membership (optional -- app still runs on Full KMP +
# HUC6 if the lookup is absent). SUBZONE_MEMBERS maps an encoded value
# to the set of huccodes it contains.
GROUPINGS <- .kmp_time("read huc10_groupings.csv", tryCatch(
  read.csv("data/huc10_groupings.csv", colClasses = "character"),
  error = function(e) NULL
))

SUBZONE_MEMBERS <- list()
region_choices  <- character(0)
forest_choices  <- character(0)
if (!is.null(GROUPINGS) && nrow(GROUPINGS) > 0) {
  axis_choices <- function(axis) {
    sub  <- GROUPINGS[GROUPINGS$axis == axis, , drop = FALSE]
    vals <- sort(unique(sub$value))
    enc  <- paste0(axis, ":", vals)
    for (k in seq_along(vals)) {
      SUBZONE_MEMBERS[[enc[k]]] <<- unique(sub$huccode[sub$value == vals[k]])
    }
    n <- vapply(vals, function(v) length(unique(sub$huccode[sub$value == v])),
                integer(1))
    setNames(enc, sprintf("%s (%d)", vals, n))
  }
  region_choices <- axis_choices("region")
  forest_choices <- axis_choices("forest")
}

# Grouped choices for the Step-1 selectInput (optgroups), and a flat
# value -> label lookup for step titles + report text.
SUBZONE_GROUPED_CHOICES <- c(
  list("Whole zone" = c("Full KMP" = "full_kmp")),
  if (length(region_choices)) list("By region"          = region_choices),
  if (length(forest_choices)) list("By national forest" = forest_choices),
  list("By HUC6 sub-zone" = huc6_choices)
)
strip_count <- function(x) sub(" \\(\\d+\\)$", "", x)
SUBZONE_LABELS <- c(
  setNames("Full KMP", "full_kmp"),
  setNames(names(huc6_choices),                unname(huc6_choices)),
  setNames(strip_count(names(region_choices)), unname(region_choices)),
  setNames(strip_count(names(forest_choices)), unname(forest_choices))
)

STARTUP_TIMINGS[["TOTAL eager startup"]] <-
  as.numeric(Sys.time() - .KMP_T0, units = "secs")
message(sprintf("[startup] %-32s %6.2fs", "TOTAL eager startup",
                STARTUP_TIMINGS[["TOTAL eager startup"]]))


# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "KMP Watershed Prioritization",
  theme = bs_theme(bootswatch = "flatly"),

  tags$head(
    tags$title("KMP Watershed Prioritization"),
    # Leaflet caches its pixel size and only auto-corrects on a window
    # resize, not when a Bootstrap tab goes hidden -> visible. Returning
    # to a tab fires shown.bs.tab; dispatch a resize so the map (and any
    # other widgets) recompute their size and render at the right place.
    tags$script(HTML(
      "$(document).on('shown.bs.tab', function(){ window.dispatchEvent(new Event('resize')); });"
    )),
    tags$style(HTML("
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

    /* Report tab: document-like styling */
    .report-body { max-width: 820px; margin: 0 auto; line-height: 1.55; }
    .report-body h1 { border-bottom: 2px solid #1f4f8b; padding-bottom: 0.3em;
                      color: #1f4f8b; }
    .report-body h2 { color: #1f4f8b; margin-top: 1.8em;
                      border-bottom: 1px solid #dee2e6; padding-bottom: 0.15em; }
    .report-body h3 { margin-top: 1.3em; color: #374151; }
    .report-body table { border-collapse: collapse; width: 100%; margin: 1em 0;
                         font-size: 0.92rem; }
    .report-body th { background: #f0ece1; text-align: left;
                      padding: 0.45em 0.75em; border-bottom: 2px solid #1f4f8b; }
    .report-body td { padding: 0.35em 0.75em; border-bottom: 1px solid #e5e7eb; }
    .report-body tr:nth-child(even) td { background: #f9fafb; }
    .report-body code { background: #f0f0f0; padding: 0.1em 0.3em;
                        border-radius: 3px; font-size: 0.9em; }
    .report-body em { color: #6c757d; }

    /* DRAFT banner + per-tab DRAFT pill */
    .draft-banner {
      background: #fff3cd;
      border: 1px solid #f0c36d;
      border-left: 4px solid #b45309;
      color: #5a3e0a;
      font-size: 0.78rem;
      line-height: 1.35;
      padding: 0.55rem 0.8rem;
      border-radius: 4px;
      margin-bottom: 0.75rem;
    }
    .draft-banner strong { color: #7a4f08; letter-spacing: 0.5px; }
    .draft-pill {
      display: inline-block;
      background: #b45309;
      color: #fff;
      font-weight: 700;
      letter-spacing: 1.5px;
      font-size: 0.7rem;
      padding: 2px 8px;
      border-radius: 3px;
      margin-right: 8px;
      vertical-align: middle;
    }
    .draft-tab-header {
      display: flex; align-items: center;
      font-size: 0.75rem; color: #5a3e0a;
      padding: 6px 10px; margin: 4px 0 6px 0;
      background: #fff8e1; border-bottom: 1px solid #f0c36d;
    }
  "))),

  sidebar = sidebar(
    width = 380,

    div(class = "draft-banner",
        tags$span(class = "draft-pill", "DRAFT"),
        tags$strong("Tool under development."),
        tags$br(),
        "Metric values, scenarios, and weights are still being refined. ",
        "Rankings are illustrative and should not yet drive real ",
        "prioritization decisions. See the ",
        tags$a(href = "https://github.com/adamkc/kmp-watershed-prioritization/blob/main/docs/data-acquisition.md",
               target = "_blank", "data-acquisition tracker"),
        " for source notes and what's still planned."),

    uiOutput("workflow_ui")
  ),

  navset_card_tab(
    id = "main_tabs",

    nav_panel(
      title = "Map",
      draft_tab_header(),
      leafletOutput("map", height = "620px")
    ),

    nav_panel(
      title = "Ranked HUCs",
      draft_tab_header(),
      div(class = "d-flex justify-content-between align-items-center my-2",
          p(class = "text-muted small mb-0",
            "Click any column header to sort. Per-metric bin scores (1-5) shown right of the composite."),
          downloadButton("download_ranking", "Download CSV",
                         class = "btn-sm btn-outline-primary")),
      DTOutput("ranking_table")
    ),

    nav_panel(
      title = "Sensitivity",
      draft_tab_header(),
      div(class = "p-2",
        p(class = "text-muted small mb-2",
          "Monte Carlo sensitivity: randomly perturb the current weights ",
          "and see which rankings are robust vs. which shift with weight ",
          "variation. Inactive metrics (weight 0) stay at 0 -- perturbation ",
          "only moves weights you've already chosen."),

        fluidRow(
          column(4,
            sliderInput("sens_uncertainty",
              label = tagList(
                "Weight uncertainty (\u00B1 %)",
                help_icon(
                  "Each active weight is multiplied by a random factor between (1 - p) and (1 + p) on every draw. Bigger values shuffle rankings more; 30% is a reasonable exploratory default.")
              ),
              min = 5, max = 100, value = 30, step = 5)
          ),
          column(4,
            sliderInput("sens_draws",
              label = tagList(
                "Number of draws",
                help_icon(
                  "How many Monte Carlo samples to run. More draws give smoother rank distributions (less Monte Carlo noise) but take slightly longer. 1000 is fine for exploration; bump to 5000 for final reports.")
              ),
              min = 100, max = 5000, value = 1000, step = 100)
          ),
          column(4,
            tags$label("\u00A0", class = "form-label d-block"),
            actionButton("run_sensitivity", "Run analysis",
                         class = "btn-primary",
                         icon = icon("rotate"))
          )
        ),

        uiOutput("sensitivity_body")
      )
    ),

    nav_panel(
      title = "Report",
      draft_tab_header(),
      div(class = "p-2",
        div(class = "d-flex justify-content-between align-items-start mb-3",
            p(class = "text-muted small mb-0 me-3",
              "Live summary of the current analysis. Copy the rendered ",
              "text below into a document, or use the download buttons ",
              "for a standalone file. For PDF, use your browser's ",
              tags$strong("Print \u2192 Save as PDF"), " on this tab."),
            div(class = "flex-shrink-0 d-flex gap-2",
                downloadButton("download_report_md", "Download .md",
                               class = "btn-sm btn-outline-primary"),
                downloadButton("download_report_html", "Download .html",
                               class = "btn-sm btn-outline-primary"))),
        div(id = "report_body", class = "report-body",
            uiOutput("report_rendered"))
      )
    ),

    nav_panel(
      title = "Methods",
      div(class = "p-3 report-body",
        shiny::markdown(paste(readLines("data/methods.md", warn = FALSE), collapse = "\n"))
      )
    ),

    nav_panel(
      title = "Acknowledgements",
      div(class = "p-3 report-body",
        tags$h2("Acknowledgements"),
        tags$p(
          "This prioritization tool grew out of work led by ",
          tags$strong("Megan Ireson"), " at the ",
          tags$strong("Scott River Watershed Council"), ", who ",
          "developed the initial concept and prototype in collaboration ",
          "with regional partners. That foundational work was funded by ",
          "the ", tags$strong("California Department of Fish and Wildlife"),
          " (CDFW)."
        ),
        tags$p(
          "The tool was expanded to cover the broader Klamath Meadows ",
          "Partnership zone by the ",
          tags$strong("Klamath Meadows Partnership"),
          ", with funding from the ",
          tags$strong("Wildlife Conservation Board"), " (WCB)."
        ),
        tags$p(
          "The interactive web application was developed by ",
          tags$strong("Adam Cummings"),
          ", with contributions from the entire KMP team."
        ),
        tags$hr(),
        tags$p(class = "text-muted small",
          "Source code and the data-acquisition tracker are on GitHub: ",
          tags$a(href = "https://github.com/adamkc/kmp-watershed-prioritization",
                 target = "_blank",
                 "adamkc/kmp-watershed-prioritization"), "."
        )
      )
    ),

    nav_panel(
      title = "Diagnostics",
      draft_tab_header(),
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
    uploaded       = NULL,           # list(data, huc_level) from upload path
    goal_text      = ""              # Step-3 scenario goal; flows to the report
  )


  # ---- Active dataset (master by default; uploaded replaces it) ------------

  active_data <- reactive({
    # An uploaded CSV brings its own HUCs, which have nothing to do with
    # the KMP sub-zone groupings -- so uploads bypass the Step-1 filter
    # entirely and always show every uploaded HUC.
    if (identical(rv$source_type, "upload") && !is.null(rv$uploaded)) {
      return(rv$uploaded$data)
    }

    src <- MASTER_DATA
    sel <- rv$subzone_id
    if (is.null(sel) || identical(sel, "full_kmp")) return(src)

    # Region / forest membership is precomputed on the KMP HUC10 codes.
    if (grepl("^(region|forest):", sel)) {
      members <- SUBZONE_MEMBERS[[sel]]
      if (is.null(members)) return(src)
      return(src[src$huccode %in% members, , drop = FALSE])
    }

    # HUC6 ("huc6:<code>"): HUC IDs are hierarchical, so a HUC10 within
    # HUC6 "180102" starts with "180102". Match on the code prefix.
    code <- sub("^huc6:", "", sel)
    n <- nchar(code)
    keep <- substr(src$huccode, 1, n) == code
    src[keep, , drop = FALSE]
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
    # Pass the huccodes we need so HUC10 can pick the statewide layer
    # when an upload references HUCs outside the bundled KMP set.
    bnd <- load_boundaries(lvl, need = d$huccode)
    join_input_to_boundaries(d, bnd, lvl)
  })

  active_bin_scores <- reactive({
    compute_bin_scores(active_data(), active_classification())
  })

  # Bin scores after flipping "negative"-direction metrics so that
  # bin 5 always means "pushes this HUC toward the top of the ranking".
  directed_bin_scores <- reactive({
    bs <- active_bin_scores()
    dirs <- vapply(names(bs),
                   function(m) metric_direction(METRICS_META, m),
                   character(1))
    names(dirs) <- names(bs)
    apply_directions(bs, dirs)
  })


  # ---- Workflow UI (renders the 3-step accordion + sliders) ----------------

  output$workflow_ui <- renderUI({
    step1_title <- sprintf("Step 1. Sub-zone \u00B7 %s",
                           SUBZONE_LABELS[[rv$subzone_id]] %||% "Full KMP")
    step2_title <- sprintf("Step 2. Metrics \u00B7 %s", rv$source_label)
    # Do NOT read rv$goal_text reactively here: workflow_ui renders the goal
    # textarea, so a dependency on goal_text forms a render -> input echo ->
    # sync-observer -> render loop that freezes on rapid scenario switching.
    step3_title <- "Step 3. Goal"
    n_active <- length(rv$active_metrics)
    step4_title <- sprintf("Step 4. Weights \u00B7 %d metric%s",
                           n_active, if (n_active == 1) "" else "s")

    # Which panels are open? Default: step 1. After a source is chosen,
    # open the Goal and Weights steps together.
    open_panels <- if (is.null(rv$source_type)) "step1" else c("step3", "step4")

    tagList(
      accordion(
        id = "steps",
        open = open_panels,

        # --- Step 1: sub-zone ---
        accordion_panel(
          value = "step1",
          title = step1_title,
          selectInput("subzone", label = NULL,
                      choices = SUBZONE_GROUPED_CHOICES,
                      selected = rv$subzone_id, width = "100%"),
          p(class = "text-muted small mb-0",
            "Narrow the analysis by one grouping at a time. ",
            tags$strong("Region"), " groups HUCs by EPA Level III ",
            "ecoregion (Coastal/Foothills / Klamath / Cascades-Modoc); ",
            tags$strong("National forest"), " uses USFS administrative ",
            "boundaries (a HUC touching two forests appears under both). ",
            tags$strong("HUC6"), " units are a temporary placeholder ",
            "pending the working group's geological / ecological zones.")
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
                         width = "100%")
          ),

          tags$hr(class = "my-3"),

          fileInput("csv_file",
                    label = "For custom metrics, upload a CSV",
                    accept = c(".csv", "text/csv"),
                    placeholder = "No file selected"),
          div(class = "text-muted",
              style = "font-size: 0.72rem; line-height: 1.35; margin-top: -8px;",
              tags$strong("Required:"), " a column named ", tags$code("huccode"),
              " with 4-, 6-, 8-, 10-, or 12-digit HUC IDs. ",
              tags$strong("HUC10 codes work anywhere in California"),
              " (the map loads statewide HUC10 boundaries on demand); the ",
              "other levels (4/6/8/12) cover the KMP zone only. ",
              tags$strong("Optional:"), " a ", tags$code("name"),
              " column for display labels. All remaining numeric columns are ",
              "treated as metrics and given sliders. HUC codes must be plain ",
              "digit strings \u2014 not scientific notation."
          )
        ),

        # --- Step 3: scenario goal ---
        accordion_panel(
          value = "step3",
          title = step3_title,
          p(class = "text-muted small", style = "line-height: 1.3;",
            "It's important to record the goal of a prioritization scenario. ",
            "Note what you're hoping to prioritize or surface — what a ",
            "top-ranked watershed should represent, and why this set of ",
            "metrics and weights. ",
            tags$em("This text is included in the generated report.")),
          textAreaInput("scenario_goal", label = NULL,
                        value = isolate(rv$goal_text),
                        width = "100%", height = "130px",
                        placeholder = paste(
                          "e.g. Prioritize meadow-rich watersheds with recent",
                          "fire on federal land, to target post-fire meadow",
                          "restoration where the most acreage can be recovered.")),
          if (identical(rv$source_type, "scenario"))
            p(class = "text-muted", style = "font-size: 0.7rem; margin-top: -4px;",
              "Prefilled from the scenario description — edit freely.")
        ),

        # --- Step 4: weights ---
        accordion_panel(
          value = "step4",
          title = step4_title,

          if (length(rv$active_metrics) == 0) {
            p(class = "text-muted small",
              "Pick a scenario or metric list in Step 2 to see sliders here.")
          } else {
            tagList(
              div(class = "d-flex gap-2 mb-2",
                  actionButton("weights_all_1", "All to 1",
                               class = "btn-sm btn-outline-secondary flex-fill"),
                  actionButton("weights_all_0", "All to 0",
                               class = "btn-sm btn-outline-secondary flex-fill")),

              div(class = "small text-muted mb-3",
                  style = "line-height: 1.25;",
                  "Arrows show how each metric's raw value maps to priority: ",
                  tags$span(style = "color: #dc2626; font-weight: 700;", "\u2191"),
                  " high raw value \u2192 high score; ",
                  tags$span(style = "color: #1f4f8b; font-weight: 700;", "\u2193"),
                  " low raw value \u2192 high score."),

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

                dir <- metric_direction(METRICS_META, nm)
                is_neg <- identical(dir, "negative")
                arrow  <- if (is_neg) "\u2193" else "\u2191"
                arrow_color <- if (is_neg) "#1f4f8b" else "#dc2626"
                arrow_title <- if (is_neg)
                  "Low raw values in this metric map to high priority scores."
                  else "High raw values in this metric map to high priority scores."
                dir_badge <- tags$span(
                  title = arrow_title,
                  style = sprintf("color: %s; font-weight: 700; margin-left: 4px;",
                                  arrow_color),
                  arrow
                )

                # Wrap the metric name in a bslib tooltip when a description
                # exists. Dotted underline is a subtle hoverability hint.
                desc <- metric_description(METRICS_META, nm)
                name_el <- if (nzchar(desc)) {
                  bslib::tooltip(
                    tags$span(
                      style = "border-bottom: 1px dotted #6c757d; cursor: help;",
                      nm
                    ),
                    desc,
                    placement = "right"
                  )
                } else {
                  nm
                }

                div(class = "slider-row",
                  div(class = "slider-name",
                      name_el, dir_badge,
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
    # Prefill the Step-3 goal from the scenario description (editable).
    rv$goal_text      <- trimws(scenario$description %||% "")
    updateTextAreaInput(session, "scenario_goal", value = rv$goal_text)
    accordion_panel_close(id = "steps", values = c("step1", "step2"))
    accordion_panel_open(id  = "steps", values = c("step3", "step4"))
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
  # Metrics are grouped by category (from data/metrics.yaml) with direction
  # indicators in each checkbox label. Per-category checkboxGroupInputs
  # keep Shiny's built-in state management; on OK we union the values.
  cat_input_id <- function(cat) paste0("custom_cat_", make.names(cat))

  # Render a metric label: direction arrow + name. If a description is
  # available, wrap in a bslib tooltip with a dotted underline as a
  # hoverability hint. `dimmed` flag is for planned/disabled rows.
  metric_label_tag <- function(m, dimmed = FALSE) {
    d <- metric_direction(METRICS_META, m)
    is_neg <- identical(d, "negative")
    arrow  <- if (is_neg) "\u2193" else "\u2191"
    color  <- if (is_neg) "#1f4f8b" else "#dc2626"
    desc   <- metric_description(METRICS_META, m)
    name_span <- tags$span(
      style = if (nzchar(desc))
        "border-bottom: 1px dotted #6c757d; cursor: help;"
        else NULL,
      m
    )
    label <- tags$span(
      tags$span(arrow,
                style = sprintf("color: %s; font-weight: 700;
                                 display: inline-block; width: 1em;
                                 margin-right: 6px;", color)),
      name_span
    )
    if (dimmed) {
      label <- tags$span(label, style = "opacity: 0.55;")
    }
    if (nzchar(desc)) {
      bslib::tooltip(label, desc, placement = "right")
    } else {
      label
    }
  }

  observeEvent(input$pick_custom, {
    cls <- active_classification()
    real_opts    <- cls$name[cls$use_in_score]
    planned_opts <- setdiff(planned_metric_names(METRICS_META), real_opts)

    # Group together so planned rows show in their declared category.
    all_opts <- c(real_opts, planned_opts)
    grouped  <- group_by_category(all_opts, METRICS_META)

    # Build one category block per non-empty group.
    groups_ui <- lapply(names(grouped), function(cat) {
      members <- grouped[[cat]]
      real_members    <- intersect(members, real_opts)
      planned_members <- intersect(members, planned_opts)

      checkbox_block <- if (length(real_members) > 0) {
        checkboxGroupInput(
          cat_input_id(cat),
          label = NULL,
          choiceNames  = lapply(real_members, metric_label_tag),
          choiceValues = as.list(real_members),
          selected = intersect(rv$active_metrics, real_members)
        )
      } else NULL

      planned_block <- if (length(planned_members) > 0) {
        # Static, non-interactive rows. Disabled native checkbox + dimmed
        # label, with the same tooltip surfaced via metric_label_tag.
        tagList(
          tags$div(
            class = "small text-muted mt-1 mb-1",
            style = "font-style: italic;
                     border-top: 1px dotted #d1d5db;
                     padding-top: 4px;",
            "Planned (data not yet acquired):"
          ),
          lapply(planned_members, function(m) {
            tags$div(
              style = "display: flex; align-items: center;
                       padding: 1px 0; line-height: 1.45;",
              tags$input(type = "checkbox", disabled = "disabled",
                         style = "margin-right: 8px; opacity: 0.5;"),
              metric_label_tag(m, dimmed = TRUE)
            )
          })
        )
      } else NULL

      tagList(
        tags$h6(cat, class = "mt-3 mb-1 text-primary border-bottom pb-1"),
        checkbox_block,
        planned_block
      )
    })

    showModal(modalDialog(
      title = "Select metrics to include",
      easyClose = TRUE, size = "l",
      p(class = "text-muted small mb-1",
        "Check the metrics you want in the composite score. ",
        "Starting weights default to 1 and are adjustable in Step 3. ",
        "Hover any metric name for a description of the underlying ",
        "dataset and method."),
      p(class = "small mb-2",
        tags$span(style = "color: #dc2626; font-weight: 700;", "\u2191"),
        " means high raw value \u2192 high score; ",
        tags$span(style = "color: #1f4f8b; font-weight: 700;", "\u2193"),
        " means low raw value \u2192 high score. Greyed-out rows are ",
        "planned metrics whose source data isn't on disk yet."),
      tagList(groups_ui),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("custom_ok", "Use selected metrics",
                     class = "btn-primary")
      )
    ))
  })

  observeEvent(input$custom_ok, {
    cls <- active_classification()
    metric_opts <- cls$name[cls$use_in_score]
    grouped <- group_by_category(metric_opts, METRICS_META)

    chosen <- unlist(lapply(names(grouped), function(cat) {
      input[[cat_input_id(cat)]]
    }))

    if (is.null(chosen) || length(chosen) == 0) {
      showNotification("Pick at least one metric.", type = "warning")
      return()
    }
    rv$source_type    <- "custom"
    rv$source_label   <- sprintf("Custom list (%d metrics)", length(chosen))
    rv$active_metrics <- chosen
    rv$init_weights   <- setNames(rep(1, length(chosen)), chosen)
    # No prefill for a custom list -- start the goal blank.
    rv$goal_text      <- ""
    updateTextAreaInput(session, "scenario_goal", value = "")
    removeModal()
    accordion_panel_close(id = "steps", values = c("step1", "step2"))
    accordion_panel_open(id  = "steps", values = c("step3", "step4"))
  })

  # --- Upload CSV path ---
  # The file input is always visible in Step 2; rv$source_type flips to
  # "upload" only when the user actually picks a file.
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
    rv$goal_text      <- ""
    updateTextAreaInput(session, "scenario_goal", value = "")
    accordion_panel_close(id = "steps", values = c("step1", "step2"))
    accordion_panel_open(id  = "steps", values = c("step3", "step4"))
  })

  # --- Sub-zone selector ---
  observeEvent(input$subzone, {
    rv$subzone_id <- input$subzone
    # Auto-advance to Step 2 only if the user hasn't progressed past
    # it yet. Changing sub-zone mid-analysis shouldn't yank them back.
    if (is.null(rv$source_type)) {
      accordion_panel_close(id = "steps", values = "step1")
      accordion_panel_open( id = "steps", values = "step2")
    }
  }, ignoreInit = TRUE)


  # --- Scenario goal: keep rv in sync with the textarea ---
  # The workflow accordion re-renders on several rv changes; the textarea
  # value is set from isolate(rv$goal_text) so it doesn't reset mid-edit,
  # and this observer mirrors the user's edits back into rv$goal_text so
  # the report (which reads rv$goal_text) stays current.
  observeEvent(input$scenario_goal, {
    new <- input$scenario_goal %||% ""
    if (!identical(rv$goal_text, new)) rv$goal_text <- new
  }, ignoreInit = TRUE, ignoreNULL = FALSE)


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
    bs <- directed_bin_scores(); w <- weights()
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

  # Keep the map rendered even when the Map tab is hidden. Otherwise Shiny
  # suspends the output and, on return, re-emits only the renderLeaflet
  # base map -- wiping the choropleth, legend, and rank badges that the
  # observe() below adds via leafletProxy (those don't get re-added unless
  # active_joined()/ranking() change, which a tab switch doesn't trigger).
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  observe({
    # HUCs render as grey outlines as soon as a sub-zone is selected
    # (Step 1), then switch to the composite-score choropleth once the
    # user picks a scenario / metric list (Step 2). The all-NA branch
    # below handles the grey state.
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
      unscored_msg <- if (length(rv$active_metrics) == 0)
        "Pick a scenario or metric list to rank."
        else "No weights active."
      sprintf("<strong>%s</strong><br/><em>%s</em>",
              ifelse(is.na(sf_wgs$display_nm), sf_wgs$name, sf_wgs$display_nm),
              unscored_msg)
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
      pal <- colorNumeric("Purples", domain = comp_vals, na.color = "#cccccc")
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
          title = "Composite score<br/><span style='font-weight:400;font-size:11px;color:#555'>darker = higher priority</span>", opacity = 0.85
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
    bs <- directed_bin_scores(); req(bs)
    # Only include bin-score columns for currently active metrics.
    keep_cols <- intersect(rv$active_metrics, names(bs))
    bs_active <- if (length(keep_cols) > 0) bs[, keep_cols, drop = FALSE] else bs[, 0]
    datatable(
      cbind(rk, bs_active),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-size: 0.8rem;
                 color: #6c757d;",
        "Bin scores are direction-adjusted: 5 always means 'pushes the HUC up'."),
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
      bs <- directed_bin_scores()
      keep_cols <- intersect(rv$active_metrics, names(bs))
      bs_active <- if (length(keep_cols) > 0) bs[, keep_cols, drop = FALSE] else bs[, 0]
      write.csv(cbind(ranking(), bs_active), file, row.names = FALSE)
    }
  )


  # ---- Sensitivity tab -----------------------------------------------------

  # Stored results of the last MC run. NULL means "not yet run".
  sens_rv <- reactiveValues(results = NULL, summary = NULL)

  observeEvent(input$run_sensitivity, {
    if (length(rv$active_metrics) == 0) {
      showNotification("Pick a scenario or metric list first.",
                       type = "warning")
      return()
    }
    bs <- directed_bin_scores()
    w  <- weights()
    d  <- active_data()

    # Guard against all-zero weights (nothing to perturb).
    if (!any(w > 0)) {
      showNotification("All weights are 0 -- nothing to perturb.",
                       type = "warning")
      return()
    }

    withProgress(message = "Running Monte Carlo...", value = 0.3, {
      res <- run_sensitivity(
        bin_scores       = bs,
        baseline_weights = w,
        huccodes         = d$huccode,
        uncertainty_pct  = input$sens_uncertainty,
        n_draws          = as.integer(input$sens_draws),
        seed             = 42L
      )
      incProgress(0.5, detail = "Summarizing...")

      baseline_rk <- ranking()$rank
      lname <- label_col_name()
      disp  <- if (!is.na(lname)) d[[lname]] else rep(NA_character_, nrow(d))

      summ <- sensitivity_summary(
        results       = res,
        display_names = disp,
        baseline_rank = baseline_rk,
        top_pct       = 0.10      # fixed: top 10% defines "top" for P(top)
      )
      incProgress(1, detail = "Done")

      sens_rv$results <- res
      sens_rv$summary <- summ
    })
  })

  output$sensitivity_body <- renderUI({
    if (is.null(sens_rv$results)) {
      return(div(class = "text-muted mt-4",
                 "No results yet. Set your parameters and click ",
                 tags$strong("Run analysis"), "."))
    }
    tagList(
      plotOutput("sens_plot", height = "620px"),
      div(class = "d-flex justify-content-between align-items-center mt-3 mb-2",
          h6("Per-HUC rank stability", class = "mb-0"),
          downloadButton("sens_download", "Download CSV",
                         class = "btn-sm btn-outline-primary")),
      DTOutput("sens_table")
    )
  })

  output$sens_plot <- renderPlot({
    req(sens_rv$results, sens_rv$summary)
    plot_rank_distribution(sens_rv$results, sens_rv$summary, limit_top = 30)
  })

  output$sens_table <- renderDT({
    req(sens_rv$summary)
    df <- sens_rv$summary[, c("huccode", "name", "baseline_rank",
                              "rank_median", "rank_mean",
                              "rank_q25", "rank_q75",
                              "rank_min", "rank_max", "p_top")]
    top_pct <- attr(sens_rv$summary, "top_pct")
    top_n   <- attr(sens_rv$summary, "top_n")
    datatable(
      df, rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-size: 0.8rem;
                 color: #6c757d;",
        sprintf("p_top = fraction of draws in which this HUC landed in the top %d%% (top %d of %d HUCs).",
                round(top_pct * 100), top_n, nrow(df))),
      options = list(
        pageLength = 20, scrollX = TRUE,
        order = list(list(3, "asc")),      # sort by rank_median
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      )
    )
  })

  output$sens_download <- downloadHandler(
    filename = function() {
      sprintf("kmp_sensitivity_%s.csv", format(Sys.time(), "%Y%m%d_%H%M"))
    },
    content = function(file) {
      write.csv(sens_rv$summary, file, row.names = FALSE)
    }
  )


  # ---- Report tab ----------------------------------------------------------

  # Lookup current scenario object (if any) for the description.
  current_scenario <- reactive({
    if (!identical(rv$source_type, "scenario")) return(NULL)
    for (s in SCENARIOS) {
      if (identical(s$name, rv$source_label)) return(s)
    }
    NULL
  })

  report_md <- reactive({
    rv_state <- list(
      subzone_id     = rv$subzone_id,
      source_type    = rv$source_type,
      source_label   = rv$source_label,
      active_metrics = rv$active_metrics
    )
    subzone_name <- SUBZONE_LABELS[[rv$subzone_id]] %||% "Full KMP"

    join_info <- if (length(rv$active_metrics) > 0) active_joined() else
      list(n_matched = nrow(active_data()), unmatched_ids = character())

    rk <- if (length(rv$active_metrics) > 0) ranking() else NULL

    build_report_md(
      rv_state       = rv_state,
      subzone_name   = subzone_name,
      scenario       = current_scenario(),
      goal           = rv$goal_text,
      ranking_df     = rk,
      weights        = weights(),
      metrics_meta   = METRICS_META,
      classification = active_classification(),
      join_info      = join_info,
      sens_results   = sens_rv$results,
      sens_summary   = sens_rv$summary
    )
  })

  # Chart reactives (reused by inline view + HTML download).
  report_chart_map <- reactive({
    if (length(rv$active_metrics) == 0) return(NULL)
    j <- active_joined()
    if (is.null(j) || is.null(j$sf) || nrow(j$sf) == 0) return(NULL)
    rk <- ranking()
    make_prioritization_map(
      joined_sf      = j$sf,
      kmp_boundary   = KMP_BOUNDARY,
      ranking_df     = rk,
      huc_level      = active_huc_level(),
      top_n          = 3,
      context_states = CONTEXT_STATES
    )
  })

  report_chart_ranking <- reactive({
    if (length(rv$active_metrics) == 0) return(NULL)
    rk <- ranking()
    make_ranking_chart(rk, n = 15)
  })

  report_chart_facets <- reactive({
    if (length(rv$active_metrics) == 0) return(NULL)
    j <- active_joined()
    if (is.null(j) || is.null(j$sf) || nrow(j$sf) == 0) return(NULL)
    d <- active_data()
    make_faceted_metric_map(
      joined_sf      = j$sf,
      bin_scores     = directed_bin_scores(),
      huccodes       = d$huccode,
      active_metrics = rv$active_metrics,
      huc_level      = active_huc_level()
    )
  })

  report_chart_sensitivity <- reactive({
    if (is.null(sens_rv$results) || is.null(sens_rv$summary)) return(NULL)
    plot_rank_distribution(sens_rv$results, sens_rv$summary, limit_top = 30)
  })

  output$report_chart_map_out <- renderPlot({
    p <- report_chart_map(); req(p); p
  })
  output$report_chart_ranking_out <- renderPlot({
    p <- report_chart_ranking(); req(p); p
  })
  output$report_chart_facets_out <- renderPlot({
    p <- report_chart_facets(); req(p); p
  })
  output$report_chart_sensitivity_out <- renderPlot({
    p <- report_chart_sensitivity(); req(p); p
  })

  # Inline view: split the markdown at each chart placeholder (in order:
  # MAP, RANKING, FACETS, SENSITIVITY) and interleave rendered markdown
  # chunks with plotOutput. Each chunk is self-contained because
  # placeholders sit between H2 sections.
  output$report_rendered <- renderUI({
    md <- report_md()

    split_at <- function(s, token) {
      parts <- strsplit(s, token, fixed = TRUE)[[1]]
      list(before = parts[1], after = if (length(parts) >= 2) parts[2] else "")
    }

    a <- split_at(md,       PLACEHOLDER_CHART_MAP)
    b <- split_at(a$after,  PLACEHOLDER_CHART_RANKING)
    c <- split_at(b$after,  PLACEHOLDER_CHART_FACETS)
    d <- split_at(c$after,  PLACEHOLDER_CHART_SENSITIVITY)

    map_ui <- if (!is.null(report_chart_map()))
      plotOutput("report_chart_map_out", height = "540px") else NULL
    rank_ui <- if (!is.null(report_chart_ranking()))
      plotOutput("report_chart_ranking_out", height = "420px") else NULL

    # Facet map height scales with metric count (~160 px per row, 3 cols).
    n_active <- length(rv$active_metrics)
    n_facet_cols <- if (n_active <= 4) 2 else if (n_active <= 9) 3 else 4
    n_facet_rows <- ceiling(n_active / n_facet_cols)
    facets_h <- max(400, n_facet_rows * 220)
    facets_ui <- if (!is.null(report_chart_facets()))
      plotOutput("report_chart_facets_out",
                 height = paste0(facets_h, "px")) else NULL

    sens_ui <- if (!is.null(report_chart_sensitivity()))
      plotOutput("report_chart_sensitivity_out", height = "560px") else NULL

    tagList(
      shiny::markdown(a$before),
      map_ui,
      shiny::markdown(b$before),
      rank_ui,
      shiny::markdown(c$before),
      facets_ui,
      shiny::markdown(d$before),
      sens_ui,
      shiny::markdown(d$after)
    )
  })

  output$download_report_md <- downloadHandler(
    filename = function() {
      sprintf("kmp_report_%s.md", format(Sys.time(), "%Y%m%d_%H%M"))
    },
    content = function(file) {
      # For MD: replace chart placeholders with a short "see HTML" note.
      md <- fill_report_chart_placeholders(report_md(), mode = "text")
      writeLines(md, file, useBytes = TRUE)
    }
  )

  output$download_report_html <- downloadHandler(
    filename = function() {
      sprintf("kmp_report_%s.html", format(Sys.time(), "%Y%m%d_%H%M"))
    },
    content = function(file) {
      # For HTML: substitute placeholders with inline base64 images. Guard
      # every step so a chart-build or rasterize failure (which can happen
      # under webR / shinylive) degrades gracefully instead of failing the
      # whole download with no file at all. Building each chart is wrapped
      # so one bad chart can't sink the others, and the overall assembly
      # falls back to a text-only report as a last resort.
      safe <- function(expr) tryCatch(expr, error = function(e) NULL)
      html <- tryCatch({
        md <- fill_report_chart_placeholders(
          report_md(),
          map_plot         = safe(report_chart_map()),
          ranking_plot     = safe(report_chart_ranking()),
          facets_plot      = safe(report_chart_facets()),
          sensitivity_plot = safe(report_chart_sensitivity()),
          mode             = "html"
        )
        render_standalone_html(as.character(shiny::markdown(md)))
      }, error = function(e) {
        md <- fill_report_chart_placeholders(report_md(), mode = "text")
        render_standalone_html(as.character(shiny::markdown(md)))
      })
      writeLines(html, file, useBytes = TRUE)
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

    # Startup timing breakdown — captured by .kmp_time() during eager
    # boot. Useful for prioritising perf work (which step dominates
    # cold start? boundary read? csv parse? package attach?).
    timing_block <- if (length(STARTUP_TIMINGS) > 0) {
      total <- STARTUP_TIMINGS[["TOTAL eager startup"]]
      rows <- lapply(names(STARTUP_TIMINGS), function(nm) {
        dt <- STARTUP_TIMINGS[[nm]]
        pct <- if (!is.null(total) && total > 0) sprintf("%4.0f%%",
                                                         100 * dt / total) else ""
        tags$tr(
          tags$td(nm),
          tags$td(sprintf("%.2fs", dt), style = "text-align: right;"),
          tags$td(pct, style = "text-align: right; color: #888;")
        )
      })
      tagList(
        tags$h5("Startup timing", class = "mt-3"),
        tags$table(
          class = "table table-sm",
          style = "max-width: 480px; font-size: 0.85rem;",
          tags$tbody(rows)
        )
      )
    } else NULL

    tagList(
      scenario_block,
      timing_block,

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
