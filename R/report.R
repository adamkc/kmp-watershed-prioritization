# Build a markdown summary of the current analysis state. The same
# markdown powers the inline Report tab (rendered to HTML) and the
# downloadable .md / .html exports.

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Assemble a markdown report of the current app state.
#'
#' @param rv_state       named list with subzone_id, source_type,
#'                       source_label, active_metrics, init_weights
#' @param subzone_name   human-readable sub-zone name
#' @param scenario       list (from get_scenario) or NULL
#' @param ranking_df     data frame with rank, huccode, name, score
#' @param weights        named numeric vector of current weights
#' @param metrics_meta   list from load_metrics_meta()
#' @param classification classify_columns() output for active data
#' @param join_info      list from join_input_to_boundaries()
#' @param sens_results   run_sensitivity() output, or NULL
#' @param sens_summary   sensitivity_summary() output, or NULL
#' @param sens_params    list(uncertainty_pct, n_draws, top_pct)
build_report_md <- function(rv_state,
                            subzone_name,
                            scenario       = NULL,
                            ranking_df,
                            weights,
                            metrics_meta,
                            classification,
                            join_info,
                            sens_results = NULL,
                            sens_summary = NULL,
                            sens_params  = NULL) {
  L <- character()
  add <- function(...) L <<- c(L, ...)

  # --- Header ---
  add(
    "# KMP Watershed Prioritization Report",
    "",
    sprintf("_Generated %s_", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    ""
  )


  # --- Configuration ---
  add("## Configuration", "")

  src_descr <- switch(
    rv_state$source_type %||% "none",
    "scenario" = "Prebuilt scenario",
    "custom"   = "Custom metric list",
    "upload"   = "User-uploaded CSV",
    "not yet chosen"
  )

  add(
    sprintf("- **Sub-zone:** %s", subzone_name),
    sprintf("- **Metrics source:** %s (%s)", src_descr, rv_state$source_label),
    sprintf("- **Active metrics:** %d", length(rv_state$active_metrics)),
    sprintf("- **HUCs in analysis:** %d", join_info$n_matched %||% NA_integer_),
    ""
  )

  if (!is.null(scenario)) {
    add("### Scenario description",
        "",
        trimws(scenario$description),
        "")
  }


  # --- Metrics & weights table ---
  if (length(rv_state$active_metrics) > 0) {
    add("### Metrics and weights", "")
    add("| Metric | Category | Direction | Weight |")
    add("|---|---|---|---:|")
    for (m in rv_state$active_metrics) {
      cat <- metric_category(metrics_meta, m)
      dir <- metric_direction(metrics_meta, m)
      arrow <- if (identical(dir, "negative")) "down (low = priority)"
               else "up (high = priority)"
      w <- weights[[m]]
      w_str <- if (is.null(w) || is.na(w)) "-" else formatC(w, digits = 2, format = "f")
      add(sprintf("| %s | %s | %s | %s |", m, cat, arrow, w_str))
    }
    add("")
  }


  # --- HUC ranking ---
  add("## Top 10 HUCs by composite score", "")
  if (is.null(ranking_df) || nrow(ranking_df) == 0) {
    add("_No rankings yet -- pick metrics and adjust weights to generate._", "")
  } else {
    add("| Rank | HUC code | Name | Score |")
    add("|---:|:---|:---|---:|")
    top10 <- ranking_df[order(ranking_df$rank), ][seq_len(min(10, nrow(ranking_df))), ,
                                                  drop = FALSE]
    for (i in seq_len(nrow(top10))) {
      nm <- top10$name[i]
      if (is.na(nm) || !nzchar(nm)) nm <- "-"
      sc <- top10$score[i]
      sc_str <- if (is.na(sc)) "-" else formatC(sc, digits = 2, format = "f")
      add(sprintf("| %d | %s | %s | %s |",
                  top10$rank[i], top10$huccode[i], nm, sc_str))
    }
    add("")

    # score distribution
    sc <- ranking_df$score[!is.na(ranking_df$score)]
    if (length(sc) > 0) {
      add(sprintf("_Score range: %.2f (min) to %.2f (max); median %.2f; IQR %.2f to %.2f._",
                  min(sc), max(sc), median(sc),
                  quantile(sc, 0.25), quantile(sc, 0.75)),
          "")
    }
  }


  # --- Sensitivity ---
  add("## Sensitivity analysis", "")
  if (is.null(sens_results) || is.null(sens_summary)) {
    add("_Not run. Open the Sensitivity tab and click Run analysis to include this section._", "")
  } else {
    top_n    <- attr(sens_summary, "top_n")
    top_pct  <- attr(sens_summary, "top_pct")

    n_stable <- sum(sens_summary$p_top >= 0.8, na.rm = TRUE)
    n_swing  <- sum(sens_summary$p_top > 0 & sens_summary$p_top < 0.2, na.rm = TRUE)

    add(
      sprintf("- **Draws:** %d", sens_results$n_draws),
      sprintf("- **Weight uncertainty:** \u00B1%d%%", sens_results$uncertainty_pct),
      sprintf("- **\"Top\" definition:** top %d%% (top %d of %d HUCs)",
              round(top_pct * 100), top_n, nrow(sens_summary)),
      sprintf("- **Robustly top HUCs (P(top) >= 0.8):** %d", n_stable),
      sprintf("- **Borderline HUCs (0 < P(top) < 0.2):** %d", n_swing),
      ""
    )

    # Top 10 stability table
    add("### Rank stability -- top 10 by median rank", "")
    add("| Median | Baseline | HUC code | Name | IQR | P(top) |")
    add("|---:|---:|:---|:---|:---|---:|")
    top_stab <- sens_summary[seq_len(min(10, nrow(sens_summary))), , drop = FALSE]
    for (i in seq_len(nrow(top_stab))) {
      nm <- top_stab$name[i]
      if (is.na(nm) || !nzchar(nm)) nm <- "-"
      add(sprintf("| %.0f | %d | %s | %s | %.0f-%.0f | %.2f |",
                  top_stab$rank_median[i],
                  top_stab$baseline_rank[i],
                  top_stab$huccode[i],
                  nm,
                  top_stab$rank_q25[i], top_stab$rank_q75[i],
                  top_stab$p_top[i]))
    }
    add("")
  }


  # --- Caveats ---
  caveats <- character()

  # Simulated-data flag: any active metric starts with "Simulated:"
  if (any(grepl("^Simulated:", rv_state$active_metrics))) {
    caveats <- c(caveats,
      "The active metric list includes **simulated** values (prefixed `Simulated:`). These are randomly generated demo data and should not be used for real prioritization decisions.")
  }

  # Zero-inflated columns in use
  zi_active <- classification$name[classification$zero_inflated &
                                   classification$use_in_score &
                                   classification$name %in% rv_state$active_metrics]
  if (length(zi_active) > 0) {
    caveats <- c(caveats, sprintf(
      "Zero-inflated metric(s) in the active set: %s. Jenks may compress these into fewer effective classes.",
      paste0("`", zi_active, "`", collapse = ", ")
    ))
  }

  # Unmatched HUCs
  if (!is.null(join_info$unmatched_ids) && length(join_info$unmatched_ids) > 0) {
    caveats <- c(caveats, sprintf(
      "%d input HUC(s) did not match the bundled KMP boundaries and were excluded from analysis.",
      length(join_info$unmatched_ids)
    ))
  }

  if (length(caveats) > 0) {
    add("## Caveats", "")
    for (c in caveats) add(paste("-", c))
    add("")
  }

  paste(L, collapse = "\n")
}


#' Wrap a rendered HTML body in a full standalone HTML document.
render_standalone_html <- function(html_body, title = "KMP Prioritization Report") {
  sprintf(
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>%s</title>
<style>
  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
         max-width: 820px; margin: 2em auto; padding: 0 1em; line-height: 1.55;
         color: #212529; }
  h1 { border-bottom: 2px solid #1f4f8b; padding-bottom: 0.3em; color: #1f4f8b; }
  h2 { color: #1f4f8b; margin-top: 1.8em; border-bottom: 1px solid #dee2e6;
       padding-bottom: 0.15em; }
  h3 { margin-top: 1.3em; color: #374151; }
  table { border-collapse: collapse; width: 100%%; margin: 1em 0; font-size: 0.95rem; }
  th { background: #f0ece1; text-align: left; padding: 0.5em 0.8em;
       border-bottom: 2px solid #1f4f8b; }
  td { padding: 0.4em 0.8em; border-bottom: 1px solid #e5e7eb; }
  tr:nth-child(even) td { background: #f9fafb; }
  code { background: #f0f0f0; padding: 0.1em 0.3em; border-radius: 3px;
         font-size: 0.9em; }
  em { color: #6c757d; }
  @media print {
    body { max-width: none; margin: 0.5in; }
    h2 { page-break-before: auto; page-break-after: avoid; }
  }
</style>
</head>
<body>%s</body>
</html>', title, html_body)
}
