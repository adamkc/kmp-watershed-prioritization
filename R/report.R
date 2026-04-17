# Build a markdown summary of the current analysis state. The same
# markdown powers the inline Report tab (rendered to HTML) and the
# downloadable .md / .html exports.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Placeholder strings inserted by build_report_md() where charts go.
# Each renderer (inline / .md / .html) replaces these with its own
# version of the chart (or nothing). Order in the report: MAP first
# (as the spatial headline), then RANKING (bar chart), then
# SENSITIVITY (rank distribution). Keep this order in sync with
# placement inside build_report_md().
PLACEHOLDER_CHART_MAP         <- "__CHART_MAP__"
PLACEHOLDER_CHART_RANKING     <- "__CHART_RANKING__"
PLACEHOLDER_CHART_FACETS      <- "__CHART_FACETS__"
PLACEHOLDER_CHART_SENSITIVITY <- "__CHART_SENSITIVITY__"


#' Static choropleth map of composite scores by HUC, with KMP zone
#' outline, top-N rank badges, and an optional locator inset showing
#' the KMP zone's position relative to a regional context (e.g. CA+OR
#' state outlines).
#'
#' Returns a ggplot (patchworked if inset is provided), or NULL if
#' there is nothing to map.
make_prioritization_map <- function(joined_sf,
                                    kmp_boundary,
                                    ranking_df,
                                    huc_level,
                                    top_n         = 3,
                                    context_states = NULL) {
  if (is.null(joined_sf) || is.null(ranking_df)) return(NULL)
  if (nrow(joined_sf) == 0 || nrow(ranking_df) == 0) return(NULL)

  huc_col <- paste0("huc", huc_level)
  joined_sf <- sf::st_transform(joined_sf, 4326)
  idx <- match(joined_sf[[huc_col]], ranking_df$huccode)
  joined_sf$composite <- ranking_df$score[idx]
  joined_sf$rank_pos  <- ranking_df$rank[idx]

  if (all(is.na(joined_sf$composite))) return(NULL)

  top_mask <- !is.na(joined_sf$rank_pos) & joined_sf$rank_pos <= top_n
  top_sf   <- joined_sf[top_mask, , drop = FALSE]
  top_pts  <- if (nrow(top_sf) > 0) {
    suppressWarnings(sf::st_point_on_surface(top_sf))
  } else NULL

  bbox <- sf::st_bbox(joined_sf)

  main <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = kmp_boundary,
                     fill = "#f3f4f6", color = "#1f4f8b",
                     linewidth = 0.55, alpha = 0.45) +
    ggplot2::geom_sf(data = joined_sf,
                     ggplot2::aes(fill = composite),
                     color = "#333", linewidth = 0.18) +
    ggplot2::scale_fill_distiller(
      palette   = "YlOrRd",
      direction = 1,
      name      = "Composite\nscore",
      na.value  = "#cccccc"
    )

  if (!is.null(top_pts) && nrow(top_pts) > 0) {
    main <- main + ggplot2::geom_sf_label(
      data = top_pts,
      ggplot2::aes(label = sprintf("#%d", rank_pos)),
      size          = 3,
      fontface      = "bold",
      color         = "#111",
      fill          = "white",
      alpha         = 0.95,
      label.r       = ggplot2::unit(0.25, "lines"),
      label.padding = ggplot2::unit(0.22, "lines")
    )
  }

  main <- main +
    ggplot2::coord_sf(
      xlim   = c(bbox[["xmin"]], bbox[["xmax"]]),
      ylim   = c(bbox[["ymin"]], bbox[["ymax"]]),
      expand = TRUE
    ) +
    ggplot2::labs(title = "Composite score by HUC (static snapshot)") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 12, face = "bold",
                                               margin = ggplot2::margin(b = 6)),
      legend.position  = "right",
      panel.background = ggplot2::element_rect(fill = "#fafafa", color = NA)
    )

  # Locator inset: CA+OR outline with the KMP zone highlighted. Shows
  # regional context for readers unfamiliar with Northern California.
  if (!is.null(context_states)) {
    ctx_bbox <- sf::st_bbox(context_states)
    inset <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = context_states,
                       fill = "white", color = "#555", linewidth = 0.25) +
      ggplot2::geom_sf(data = kmp_boundary,
                       fill = "#1f4f8b", color = "#1f4f8b",
                       linewidth = 0.3, alpha = 0.7) +
      ggplot2::coord_sf(
        xlim   = c(ctx_bbox[["xmin"]], ctx_bbox[["xmax"]]),
        ylim   = c(ctx_bbox[["ymin"]], ctx_bbox[["ymax"]]),
        expand = FALSE
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#f8f8f8",
                                                 color = "#999"),
        plot.margin      = ggplot2::margin(0, 0, 0, 0)
      )

    # Bottom-left corner keeps clear of the legend on the right.
    main <- main +
      patchwork::inset_element(
        inset,
        left = 0.01, bottom = 0.01, right = 0.26, top = 0.32,
        align_to = "panel", on_top = TRUE
      )
  }

  main
}


#' Small-multiples choropleth: one panel per active metric, colored by
#' that metric's bin score (1-5 after direction adjustment so "5" always
#' means "pushes this HUC up the ranking"). Shared color scale across
#' panels so panels can be visually compared.
#'
#' @param joined_sf     HUC polygons joined to input data
#' @param bin_scores    directed_bin_scores data frame (from apply_directions)
#' @param huccodes      character vector of HUC IDs aligned to bin_scores rows
#' @param active_metrics metric names currently in use
#' @param huc_level     HUC level integer
make_faceted_metric_map <- function(joined_sf,
                                    bin_scores,
                                    huccodes,
                                    active_metrics,
                                    huc_level) {
  active_cols <- intersect(active_metrics, names(bin_scores))
  if (length(active_cols) == 0) return(NULL)
  if (is.null(joined_sf) || nrow(joined_sf) == 0) return(NULL)

  huc_col <- paste0("huc", huc_level)
  joined_sf <- sf::st_transform(joined_sf, 4326)

  # Build long-form sf: for each active metric, duplicate geometry rows
  # with a 'metric' column and the corresponding bin score.
  per_metric <- lapply(active_cols, function(m) {
    vals <- bin_scores[[m]][match(joined_sf[[huc_col]], huccodes)]
    df   <- joined_sf[, huc_col, drop = FALSE]
    df$metric <- factor(m, levels = active_cols)
    df$bin    <- vals
    df
  })
  sf_long <- do.call(rbind, per_metric)

  # Facet label: strip "Simulated: " prefix and wrap long names.
  label_fun <- function(x) {
    cleaned <- gsub("^Simulated:\\s*", "", x)
    vapply(cleaned, function(s) paste(strwrap(s, width = 28), collapse = "\n"),
           character(1))
  }

  n_cols <- if (length(active_cols) <= 4)      2
            else if (length(active_cols) <= 9) 3
            else                                4

  ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(ggplot2::aes(fill = bin),
                     color = "#666", linewidth = 0.08) +
    ggplot2::scale_fill_distiller(
      palette   = "YlOrRd",
      direction = 1,
      limits    = c(1, 5),
      breaks    = 1:5,
      name      = "Bin",
      na.value  = "#cccccc"
    ) +
    ggplot2::facet_wrap(~ metric, ncol = n_cols,
                        labeller = ggplot2::as_labeller(label_fun)) +
    ggplot2::coord_sf() +
    ggplot2::labs(
      title    = "Per-metric bin scores across HUCs",
      subtitle = "Bin 5 (darkest) = highest priority contribution from that metric"
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title     = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle  = ggplot2::element_text(size = 9, color = "#6c757d",
                                             margin = ggplot2::margin(b = 8)),
      strip.text     = ggplot2::element_text(size = 9, face = "bold",
                                             margin = ggplot2::margin(t = 2, b = 2)),
      strip.background = ggplot2::element_rect(fill = "#f0ece1", color = NA),
      legend.position = "right",
      panel.spacing   = ggplot2::unit(6, "pt")
    )
}


#' Horizontal bar chart of the top-N HUCs by composite score.
#'
#' Returns a ggplot object, or NULL if there's nothing to plot.
make_ranking_chart <- function(ranking_df, n = 15) {
  if (is.null(ranking_df) || nrow(ranking_df) == 0) return(NULL)
  df <- ranking_df[!is.na(ranking_df$score), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df <- df[order(df$rank), ]
  df <- df[seq_len(min(n, nrow(df))), , drop = FALSE]

  # Use the HUC name if present; fall back to huccode. Disambiguate any
  # duplicate names (e.g. multiple "Bear Creek") with the huccode.
  labels <- ifelse(is.na(df$name) | !nzchar(df$name), df$huccode, df$name)
  dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  if (any(dup)) labels[dup] <- paste0(labels[dup], " (", df$huccode[dup], ")")
  df$label <- factor(labels, levels = rev(labels))  # rank 1 at top of plot

  ggplot2::ggplot(df, ggplot2::aes(x = score, y = label)) +
    ggplot2::geom_col(fill = "#1f4f8b", alpha = 0.88) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", score)),
                       hjust = -0.15, size = 3.2, color = "#212529") +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      x = "Composite score (1 = lowest priority, 5 = highest)",
      y = NULL,
      title = sprintf("Top %d HUCs by composite score", nrow(df))
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(size = 12, face = "bold")
    )
}


#' Render a ggplot to a base64-encoded PNG data URI suitable for
#' embedding in an HTML document as <img src="...">.
plot_to_data_uri <- function(plot, width = 7, height = 4.5, dpi = 110) {
  if (is.null(plot)) return("")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ggplot2::ggsave(tmp, plot, width = width, height = height,
                  dpi = dpi, bg = "white")
  b64 <- base64enc::base64encode(tmp)
  sprintf(
    '\n\n<p><img src="data:image/png;base64,%s" alt="Chart" style="max-width: 100%%; height: auto; display: block; margin: 1em auto;"></p>\n\n',
    b64
  )
}


#' Substitute chart placeholders in a report markdown string.
#'
#' @param md                Markdown from build_report_md().
#' @param map_plot          ggplot for the static priority map, or NULL.
#' @param ranking_plot      ggplot for the top-ranked bar chart, or NULL.
#' @param sensitivity_plot  ggplot for the rank-distribution boxplot, or NULL.
#' @param mode              "html" for inline base64 images, "text" for a
#'                          "see HTML report" note suitable for the .md
#'                          download.
#'
#' @return md with placeholders replaced.
fill_report_chart_placeholders <- function(md,
                                           map_plot         = NULL,
                                           ranking_plot     = NULL,
                                           facets_plot      = NULL,
                                           sensitivity_plot = NULL,
                                           mode             = c("html", "text")) {
  mode <- match.arg(mode)

  # Facet height scales with the number of panels. Estimate by reading
  # off the ggplot layout if possible; fall back to a default.
  facets_h <- 8
  if (!is.null(facets_plot)) {
    n_panels <- tryCatch(
      length(unique(facets_plot$data$metric)), error = function(e) NA
    )
    if (!is.na(n_panels) && n_panels > 0) {
      # assume 3 cols; ~3 inches per row, minimum 6
      facets_h <- max(6, ceiling(n_panels / 3) * 3)
    }
  }

  if (mode == "html") {
    mp_repl <- plot_to_data_uri(map_plot,         width = 8.0, height = 6.5)
    rk_repl <- plot_to_data_uri(ranking_plot,     width = 7.5, height = 4.5)
    ft_repl <- plot_to_data_uri(facets_plot,      width = 8.5, height = facets_h)
    sn_repl <- plot_to_data_uri(sensitivity_plot, width = 8.5, height = 7.0)
  } else {
    mp_repl <- "\n\n_Map: composite score by HUC -- see the HTML report or inline view._\n\n"
    rk_repl <- "\n\n_Chart: top-ranked HUCs -- see the HTML report or inline view._\n\n"
    ft_repl <- "\n\n_Chart: per-metric bin scores -- see the HTML report or inline view._\n\n"
    sn_repl <- "\n\n_Chart: rank distribution -- see the HTML report or inline view._\n\n"
  }
  md <- gsub(PLACEHOLDER_CHART_MAP,         mp_repl, md, fixed = TRUE)
  md <- gsub(PLACEHOLDER_CHART_RANKING,     rk_repl, md, fixed = TRUE)
  md <- gsub(PLACEHOLDER_CHART_FACETS,      ft_repl, md, fixed = TRUE)
  md <- gsub(PLACEHOLDER_CHART_SENSITIVITY, sn_repl, md, fixed = TRUE)
  md
}


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


  # --- Spatial overview (static map) ---
  # Inserted whenever there's an active analysis; placeholder resolves
  # to an inline chart or a short "see HTML report" note per renderer.
  add("## Spatial overview", "",
      PLACEHOLDER_CHART_MAP, "")


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

    # Placeholder for the top-N bar chart (filled in by the renderer).
    add(PLACEHOLDER_CHART_RANKING, "")
  }


  # --- Metric breakdowns (per-metric small multiples) -----------------------
  if (length(rv_state$active_metrics) > 0) {
    add("## Metric breakdowns", "",
        "_One panel per active metric, colored by that metric's ",
        "direction-adjusted bin score (1 = low priority contribution, ",
        "5 = high). Look for HUCs that score high on some metrics but ",
        "not others -- those are the ones whose ranking shifts most ",
        "as weights change._", "",
        PLACEHOLDER_CHART_FACETS, "")
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

    # Placeholder for the rank-distribution boxplot.
    add(PLACEHOLDER_CHART_SENSITIVITY, "")
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
