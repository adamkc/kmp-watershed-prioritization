# Monte Carlo weight sensitivity: perturb each active weight
# independently and recompute composite / rank many times. Output is
# a per-HUC rank distribution that reveals which rankings are robust
# vs. which shift meaningfully with weight variation.


#' Run a Monte Carlo sensitivity analysis over weight perturbations.
#'
#' Each draw multiplies every active weight (weight > 0) by a
#' Uniform(1 - p, 1 + p) factor, clamped at 0. Inactive metrics stay
#' at 0 -- weights the user explicitly zeroed shouldn't spring back
#' to life from a random perturbation.
#'
#' @param bin_scores        Directed bin scores (data frame from
#'                          apply_directions(); bin 5 = "high priority").
#' @param baseline_weights  Named numeric vector keyed by metric column.
#' @param huccodes          Character vector of HUC IDs aligned to
#'                          bin_scores rows; used as column names on the
#'                          ranks matrix so downstream code can index
#'                          by HUC ID rather than row order.
#' @param uncertainty_pct   Percent perturbation per weight (30 = +/- 30%).
#' @param n_draws           Monte Carlo draws.
#' @param seed              RNG seed.
#' @return list with
#'   $n_draws, $uncertainty_pct, $baseline_weights
#'   $ranks      n_draws x n_hucs integer matrix, columns = huccodes
#'   $composites n_draws x n_hucs numeric matrix, columns = huccodes
run_sensitivity <- function(bin_scores,
                            baseline_weights,
                            huccodes,
                            uncertainty_pct = 30,
                            n_draws         = 1000L,
                            seed            = 42L) {
  stopifnot(is.data.frame(bin_scores),
            is.numeric(baseline_weights),
            length(huccodes) == nrow(bin_scores))

  n_hucs  <- nrow(bin_scores)
  w_names <- names(baseline_weights)
  n_w     <- length(baseline_weights)

  p       <- uncertainty_pct / 100
  active  <- baseline_weights > 0

  set.seed(seed)
  ranks_mat <- matrix(NA_integer_, nrow = n_draws, ncol = n_hucs,
                      dimnames = list(NULL, huccodes))
  comp_mat  <- matrix(NA_real_,    nrow = n_draws, ncol = n_hucs,
                      dimnames = list(NULL, huccodes))

  for (i in seq_len(n_draws)) {
    mult <- runif(n_w, 1 - p, 1 + p)
    mult[!active] <- 0
    w_draw <- pmax(0, baseline_weights * mult)
    names(w_draw) <- w_names

    comp <- composite_score(bin_scores, w_draw)
    comp_mat[i, ]  <- comp
    ranks_mat[i, ] <- rank_hucs(comp)
  }

  list(
    n_draws          = n_draws,
    uncertainty_pct  = uncertainty_pct,
    baseline_weights = baseline_weights,
    ranks            = ranks_mat,
    composites       = comp_mat,
    n_hucs           = n_hucs,
    timestamp        = Sys.time()
  )
}


#' Per-HUC rank statistics from a sensitivity run.
#'
#' Output is sorted by median rank (ascending). Columns include
#' baseline_rank, rank_median, rank_mean, rank_q25, rank_q75,
#' rank_min, rank_max, and p_top (fraction of draws in which the
#' HUC landed in the top `top_pct` fraction).
sensitivity_summary <- function(results,
                                display_names,
                                baseline_rank,
                                top_pct = 0.10) {
  ranks  <- results$ranks
  huccodes <- colnames(ranks)
  n_hucs <- ncol(ranks)
  top_n  <- max(1L, floor(n_hucs * top_pct))

  df <- data.frame(
    huccode       = huccodes,
    name          = display_names,
    baseline_rank = baseline_rank,
    rank_median   = apply(ranks, 2, median,   na.rm = TRUE),
    rank_mean     = round(colMeans(ranks, na.rm = TRUE), 2),
    rank_q25      = apply(ranks, 2, quantile, 0.25, na.rm = TRUE),
    rank_q75      = apply(ranks, 2, quantile, 0.75, na.rm = TRUE),
    rank_min      = apply(ranks, 2, min,      na.rm = TRUE),
    rank_max      = apply(ranks, 2, max,      na.rm = TRUE),
    p_top         = round(colMeans(ranks <= top_n, na.rm = TRUE), 3),
    stringsAsFactors = FALSE
  )
  attr(df, "top_n")   <- top_n
  attr(df, "top_pct") <- top_pct
  df[order(df$rank_median), , drop = FALSE]
}


#' Rank-distribution plot: horizontal boxplot of ranks per HUC,
#' sorted best-to-worst by median rank.
plot_rank_distribution <- function(results, summary_df, limit_top = 30) {
  n_show    <- min(limit_top, nrow(summary_df))
  show_rows <- summary_df[seq_len(n_show), , drop = FALSE]

  # Pull matrix columns by HUC ID -- column names on the ranks matrix
  # make the lookup robust to any reordering of summary_df.
  col_idx  <- match(show_rows$huccode, colnames(results$ranks))
  ranks_m  <- results$ranks[, col_idx, drop = FALSE]

  # Display labels prefer human-readable name; fall back to huccode.
  labels <- ifelse(is.na(show_rows$name) | !nzchar(show_rows$name),
                   show_rows$huccode,
                   show_rows$name)

  # Disambiguate duplicate HUC names (common ones like "Bear Creek"
  # appear multiple times) by appending the huccode in parens.
  dup_mask <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  if (any(dup_mask)) {
    labels[dup_mask] <- paste0(labels[dup_mask],
                               " (", show_rows$huccode[dup_mask], ")")
  }

  long <- data.frame(
    label = rep(labels, each = results$n_draws),
    rank  = as.vector(ranks_m)
  )
  # Reverse so rank 1 sits at the top of the plot.
  long$label <- factor(long$label, levels = rev(labels))

  ggplot2::ggplot(long, ggplot2::aes(x = rank, y = label)) +
    ggplot2::geom_boxplot(outlier.size = 0.7, fill = "#e0e7ff",
                          color = "#1f4f8b", alpha = 0.8) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::labs(
      x = "Rank (1 = highest priority)",
      y = NULL,
      title = sprintf(
        "Rank distribution over %d Monte Carlo draws (\u00B1%d%% weight uncertainty)",
        results$n_draws, results$uncertainty_pct),
      subtitle = sprintf("Top %d HUCs by median rank", n_show)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle      = ggplot2::element_text(size = 10, color = "#6c757d")
    )
}
