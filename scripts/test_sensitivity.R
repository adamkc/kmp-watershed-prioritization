# Smoke test for R/sensitivity.R using the simulated HUC10 data +
# KMP General scenario weights. Prints a summary of rank stability
# and saves the plot to a temp PNG for visual inspection.
#
# Run from the project root:
#   Rscript scripts/test_sensitivity.R

source("R/input.R")
source("R/columns.R")
source("R/score.R")
source("R/scenarios.R")
source("R/metrics.R")
source("R/sensitivity.R")

df <- read_input_csv("data/kmp_metrics.csv")
v  <- validate_input(df)
cls <- classify_columns(v$data)
bins <- compute_bin_scores(v$data, cls)

# Apply directions from metrics.yaml.
meta <- load_metrics_meta("data/metrics.yaml")
dirs <- setNames(
  vapply(names(bins), function(m) metric_direction(meta, m), character(1)),
  names(bins)
)
bins <- apply_directions(bins, dirs)

# Use KMP General scenario weights.
scenarios <- load_scenarios("data/scenarios.yaml")
scen <- get_scenario(scenarios, "kmp_general")
weights <- scen$weights[intersect(names(scen$weights), names(bins))]

# Baseline composite + rank.
baseline_comp <- composite_score(bins, weights)
baseline_rk   <- rank_hucs(baseline_comp)

# Run sensitivity
cat("Running Monte Carlo sensitivity...\n")
t0 <- Sys.time()
res <- run_sensitivity(
  bin_scores        = bins,
  baseline_weights  = weights,
  huccodes          = v$data$huccode,
  uncertainty_pct   = 30,
  n_draws           = 1000L,
  seed              = 42L
)
dt <- as.numeric(Sys.time() - t0, units = "secs")
cat(sprintf("  %d draws x %d HUCs in %.2f sec\n\n",
            res$n_draws, res$n_hucs, dt))

# Summarize
summ <- sensitivity_summary(
  results       = res,
  display_names = v$data$name,
  baseline_rank = baseline_rk,
  top_pct       = 0.10
)

cat("Top 10 HUCs by median rank:\n")
print(head(summ[, c("name", "baseline_rank", "rank_median",
                    "rank_q25", "rank_q75", "p_top")], 10),
      row.names = FALSE)

cat(sprintf("\nStability indicators (top %d%%):\n", 10))
n_stable <- sum(summ$p_top >= 0.8)
n_swing  <- sum(summ$p_top > 0 & summ$p_top < 0.2)
cat(sprintf("  HUCs with >=80%% top-10%% probability: %d\n", n_stable))
cat(sprintf("  HUCs that sometimes-but-rarely hit top-10%%: %d\n", n_swing))

# Save the plot so it can be eyeballed.
out_plot <- tempfile("rank_dist_", fileext = ".png")
ggplot2::ggsave(out_plot,
                plot_rank_distribution(res, summ, limit_top = 30),
                width = 9, height = 7, dpi = 130)
cat(sprintf("\nRank-distribution plot written to:\n  %s\n", out_plot))
