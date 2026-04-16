# Smoke test for R/scenarios.R.
#
# Loads the scenario catalog, validates every scenario's metric
# references against the master data, and prints a summary.
#
# Run from the project root:
#   Rscript scripts/test_scenarios.R

source("R/input.R")
source("R/scenarios.R")

master <- read_input_csv("data/kmp_metrics.csv")
metric_cols <- setdiff(names(master), c("huccode", "HUC12 name"))

scenarios <- load_scenarios("data/scenarios.yaml")

cat(sprintf("Loaded %d scenario(s) from data/scenarios.yaml\n\n", length(scenarios)))

for (s in scenarios) {
  cat(sprintf("  [%s] %s\n", s$id, s$name))
  cat(sprintf("    %d metrics; weight sum = %.2f\n",
              length(s$weights), sum(s$weights)))
  cat(sprintf("    %s\n\n",
              paste(strwrap(s$description, width = 68), collapse = "\n    ")))
}

v <- validate_scenarios(scenarios, metric_cols)
if (v$ok) {
  cat("All scenarios reference valid metrics.\n")
} else {
  cat("Validation issues:\n")
  for (msg in v$issues) cat("  ", msg, "\n")
}
