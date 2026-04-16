# Smoke test for R/columns.R against the clean example CSV.
# Run from the project root:
#   Rscript scripts/test_columns.R

source("R/input.R")
source("R/columns.R")

df <- read_input_csv("data/examples/exampleinput_clean.csv")
v  <- validate_input(df)

cls <- classify_columns(v$data)

# Print a compact summary.
cat("\nColumn classification (", nrow(cls), " columns)\n", sep = "")
cat(rep("-", 80), sep = ""); cat("\n")
print(cls, row.names = FALSE)

cat("\nColumns that would get weight sliders (",
    sum(cls$use_in_score), "):\n", sep = "")
cat("  ", paste(scoring_columns(cls), collapse = "\n  "), "\n", sep = "")

flagged <- cls[cls$zero_inflated, "name"]
if (length(flagged) > 0) {
  cat("\nZero-inflated columns (>=50% zeros, Jenks will choke):\n")
  cat("  ", paste(flagged, collapse = "\n  "), "\n", sep = "")
}

missing_cols <- cls[cls$n_missing > 0 & cls$use_in_score, c("name", "n_missing")]
if (nrow(missing_cols) > 0) {
  cat("\nScoring columns with missing values (slider annotation):\n")
  for (i in seq_len(nrow(missing_cols))) {
    cat(sprintf("  %-40s  %d HUCs missing\n",
                missing_cols$name[i], missing_cols$n_missing[i]))
  }
}

cat("\nDone.\n")
