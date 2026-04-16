# Smoke test for R/input.R against both example CSVs.
# Run from the project root:
#   Rscript scripts/test_input.R

source("R/input.R")

run_case <- function(label, csv_path) {
  cat("\n================================================================\n")
  cat(label, "\n")
  cat("  ", csv_path, "\n")
  cat("================================================================\n")

  res <- tryCatch({
    df <- read_input_csv(csv_path)
    cat("  rows x cols:   ", nrow(df), " x ", ncol(df), "\n", sep = "")

    v <- validate_input(df)
    cat("  HUC level:     HUC", v$huc_level, " (auto-detected)\n", sep = "")

    bnd <- load_boundaries(v$huc_level)
    cat("  boundaries:    ", nrow(bnd), " features loaded\n", sep = "")

    j <- join_input_to_boundaries(v$data, bnd, v$huc_level)
    cat("  joined:        ", j$n_matched, " / ", j$n_input, " input rows matched\n", sep = "")
    cat("  unused geom:   ", j$n_unused_geom, " boundary features not in input\n", sep = "")
    if (length(j$unmatched_ids) > 0) {
      cat("  unmatched ids: ", paste(head(j$unmatched_ids, 5), collapse = ", "),
          if (length(j$unmatched_ids) > 5) " ..." else "", "\n", sep = "")
    }
    "PASS"
  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n", sep = "")
    "EXPECTED ERROR"
  })

  cat("  result:        ", res, "\n", sep = "")
}

# The "clean" file should flow end-to-end.
run_case(
  "CASE 1: clean input (well-formed HUC12 codes)",
  "data/examples/exampleinput_clean.csv"
)

# The original file has HUC codes truncated to scientific notation
# by Excel -- validator should reject with a helpful error.
run_case(
  "CASE 2: raw Excel export (scientific-notation HUC codes)",
  "data/examples/exampleinput.csv"
)

cat("\nDone.\n")
