# Smoke test for R/score.R.
#
# Runs the full pipeline (read -> validate -> classify -> bin -> composite)
# against the clean Scott River example, and compares the top-5 output to
# the workbook's known top-5 under approximately the workbook's weights.
#
# Workbook top-5 (from scoring sheet in the draft workbook):
#   1. Lower East Fork Scott River  (27.3)
#   2. Upper East Fork Scott River  (26.0)
#   3. South Fork Scott River       (24.3)
#   4. Kelsey Creek                 (24.3)
#   5. Kidder Creek                 (23.3)
#
# Our composite uses weighted mean (1..5 scale), while the workbook uses
# weighted sum -- so the exact numeric composite will differ, but the
# top-5 set should match.

source("R/input.R")
source("R/columns.R")
source("R/score.R")

df <- read_input_csv("data/examples/exampleinput_clean.csv")
v  <- validate_input(df)
cls <- classify_columns(v$data)
bins <- compute_bin_scores(v$data, cls)

# --- Test 1: unit weights -----------------------------------------------------

cat("\n=== Test 1: unit weights (all metrics weight 1) ===\n")
w_unit <- setNames(rep(1, ncol(bins)), names(bins))
comp_unit <- composite_score(bins, w_unit)
rk_unit <- rank_hucs(comp_unit)

top_unit <- data.frame(
  rank     = rk_unit,
  huccode  = v$data$huccode,
  name     = v$data$`HUC12 name`,
  score    = round(comp_unit, 3)
)
top_unit <- top_unit[order(top_unit$rank), ]
print(head(top_unit, 10), row.names = FALSE)


# --- Test 2: approximately workbook weights -----------------------------------

cat("\n=== Test 2: approximately workbook weights ===\n")
w_wb <- c(
  "SUM of ClimRankAve ByArea"      = 0,
  "SUM of ConnRankAveByArea"       = 1,
  "SUM of RareCountAveByArea"      = 0,
  "SUM of RareAmphAveByArea"       = 0.333,
  "SUM of RarePlantAveByArea"      = 0.333,
  "Apr1SWE"                        = 1,
  "Coho score"                     = 0.333,
  "Cascades frog score"            = 0,
  "aspen score"                    = 0,
  "darlingtonia score"             = 0,
  "Inventoried MdwPercent (/100)"  = 1,
  "inventoried mdw acres"          = 0,
  "MdwNEPA percent"                = 5,
  "MdwNEPA Ac"                     = 0,
  "Fire percent"                   = 1,
  "Fire Ac"                        = 0,
  "LMMhigh percent"                = 0,
  "LMMhigh Ac"                     = 0,
  "LMMmed Percent"                 = 0,
  "LMMmed Ac"                      = 0,
  "EFM percent"                    = 1,
  "EFM_Ac"                         = 0,
  "Federal Land Percent"           = 1,
  "Feds_Ac"                        = 0
)

comp_wb <- composite_score(bins, w_wb)
rk_wb <- rank_hucs(comp_wb)

top_wb <- data.frame(
  rank     = rk_wb,
  huccode  = v$data$huccode,
  name     = v$data$`HUC12 name`,
  score    = round(comp_wb, 3)
)
top_wb <- top_wb[order(top_wb$rank), ]
print(head(top_wb, 10), row.names = FALSE)

# --- Validation: does our top-5 match the workbook's top-5? ------------------

expected_top5 <- c(
  "Lower East Fork Scott River",
  "Upper East Fork Scott River",
  "South Fork Scott River",
  "Kelsey Creek",
  "Kidder Creek"
)
our_top5 <- head(top_wb$name, 5)

cat("\nExpected top-5:\n  ", paste(expected_top5, collapse = "\n  "), "\n", sep = "")
cat("Our top-5:\n  ",      paste(our_top5,      collapse = "\n  "), "\n", sep = "")

hits <- sum(expected_top5 %in% our_top5)
cat(sprintf("\n%d / 5 of the workbook's top-5 appear in ours.\n", hits))

cat("\nDone.\n")
