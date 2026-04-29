# Smoke test: simulate what shinylive does at app startup. Source app.R
# under a fresh session and report whether the app object materialises
# cleanly. If this fails locally, we have an app-side bug independent
# of webR / rlang.

# Tee stderr to stdout so we see all messages in order
options(warn = 1)

cat("=== R session info ===\n")
cat("R:", R.version.string, "\n")

cat("\n=== sourcing R/ helper files ===\n")
for (f in list.files("R", "\\.R$", full.names = TRUE)) {
  cat("  source(", f, ")\n", sep = "")
  source(f)
}

cat("\n=== loading metric metadata + scenarios ===\n")
m <- load_metrics_meta("data/metrics.yaml")
cat("  ", length(m$meta), "metric meta entries\n")

s <- load_scenarios("data/scenarios.yaml")
cat("  ", length(s), "scenarios\n")

cat("\n=== loading master CSV ===\n")
master <- read_input_csv("data/kmp_metrics.csv")
v <- validate_input(master)
cls <- classify_columns(v$data, id_col = "huccode")
real_metric_names <- cls$name[cls$use_in_score]
cat("  ", nrow(v$data), "rows,",
    length(real_metric_names), "real metrics,",
    "HUC level", v$huc_level, "\n")

cat("\n=== validating scenarios against master ===\n")
res <- validate_scenarios(s, real_metric_names)
cat("  ok =", res$ok, "\n")
if (!res$ok) for (i in res$issues) cat("  ISSUE:", i, "\n")

cat("\n=== sourcing app.R (UI + server defs only -- not running) ===\n")
# This is what shinylive does: it sources app.R which assigns a `shinyApp(...)`
# object. If anything in our app.R code throws at source-time, we see it here.
suppressWarnings({
  app_obj <- tryCatch({
    # sys.source so library() inside app.R takes effect in the global env
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    # The last expression in app.R should be the shinyApp() object.
    last <- env$.Last.value %||% get0("app", envir = env, ifnotfound = NULL)
    if (is.null(last)) {
      "(app.R sourced but no shinyApp object captured -- look at last expr)"
    } else {
      class(last)[1]
    }
  }, error = function(e) {
    cat("  *** ERROR during app.R source: ", conditionMessage(e), "\n")
    paste("ERROR:", conditionMessage(e))
  })
})
cat("  app object class:", app_obj, "\n")

cat("\n=== done ===\n")
