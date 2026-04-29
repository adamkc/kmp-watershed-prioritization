# Split kmp_huc10.geojson into a small KMP-only file (default, shipped
# in the shinylive bundle) and a large all-CA file (kept locally only,
# gitignored). The all-CA file was added in 64ebb43 as a "quiet feature"
# so users uploading non-KMP HUC10 CSVs would still see their watersheds
# render -- but it costs ~5 MB of bundle download for a feature most
# users never hit. The split keeps that capability available locally and
# trims the deployed bundle.
#
# Idempotent. On first run, renames the all-CA file aside before writing
# the small one. On subsequent runs, reads from the renamed file.
#
# Run from the project root:
#   Rscript scripts/split_huc10.R

library(sf)

CA_PATH    <- "data/kmp_huc10_ca.geojson"   # all-CA, gitignored
SMALL_PATH <- "data/kmp_huc10.geojson"      # KMP-only, committed
METRICS    <- "data/kmp_metrics.csv"

# Determine source. Prefer the renamed all-CA file if present;
# otherwise treat the existing kmp_huc10.geojson as the all-CA source
# (first-run case) and rename it before overwriting.
if (file.exists(CA_PATH)) {
  src <- CA_PATH
  cat(sprintf("Reading existing all-CA file: %s\n", src))
} else if (file.exists(SMALL_PATH)) {
  src <- SMALL_PATH
  cat(sprintf("Renaming %s -> %s before resplit\n", SMALL_PATH, CA_PATH))
  file.rename(SMALL_PATH, CA_PATH)
  src <- CA_PATH
} else {
  stop("Neither ", CA_PATH, " nor ", SMALL_PATH, " exists. ",
       "Run scripts/prepare_ca_huc10.R first to build the all-CA layer.",
       call. = FALSE)
}

x <- st_read(src, quiet = TRUE)
cat(sprintf("Source:   %d features\n", nrow(x)))

# Derive KMP HUC10 codes by truncating HUC12 huccodes from the metrics
# CSV. read.csv (base) keeps strings as character; force the column type
# explicitly to avoid scientific-notation surprises.
metrics <- read.csv(METRICS, colClasses = c(huccode = "character"),
                    check.names = FALSE)
kmp_huc10s <- unique(substr(metrics$huccode, 1, 10))
cat(sprintf("KMP HUC10s (from %s): %d unique\n", METRICS,
            length(kmp_huc10s)))

# Filter. Use HUC10 column from the boundary layer (lowercase per the
# convention in prepare_ca_huc10.R).
if (!"huc10" %in% names(x)) {
  stop("Boundary layer is missing 'huc10' column; got: ",
       paste(names(x), collapse = ", "), call. = FALSE)
}
keep <- x$huc10 %in% kmp_huc10s
out <- x[keep, , drop = FALSE]
cat(sprintf("Filtered: %d features kept (of %d)\n", nrow(out), nrow(x)))

missing <- setdiff(kmp_huc10s, x$huc10)
if (length(missing) > 0) {
  warning(sprintf("%d KMP HUC10 codes have no boundary feature: %s",
                  length(missing),
                  paste(head(missing, 10), collapse = ", ")),
          call. = FALSE)
}

st_write(out, SMALL_PATH, delete_dsn = TRUE, quiet = TRUE)
size_small <- round(file.info(SMALL_PATH)$size / 1024 / 1024, 2)
size_ca    <- round(file.info(CA_PATH)$size    / 1024 / 1024, 2)
cat(sprintf("Wrote %s (%.2f MB, %d features)\n", SMALL_PATH,
            size_small, nrow(out)))
cat(sprintf("Kept  %s (%.2f MB, %d features) -- gitignored, local only\n",
            CA_PATH, size_ca, nrow(x)))
