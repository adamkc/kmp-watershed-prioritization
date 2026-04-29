# Acquire and process Basin Characterization Model (BCM, Flint et al.)
# 1991-2020 climatology rasters for selected variables. Each variable
# becomes one column in the per-HUC10 output.
#
# Currently configured to compute:
#   * Summer water stress (CWD, mm)  -- BCM `cwd` annual climatic water
#                                        deficit, 1991-2020 mean.
#                                        Direction: negative (high CWD =
#                                        water-stressed = lower priority).
#
# Other BCM rasters present on disk (`aet`, `pck`, `pet`, `ppt`, `rch`,
# `run`, `str`, `tmn`, `tmx`) can be added by extending VARS below.
#
# Source: USGS Basin Characterization Model (Flint et al.). Rasters
# manually placed under `data/source/bcm/` as ESRI ASCII grids (270 m,
# California Albers / EPSG:3310). NoData = -9999 force-masked.
#
# Output: data/source/bcm/huc10_bcm.csv

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
})

sf::sf_use_s2(FALSE)
terraOptions(progress = 0)

SRC_DIR <- "data/source/bcm"
OUT_CSV <- file.path(SRC_DIR, "huc10_bcm.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
ASSERTED_CRS <- "EPSG:3310"

# Variables to extract: list(file = "...", col = "output column name")
# Add rows here to compute additional BCM variables (aet, ppt, tmx, etc.)
VARS <- list(
  list(file = "cwd1991_2020_ave.asc",
       col  = "Summer water stress (CWD, mm)")
)

# ---- HUCs in EPSG:3310 -------------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, ASSERTED_CRS)
huc10 <- st_make_valid(huc10)
message("HUC10s: ", nrow(huc10))

kmp_bbox <- st_bbox(st_buffer(st_as_sfc(st_bbox(huc10)), 5000))
kmp_ext  <- ext(c(kmp_bbox["xmin"], kmp_bbox["xmax"],
                  kmp_bbox["ymin"], kmp_bbox["ymax"]))

# ---- Per-variable extraction ------------------------------------------

out <- data.frame(huccode = huc10$huc10, name = huc10$name,
                  stringsAsFactors = FALSE)

for (v in VARS) {
  asc_path <- file.path(SRC_DIR, v$file)
  if (!file.exists(asc_path)) {
    message("Skipping ", v$file, " -- not on disk")
    next
  }
  message("\n=== ", v$col, " ===")
  message("  reading ", asc_path)
  r <- rast(asc_path)
  crs(r) <- ASSERTED_CRS
  NAflag(r) <- -9999

  rc <- crop(r, kmp_ext)
  rc <- subst(rc, -9999, NA)
  message("  cropped: ", paste(dim(rc), collapse = "x"))

  vals <- exact_extract(rc, huc10, "mean",
                        default_value = NA_real_, progress = FALSE)
  out[[v$col]] <- vals
  cat(sprintf("  summary:\n"))
  print(summary(vals))
}

out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("\nWrote ", OUT_CSV, " (", nrow(out), " rows, ",
        ncol(out) - 2, " variable(s))")

# Print top/bottom HUCs for the first variable
if (length(VARS) > 0) {
  col1 <- VARS[[1]]$col
  cat("\nTop 10 by '", col1, "':\n", sep = "")
  print(head(out[order(-out[[col1]]),
                 c("huccode", "name", col1)], 10))
  cat("\nBottom 10:\n")
  print(head(out[order(out[[col1]]),
                 c("huccode", "name", col1)], 10))
}
