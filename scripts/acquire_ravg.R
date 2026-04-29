# Acquire and process USFS RAVG (Rapid Assessment of Vegetation Condition
# after Wildfire) annual CBI-4 mosaics. Produces per-HUC10:
#
#   * High severity fire (% area)            -- class 4 union, 10y window
#   * Moderate-high severity fire (% area)   -- class >= 3 union, 10y window
#   * Recent fire (% area, 3yr)              -- class > 1 union, 3y window
#   * Reburn area (% area)                   -- fraction of HUC pixels where
#                                               class > 1 occurred in >= 2
#                                               distinct years across the
#                                               full RAVG record
#
# Source: USFS Geospatial Technology and Applications Center (RAVG).
# Annual zips already in `data/source/ravg/ravg_<YEAR>_cbi4.zip`. Each
# zip contains <name>.tif (4-class composite burn index, NAD83/CONUS
# Albers, 30 m). Read directly via GDAL /vsizip/.
#
# Output: data/source/ravg/huc10_ravg_metrics.csv

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
})

sf::sf_use_s2(FALSE)
terraOptions(progress = 0)

SRC_DIR    <- "data/source/ravg"
OUT_CSV    <- file.path(SRC_DIR, "huc10_ravg_metrics.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070

# Calendar-year windows. Inclusive endpoints, matching the convention used
# in MTBS ("10y window" -> 11 calendar years).
THIS_YEAR  <- as.integer(format(Sys.Date(), "%Y"))
WIN_10YR   <- (THIS_YEAR - 10):THIS_YEAR
WIN_3YR    <- (THIS_YEAR - 3):THIS_YEAR

# ---- 1. HUCs and KMP extent --------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc_all <- read_sf(HUC10_FILE)
huc10 <- huc_all[huc_all$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
huc10$huc_area_m2 <- as.numeric(st_area(huc10))
message("HUC10s: ", nrow(huc10))

kmp_bbox <- st_bbox(st_buffer(st_as_sfc(st_bbox(huc10)), 5000))
kmp_ext  <- ext(c(kmp_bbox["xmin"], kmp_bbox["xmax"],
                  kmp_bbox["ymin"], kmp_bbox["ymax"]))

# ---- 2. Discover available year zips -----------------------------------

zips <- list.files(SRC_DIR, pattern = "^ravg_\\d{4}_cbi4\\.zip$",
                   full.names = TRUE)
years_available <- sort(as.integer(sub(".*ravg_(\\d{4})_.*", "\\1", zips)))
message("RAVG years available: ", paste(range(years_available), collapse = "-"),
        " (", length(years_available), " years)")

# ---- 3. Iterate years; build four accumulator rasters -----------------
#
#   hs10       -- high severity (class == 4) union, restricted to WIN_10YR
#   modhs10    -- moderate-or-high (class >= 3) union, restricted to WIN_10YR
#   any3       -- any burn (class > 1) union, restricted to WIN_3YR
#   reburn_sum -- count of years where class > 1, ALL available years

accum_max <- function(acc, layer) {
  if (is.null(acc)) layer else max(acc, layer)
}
accum_sum <- function(acc, layer) {
  if (is.null(acc)) layer else acc + layer
}

hs10 <- modhs10 <- any3 <- reburn_sum <- NULL

for (yr in years_available) {
  zp <- file.path(SRC_DIR, sprintf("ravg_%d_cbi4.zip", yr))
  vsizip <- sprintf("/vsizip/%s/ravg_%d_cbi4.tif", zp, yr)
  rc <- crop(rast(vsizip), kmp_ext)
  rc <- subst(rc, NA, 0)
  hs <- rc == 4
  mh <- rc >= 3
  an <- rc > 1
  burn_yr <- an   # 1 if class>1, 0 otherwise -- adds to reburn_sum

  if (yr %in% WIN_10YR) {
    hs10    <- accum_max(hs10, hs)
    modhs10 <- accum_max(modhs10, mh)
  }
  if (yr %in% WIN_3YR) {
    any3 <- accum_max(any3, an)
  }
  reburn_sum <- accum_sum(reburn_sum, burn_yr)

  message(sprintf(
    "Year %d: hs=%d mh=%d any=%d burn_yr=%d",
    yr,
    global(hs, "sum", na.rm = TRUE)[1, 1],
    global(mh, "sum", na.rm = TRUE)[1, 1],
    global(an, "sum", na.rm = TRUE)[1, 1],
    global(burn_yr, "sum", na.rm = TRUE)[1, 1]))
}

stopifnot(!is.null(hs10), !is.null(modhs10), !is.null(any3),
          !is.null(reburn_sum))

# ---- 4. Zonal stats per HUC --------------------------------------------

message("Zonal: high severity (10yr) ...")
huc10$`High severity fire (% area)` <-
  100 * exact_extract(hs10,    huc10, "mean", default_value = 0,
                      progress = FALSE)
message("Zonal: moderate-high severity (10yr) ...")
huc10$`Moderate-high severity fire (% area)` <-
  100 * exact_extract(modhs10, huc10, "mean", default_value = 0,
                      progress = FALSE)
message("Zonal: any burn (3yr) ...")
huc10$`Recent fire (% area, 3yr)` <-
  100 * exact_extract(any3,    huc10, "mean", default_value = 0,
                      progress = FALSE)
message("Zonal: reburn area, >=2 years burned (",
        min(years_available), "-", max(years_available), ") ...")
reburn_mask <- reburn_sum >= 2
huc10$`Reburn area (% area)` <-
  100 * exact_extract(reburn_mask, huc10, "mean", default_value = 0,
                      progress = FALSE)

# ---- 5. Write output --------------------------------------------------

out <- st_drop_geometry(huc10)[, c(
  "huc10", "name", "huc_area_m2",
  "High severity fire (% area)",
  "Moderate-high severity fire (% area)",
  "Recent fire (% area, 3yr)",
  "Reburn area (% area)"
)]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Distribution summaries ===\n")
for (col in tail(names(out), 4)) {
  cat("---", col, "\n")
  print(summary(out[[col]]))
}

cat("\nTop 10 by Reburn area (% area):\n")
print(head(out[order(-out$`Reburn area (% area)`),
              c("huccode", "name",
                "Reburn area (% area)",
                "Recent fire (% area, 3yr)",
                "High severity fire (% area)")], 10))
