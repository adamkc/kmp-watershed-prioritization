# Acquire and process MTBS (Monitoring Trends in Burn Severity) perimeters
# Computes per-HUC10:
#   * Recent fire (% area, 10yr) -- union of fire perimeters from the past
#     10 years, intersected with each HUC10
#   * Unburned 30yr (%) -- complement of the past-30-year burn union per HUC10
#   * High severity fire (% area) -- placeholder NA; needs the severity
#     raster mosaics, URL pattern not yet confirmed
#
# Output: data/source/mtbs/huc10_mtbs_metrics.csv
#
# Source: MTBS Burned Areas Boundaries shapefile (composite, 1984-present)
# https://www.mtbs.gov/direct-download
#
# Run: Rscript scripts/acquire_mtbs.R

suppressPackageStartupMessages({
  library(sf)
  library(readr)
})

sf::sf_use_s2(FALSE)

PERIM_URL  <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
SRC_DIR    <- "data/source/mtbs"
ZIP_PATH   <- file.path(SRC_DIR, "mtbs_perimeter_data.zip")
UNZIP_DIR  <- file.path(SRC_DIR, "perims")
OUT_CSV    <- file.path(SRC_DIR, "huc10_mtbs_metrics.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"

# Reference date for the rolling windows
TODAY      <- Sys.Date()
RECENT_CUT <- TODAY - 365.25 * 10   # past 10 years
LONG_CUT   <- TODAY - 365.25 * 30   # past 30 years

# Equal-area CRS for area math
PLANAR_CRS <- 5070

dir.create(SRC_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Download perimeters (cache locally) ------------------------------

EXPECTED_SIZE <- 346951504
if (!file.exists(ZIP_PATH) || file.info(ZIP_PATH)$size < EXPECTED_SIZE) {
  message("Downloading MTBS perimeters (~347 MB) ...")
  options(timeout = 1200)  # 20 min ceiling for slow links
  download.file(PERIM_URL, ZIP_PATH, mode = "wb", method = "libcurl")
} else {
  message("MTBS perimeters already present at ", ZIP_PATH,
          " (", round(file.info(ZIP_PATH)$size / 1e6, 1), " MB)")
}

if (!dir.exists(UNZIP_DIR) || length(list.files(UNZIP_DIR)) == 0) {
  message("Unzipping ...")
  unzip(ZIP_PATH, exdir = UNZIP_DIR)
}

shp <- list.files(UNZIP_DIR, pattern = "\\.shp$", full.names = TRUE,
                  recursive = TRUE)
if (length(shp) == 0) stop("No .shp found under ", UNZIP_DIR)
message("Reading ", shp[1])

perims <- read_sf(shp[1])
message("MTBS perimeters: ", nrow(perims))
message("Columns: ", paste(names(perims), collapse = ", "))

# ---- 2. Identify date column and bbox-clip to study area ----------------

# MTBS perimeters carry a date column ("Ig_Date" in recent versions, "FireDate"
# or "StartDate" in older). Detect it.
date_candidates <- c("ig_date", "Ig_Date", "IG_DATE", "FireDate", "StartDate", "DATE_")
date_col <- date_candidates[match(TRUE, tolower(date_candidates) %in%
                                  tolower(names(perims)))]
if (is.na(date_col)) stop("No recognised date column. Available: ",
                          paste(names(perims), collapse = ", "))
# Match case-insensitively to actual column name
date_col <- names(perims)[tolower(names(perims)) == tolower(date_col)][1]
message("Using date column: ", date_col)

perims$.fire_date <- as.Date(perims[[date_col]])
n_bad <- sum(is.na(perims$.fire_date))
if (n_bad > 0) message("  ", n_bad, " perimeters with unparsable dates dropped")
perims <- perims[!is.na(perims$.fire_date), ]

# Drop Z/M, fix invalid, project
perims <- st_zm(perims, drop = TRUE, what = "ZM")
perims <- st_transform(perims, PLANAR_CRS)
perims <- st_make_valid(perims)

# Clip to KMP-zone bbox to dramatically reduce data volume
huc_all <- read_sf(HUC10_FILE)
metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- huc_all[huc_all$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
huc10$huc_area_m2 <- as.numeric(st_area(huc10))
message("HUC10 features: ", nrow(huc10))

# Buffer the HUC bbox by 5 km so fires that cross HUC edges are kept
huc_bbox <- st_as_sfc(st_bbox(st_buffer(st_as_sfc(st_bbox(huc10)), 5000)))
perims_local <- perims[lengths(st_intersects(perims, huc_bbox)) > 0, ]
message("MTBS perimeters in study bbox: ", nrow(perims_local))

# ---- 3. Build the two time-window unions --------------------------------

recent <- perims_local[perims_local$.fire_date >= RECENT_CUT, ]
long   <- perims_local[perims_local$.fire_date >= LONG_CUT, ]
message("Recent (>= ", RECENT_CUT, "): ", nrow(recent), " fires")
message("Long  (>= ", LONG_CUT,   "): ", nrow(long),   " fires")

union_intersect_area <- function(fires, hucs) {
  if (nrow(fires) == 0) {
    return(setNames(rep(0, nrow(hucs)), hucs$huc10))
  }
  message("  unioning ", nrow(fires), " fire polygons ...")
  u <- st_union(fires)
  u <- st_make_valid(u)
  message("  intersecting union with ", nrow(hucs), " HUCs ...")
  inter <- st_intersection(hucs[, "huc10"], u)
  inter$area_m2 <- as.numeric(st_area(inter))
  agg <- aggregate(area_m2 ~ huc10, data = st_drop_geometry(inter), FUN = sum)
  out <- setNames(rep(0, nrow(hucs)), hucs$huc10)
  out[agg$huc10] <- agg$area_m2
  out
}

message("Computing 10-yr union ...")
recent_area <- union_intersect_area(recent, huc10)
message("Computing 30-yr union ...")
long_area   <- union_intersect_area(long,   huc10)

# ---- 4. Assemble per-HUC10 output ---------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name", "huc_area_m2")]
out$recent_burned_m2 <- recent_area[match(out$huc10, names(recent_area))]
out$long_burned_m2   <- long_area[match(out$huc10, names(long_area))]

out$`Recent fire (% area, 10yr)` <-
  pmin(100 * out$recent_burned_m2 / out$huc_area_m2, 100)
out$`Unburned 30yr (%)` <-
  pmax(100 * (1 - out$long_burned_m2 / out$huc_area_m2), 0)
# High-severity placeholder until severity-mosaic URL is resolved
out$`High severity fire (% area)` <- NA_real_

names(out)[1] <- "huccode"
out <- out[order(out$huccode), c("huccode", "name", "huc_area_m2",
                                 "recent_burned_m2", "long_burned_m2",
                                 "Recent fire (% area, 10yr)",
                                 "Unburned 30yr (%)",
                                 "High severity fire (% area)")]

write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Recent fire (% area, 10yr) ===\n")
print(summary(out$`Recent fire (% area, 10yr)`))
cat("\n=== Unburned 30yr (%) ===\n")
print(summary(out$`Unburned 30yr (%)`))
cat("\nTop 10 most-recently-burned HUCs:\n")
print(head(out[order(-out$`Recent fire (% area, 10yr)`),
              c("huccode", "name", "Recent fire (% area, 10yr)",
                "Unburned 30yr (%)")], 10))
