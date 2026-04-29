# Acquire SNODAS (NOAA G02158) snow water equivalent rasters and
# compute per-HUC10:
#   * Snowpack (peak SWE, mm)            -- mean April 1 SWE across years
#   * Snowpack trajectory (mm/yr)        -- linear trend of April 1 SWE
#                                           across the available years
#
# Source: NSIDC public NOAA archive, masked CONUS daily SNODAS:
#   https://noaadata.apps.nsidc.org/NOAA/G02158/masked/<YYYY>/<MM_Mon>/SNODAS_<YYYYMMDD>.tar
# Each daily TAR contains gzipped .dat binary grids for several variables
# plus a .txt header per variable. Product code 1034 = SWE (instantaneous,
# mm). Grid is 30-arcsecond geographic (EPSG:4326), 6935 x 3351, with
# NoData = -9999.
#
# This script:
#   1. Downloads April 1 TARs for the requested year range (cached)
#   2. Extracts the SWE .dat.gz from each, gunzips it
#   3. Parses the matching .txt header for grid dimensions and origin
#   4. Builds a SpatRaster using those parameters, masks NoData
#   5. Crops to the KMP bbox before any further work (CONUS grid is huge)
#   6. Mean across years = peak SWE; linear trend across years = trajectory
#   7. Zonal mean per HUC10 for both
#
# Output: data/source/snodas/huc10_snowpack.csv

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
})

sf::sf_use_s2(FALSE)
terraOptions(progress = 0)

YEARS      <- 2003:2025  # April 1 of each (SNODAS production began late 2003)
SRC_DIR    <- "data/source/snodas"
TAR_DIR    <- file.path(SRC_DIR, "tar")
WORK_DIR   <- file.path(SRC_DIR, "work")
OUT_CSV    <- file.path(SRC_DIR, "huc10_snowpack.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070

dir.create(TAR_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

month_name <- function(m) {
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[m]
}

# ---- 1. Download one daily TAR (cached) ---------------------------------

snodas_tar_url <- function(date) {
  yr <- format(date, "%Y"); mn <- as.integer(format(date, "%m"))
  ymd <- format(date, "%Y%m%d")
  sprintf("https://noaadata.apps.nsidc.org/NOAA/G02158/masked/%s/%02d_%s/SNODAS_%s.tar",
          yr, mn, month_name(mn), ymd)
}

download_tar <- function(date) {
  ymd  <- format(date, "%Y%m%d")
  dest <- file.path(TAR_DIR, paste0("SNODAS_", ymd, ".tar"))
  if (file.exists(dest) && file.info(dest)$size > 1e6) {
    return(dest)
  }
  url <- snodas_tar_url(date)
  message("  download ", url)
  options(timeout = 600)
  download.file(url, dest, mode = "wb", quiet = TRUE, method = "libcurl")
  dest
}

# ---- 2. Extract the SWE .dat (and header .txt) from a daily TAR --------

# Returns a list(dat = path, txt = path) for the SWE snapshot variable.
# SNODAS filename code for SWE snapshot is the substring "1034tS".
extract_swe <- function(tar_path, work_subdir) {
  ex <- file.path(WORK_DIR, work_subdir)
  dir.create(ex, recursive = TRUE, showWarnings = FALSE)
  files <- untar(tar_path, list = TRUE)
  swe_files <- files[grepl("1034tS", files)]
  if (length(swe_files) == 0) stop("No SWE file in ", tar_path)
  untar(tar_path, files = swe_files, exdir = ex)

  # In the TAR both .dat and .txt are gzipped (.dat.gz, .txt.gz). Find each.
  dat_gz <- list.files(ex, pattern = "1034tS.*\\.dat\\.gz$",
                       full.names = TRUE)
  txt_gz <- list.files(ex, pattern = "1034tS.*\\.txt\\.gz$",
                       full.names = TRUE)
  if (length(dat_gz) != 1 || length(txt_gz) != 1) {
    stop("Unexpected files in ", ex, ": ", paste(list.files(ex), collapse=","))
  }
  dat <- sub("\\.gz$", "", dat_gz)
  txt <- sub("\\.gz$", "", txt_gz)
  if (!file.exists(dat)) R.utils::gunzip(dat_gz, destname = dat, remove = FALSE)
  if (!file.exists(txt)) R.utils::gunzip(txt_gz, destname = txt, remove = FALSE)
  list(dat = dat, txt = txt)
}

# ---- 3. Parse the .txt header to read grid params ----------------------

# SNODAS .txt headers are key/value lines. We need the columns, rows,
# data type, and the geographic origin / cell size.
parse_header <- function(txt_path) {
  lines <- readLines(txt_path, warn = FALSE)
  kv <- strsplit(lines, ":\\s*")
  kv <- kv[lengths(kv) == 2]
  k <- vapply(kv, `[`, character(1), 1)
  v <- vapply(kv, `[`, character(1), 2)
  names(v) <- trimws(k)
  v
}

# ---- 4. Build a SpatRaster from the binary .dat ------------------------

# SNODAS masked CONUS specs (post-2010):
#   ncol = 6935, nrow = 3351
#   resolution = 0.00833333... deg (= 30 arc-seconds)
#   NW corner of NW pixel: lon -124.73375, lat 52.87083
#   data type: int16, little-endian; NoData = -9999; SWE units: mm
build_swe_rast <- function(dat_path, txt_path, kmp_ext_4326) {
  hdr <- parse_header(txt_path)

  ncol <- as.integer(hdr["Number of columns"])
  nrow <- as.integer(hdr["Number of rows"])
  ll_lon <- as.numeric(hdr["Minimum x-axis coordinate"])
  ll_lat <- as.numeric(hdr["Minimum y-axis coordinate"])
  ur_lon <- as.numeric(hdr["Maximum x-axis coordinate"])
  ur_lat <- as.numeric(hdr["Maximum y-axis coordinate"])
  ndv    <- as.numeric(hdr["No data value"])
  if (is.na(ncol) || is.na(nrow)) stop("Header missing dimensions")

  # Read full grid (~50 MB int16 = ~25 MB)
  raw <- readBin(dat_path, what = "integer", n = ncol * nrow,
                 size = 2, signed = TRUE, endian = "big")
  # NSIDC docs say SNODAS .dat is BIG-endian -- not little

  r <- rast(ncol = ncol, nrow = nrow,
            xmin = ll_lon, xmax = ur_lon,
            ymin = ll_lat, ymax = ur_lat,
            crs = "EPSG:4326")
  values(r) <- raw
  r[r == ndv] <- NA
  # SWE in masked SNODAS is mm * 1 (no scale)
  # Crop to KMP extent immediately to keep memory low
  crop(r, kmp_ext_4326)
}

# ---- 5. HUC10s ---------------------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10_4326 <- huc10
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
message("HUC10s: ", nrow(huc10))

bb <- st_bbox(huc10_4326)
bb <- bb + c(-0.2, -0.2, 0.2, 0.2)
kmp_ext_4326 <- ext(c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))

# ---- 6. Loop years; build per-year SWE rasters -------------------------

if (!requireNamespace("R.utils", quietly = TRUE)) {
  install.packages("R.utils", repos = "https://cloud.r-project.org")
}

stack_list <- list()
year_labels <- integer()
for (yr in YEARS) {
  date <- as.Date(sprintf("%d-04-01", yr))
  message("\n=== ", date, " ===")
  tar_path <- tryCatch(download_tar(date),
                       error = function(e) {
                         message("  download failed: ", e$message); NULL
                       })
  if (is.null(tar_path)) next
  fl <- tryCatch(extract_swe(tar_path, format(date, "%Y-%m-%d")),
                 error = function(e) {
                   message("  extract failed: ", e$message); NULL
                 })
  if (is.null(fl)) next
  r <- tryCatch(build_swe_rast(fl$dat, fl$txt, kmp_ext_4326),
                error = function(e) {
                  message("  raster build failed: ", e$message); NULL
                })
  if (is.null(r)) next
  message(sprintf("  KMP bbox SWE: min=%.0f mean=%.0f max=%.0f n_nonzero=%d",
                  global(r, "min", na.rm = TRUE)[1, 1],
                  global(r, "mean", na.rm = TRUE)[1, 1],
                  global(r, "max", na.rm = TRUE)[1, 1],
                  global(r > 0, "sum", na.rm = TRUE)[1, 1]))
  stack_list[[length(stack_list) + 1]] <- r
  year_labels <- c(year_labels, yr)
}
if (length(stack_list) == 0) stop("No SNODAS rasters built")
message("\nSuccessful years: ", paste(year_labels, collapse = ", "))

# ---- 7. Mean and trend across years -----------------------------------

stk <- rast(stack_list)
names(stk) <- paste0("y", year_labels)
mean_swe <- mean(stk, na.rm = TRUE)

# Two trend metrics per pixel:
#   * OLS slope -- terra::regress, sensitive to extreme years
#   * Theil-Sen slope (median of pairwise slopes) -- robust to outliers
#     like the 2023 record-snow year
trend_full <- regress(stk, year_labels, na.rm = TRUE)
slope_idx <- which(grepl("slope|^x$|^year", names(trend_full)))
if (length(slope_idx) == 0) slope_idx <- nlyr(trend_full)
message("regress() layer names: ",
        paste(names(trend_full), collapse = ", "),
        " -- using layer ", slope_idx, " as OLS slope")
trend_ols <- trend_full[[slope_idx]]

message("Computing Theil-Sen slope per pixel ...")
ts_slope_fun <- function(y, x = year_labels) {
  ok <- !is.na(y)
  if (sum(ok) < 5) return(NA_real_)
  yy <- y[ok]; xx <- x[ok]
  pairs <- utils::combn(length(yy), 2)
  slopes <- (yy[pairs[2, ]] - yy[pairs[1, ]]) /
            (xx[pairs[2, ]] - xx[pairs[1, ]])
  median(slopes, na.rm = TRUE)
}
trend_ts <- app(stk, ts_slope_fun)
names(trend_ts) <- "ts_slope"

# ---- 8. Zonal stats per HUC10 ------------------------------------------

mean_swe_5070  <- project(mean_swe,  paste0("EPSG:", PLANAR_CRS),
                          method = "bilinear")
trend_ols_5070 <- project(trend_ols, paste0("EPSG:", PLANAR_CRS),
                          method = "bilinear")
trend_ts_5070  <- project(trend_ts,  paste0("EPSG:", PLANAR_CRS),
                          method = "bilinear")

huc10$`Snowpack (peak SWE, mm)` <-
  exact_extract(mean_swe_5070, huc10, "mean",
                default_value = NA_real_, progress = FALSE)
huc10$`Snowpack trajectory OLS (mm/yr)` <-
  exact_extract(trend_ols_5070, huc10, "mean",
                default_value = NA_real_, progress = FALSE)
huc10$`Snowpack trajectory Theil-Sen (mm/yr)` <-
  exact_extract(trend_ts_5070, huc10, "mean",
                default_value = NA_real_, progress = FALSE)

# ---- 9. Write output ---------------------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name",
                                   "Snowpack (peak SWE, mm)",
                                   "Snowpack trajectory OLS (mm/yr)",
                                   "Snowpack trajectory Theil-Sen (mm/yr)")]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Snowpack (peak SWE, mm) ===\n")
print(summary(out$`Snowpack (peak SWE, mm)`))
cat("\n=== Snowpack trajectory OLS (mm/yr) ===\n")
print(summary(out$`Snowpack trajectory OLS (mm/yr)`))
cat("\n=== Snowpack trajectory Theil-Sen (mm/yr) ===\n")
print(summary(out$`Snowpack trajectory Theil-Sen (mm/yr)`))

cat("\nTop 10 HUCs by mean April-1 SWE (with both trends):\n")
print(head(out[order(-out$`Snowpack (peak SWE, mm)`), ], 10))
