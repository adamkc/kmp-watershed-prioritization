# Plot per-HUC10 April-1 SWE time series (2004-2025) with regional OLS
# and Theil-Sen trendlines overlaid. Reads the cached SNODAS .dat files
# left behind by acquire_snodas.R (no re-download).
#
# Output: docs/plots/snodas_swe_trends.png

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
  library(ggplot2)
})

sf::sf_use_s2(FALSE)
terraOptions(progress = 0)

SRC_DIR    <- "data/source/snodas"
WORK_DIR   <- file.path(SRC_DIR, "work")
OUT_LONG   <- file.path(SRC_DIR, "huc10_swe_per_year.csv")
OUT_PLOT   <- "docs/plots/snodas_swe_trends.png"
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070

dir.create(dirname(OUT_PLOT), recursive = TRUE, showWarnings = FALSE)

# ---- 1. HUCs ------------------------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)

bb <- st_bbox(st_transform(huc10, 4326))
bb <- bb + c(-0.2, -0.2, 0.2, 0.2)
kmp_ext_4326 <- ext(c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))

# ---- 2. Re-build SpatRaster from each year's cached .dat ---------------

parse_header <- function(txt_path) {
  lines <- readLines(txt_path, warn = FALSE)
  kv <- strsplit(lines, ":\\s*")
  kv <- kv[lengths(kv) == 2]
  k <- vapply(kv, `[`, character(1), 1)
  v <- vapply(kv, `[`, character(1), 2)
  names(v) <- trimws(k)
  v
}

build_swe_rast <- function(dat_path, txt_path, ext_4326) {
  hdr  <- parse_header(txt_path)
  ncol <- as.integer(hdr["Number of columns"])
  nrow <- as.integer(hdr["Number of rows"])
  ll_lon <- as.numeric(hdr["Minimum x-axis coordinate"])
  ll_lat <- as.numeric(hdr["Minimum y-axis coordinate"])
  ur_lon <- as.numeric(hdr["Maximum x-axis coordinate"])
  ur_lat <- as.numeric(hdr["Maximum y-axis coordinate"])
  ndv    <- as.numeric(hdr["No data value"])
  raw <- readBin(dat_path, what = "integer", n = ncol * nrow,
                 size = 2, signed = TRUE, endian = "big")
  r <- rast(ncol = ncol, nrow = nrow,
            xmin = ll_lon, xmax = ur_lon, ymin = ll_lat, ymax = ur_lat,
            crs = "EPSG:4326")
  values(r) <- raw
  r[r == ndv] <- NA
  crop(r, ext_4326)
}

# Find all available year work-dirs
year_dirs <- sort(list.dirs(WORK_DIR, recursive = FALSE))
year_dirs <- year_dirs[grepl("\\d{4}-04-01$", year_dirs)]
years <- as.integer(sub(".*(\\d{4})-04-01$", "\\1", year_dirs))
message("Years available: ", paste(range(years), collapse = "-"),
        " (", length(years), ")")

# ---- 3. Extract per-HUC mean SWE per year ------------------------------

long <- list()
for (i in seq_along(year_dirs)) {
  yr <- years[i]
  dat <- list.files(year_dirs[i], pattern = "1034tS.*\\.dat$",
                    full.names = TRUE)
  txt <- list.files(year_dirs[i], pattern = "1034tS.*\\.txt$",
                    full.names = TRUE)
  if (length(dat) != 1 || length(txt) != 1) {
    message("Skipping ", yr, " (missing .dat or .txt)")
    next
  }
  r <- build_swe_rast(dat, txt, kmp_ext_4326)
  r5070 <- project(r, paste0("EPSG:", PLANAR_CRS), method = "bilinear")
  v <- exact_extract(r5070, huc10, "mean", default_value = NA_real_,
                     progress = FALSE)
  long[[length(long) + 1]] <- data.frame(
    huccode = huc10$huc10, year = yr, swe_mm = v,
    stringsAsFactors = FALSE
  )
  message(sprintf("  %d: regional mean = %.1f mm", yr, mean(v, na.rm = TRUE)))
}
df <- do.call(rbind, long)
write_csv(df, OUT_LONG)
message("Wrote ", OUT_LONG, " (", nrow(df), " rows)")

# ---- 4. Region-wide trends (for overlay lines) -------------------------

reg_mean <- aggregate(swe_mm ~ year, data = df, FUN = mean, na.rm = TRUE)
ols <- lm(swe_mm ~ year, data = reg_mean)
ols_intercept <- coef(ols)[1]; ols_slope <- coef(ols)[2]

ts_pairwise <- function(y, x) {
  pairs <- utils::combn(length(y), 2)
  median((y[pairs[2, ]] - y[pairs[1, ]]) / (x[pairs[2, ]] - x[pairs[1, ]]))
}
ts_slope <- ts_pairwise(reg_mean$swe_mm, reg_mean$year)
ts_intercept <- median(reg_mean$swe_mm - ts_slope * reg_mean$year)

message(sprintf("OLS:        slope = %+.2f mm/yr",  ols_slope))
message(sprintf("Theil-Sen:  slope = %+.2f mm/yr",  ts_slope))

# ---- 5. Plot -----------------------------------------------------------

trend_colors <- c(OLS = "#d62728", `Theil-Sen` = "#1f77b4")

p <- ggplot(df, aes(x = year, y = swe_mm, group = huccode)) +
  geom_line(alpha = 0.10, linewidth = 0.4, color = "grey20") +
  geom_abline(aes(intercept = ols_intercept, slope = ols_slope,
                  color = "OLS"),
              linewidth = 1.1, linetype = "solid") +
  geom_abline(aes(intercept = ts_intercept, slope = ts_slope,
                  color = "Theil-Sen"),
              linewidth = 1.1, linetype = "longdash") +
  geom_point(data = reg_mean, aes(x = year, y = swe_mm), inherit.aes = FALSE,
             color = "black", size = 1.6) +
  geom_line(data = reg_mean, aes(x = year, y = swe_mm), inherit.aes = FALSE,
            color = "black", linewidth = 0.7) +
  scale_color_manual(values = trend_colors,
                     name = NULL,
                     labels = c(
                       OLS = sprintf("OLS slope: %+.2f mm/yr", ols_slope),
                       `Theil-Sen` = sprintf("Theil-Sen slope: %+.2f mm/yr",
                                             ts_slope)
                     )) +
  scale_x_continuous(breaks = seq(min(df$year), max(df$year), 2)) +
  labs(
    title = "April 1 Snow Water Equivalent — KMP HUC10s, 2004–2025",
    subtitle = paste0(
      "Each grey line is one of ", length(unique(df$huccode)),
      " HUC10 watersheds (alpha = 0.10). ",
      "Black points/line = regional mean. SNODAS masked CONUS (NSIDC G02158)."),
    x = "Year",
    y = "Mean April 1 SWE (mm) per HUC10",
    caption = "Theil-Sen median slope is robust to extreme years (2015 drought, 2023 record)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

ggsave(OUT_PLOT, p, width = 10, height = 6, dpi = 150, bg = "white")
message("Wrote ", OUT_PLOT)
