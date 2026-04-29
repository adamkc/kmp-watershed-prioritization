# Acquire and process TNC Resilient and Connected Network (Anderson et al.)
# resilient-sites raster. Computes per-HUC10:
#
#   * Climate resiliency index -- mean of TNC Terrestrial Resilience score
#                                 across each HUC10. Score is a regional
#                                 z-score (range -3.501 to +3.500 SD);
#                                 higher = more resilient under climate change.
#
# Source: TNC "Resilient and Connected Network" geodatabase (RCN_Data.gdb),
# manually placed under `data/source/tnc-rl/`. The `Terrestrial_resilience`
# subdataset stores the score directly as INT2S pixel values scaled ×1000
# (e.g. pixel value -3501 = -3.501 SD). NoData values fall outside this
# range. We use this subdataset rather than `Resilient_Sites_Terrand_Coast`
# because GDAL's OpenFileGDB driver doesn't decode the latter's RAT keys
# correctly into the 32-bit pixel space.
#
# Output: data/source/tnc-rl/huc10_climate_resiliency.csv

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
})

sf::sf_use_s2(FALSE)
terraOptions(progress = 0)

GDB        <- "data/source/tnc-rl/RCN_Data.gdb"
LAYER      <- "Terrestrial_resilience"
SCORE_DIV  <- 1000  # pixel int / 1000 = z-score
VALID_RANGE <- c(-3501, 3500)  # outside this range = NoData

SRC_DIR    <- "data/source/tnc-rl"
OUT_CSV    <- file.path(SRC_DIR, "huc10_climate_resiliency.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070

# ---- 1. HUCs and KMP extent --------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
message("HUC10s: ", nrow(huc10))

kmp_bbox <- st_bbox(st_buffer(st_as_sfc(st_bbox(huc10)), 5000))
kmp_ext  <- ext(c(kmp_bbox["xmin"], kmp_bbox["xmax"],
                  kmp_bbox["ymin"], kmp_bbox["ymax"]))

# ---- 2. Read raster, crop, scale to z-score ----------------------------

r <- rast(sprintf("OpenFileGDB:%s:%s", GDB, LAYER))
message("Source raster: ", paste(dim(r), collapse = "x"),
        ", res ", paste(round(res(r), 1), collapse = "x"),
        ", dtype ", datatype(r),
        ", CRS ", crs(r, describe = TRUE)$name)

rc <- crop(r, kmp_ext)
message("Cropped: ", paste(dim(rc), collapse = "x"))

# Mask out values outside the documented score range (NoData / sentinel)
rc[rc < VALID_RANGE[1] | rc > VALID_RANGE[2]] <- NA
r_score <- rc / SCORE_DIV

samp <- spatSample(r_score, 5000, na.rm = TRUE)[[1]]
cat("Score sample summary:\n"); print(summary(samp))

# ---- 3. Zonal mean per HUC10 -------------------------------------------

message("Zonal mean per HUC10 ...")
huc10$`Climate resiliency index` <- exact_extract(
  r_score, huc10, "mean", default_value = NA_real_, progress = FALSE
)

# ---- 4. Write output --------------------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name", "Climate resiliency index")]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows; ",
        sum(!is.na(out$`Climate resiliency index`)), " computed, ",
        sum(is.na(out$`Climate resiliency index`)), " NA)")

cat("\n=== Climate resiliency index distribution ===\n")
print(summary(out$`Climate resiliency index`))
cat("\nTop 10 most resilient HUCs:\n")
print(head(out[order(-out$`Climate resiliency index`), ], 10))
cat("\nBottom 10 least resilient HUCs:\n")
print(head(out[order(out$`Climate resiliency index`), ], 10))
