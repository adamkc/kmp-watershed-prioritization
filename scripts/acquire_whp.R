# Acquire and process USFS Wildfire Hazard Potential (WHP) raster.
# Computes per-HUC10:
#   * Wildfire hazard potential -- area-weighted mean of the continuous
#     CONUS WHP score across each HUC10
#
# Source: USFS Rocky Mountain Research Station, WHP version 2023.
# Manually placed under `data/source/wfp/`. The package ships
# both raster forms; we use the continuous (`_cnt_conus.tif`) for finer
# resolution. WHP raster is already in NAD83/CONUS Albers (EPSG:5070,
# 270 m), no reprojection of the source needed.
#
# Output: data/source/wfp/huc10_whp.csv

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
})

sf::sf_use_s2(FALSE)
terraOptions(progress = 0)

WHP_TIF    <- "data/source/wfp/whp2023_GeoTIF/whp2023_cnt_conus.tif"
SRC_DIR    <- "data/source/wfp"
OUT_CSV    <- file.path(SRC_DIR, "huc10_whp.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070

# ---- 1. HUCs in EPSG:5070 ----------------------------------------------

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

# ---- 2. Read raster, crop ---------------------------------------------

r <- rast(WHP_TIF)
message("Source: ", paste(dim(r), collapse = "x"),
        ", res ", paste(round(res(r), 0), collapse = "x"),
        ", dtype ", datatype(r),
        ", CRS ", crs(r, describe = TRUE)$name)

rc <- crop(r, kmp_ext)
message("Cropped: ", paste(dim(rc), collapse = "x"))

samp <- spatSample(rc, 5000, na.rm = TRUE)[[1]]
cat("KMP-bbox WHP value summary:\n"); print(summary(samp))

# ---- 3. Zonal mean ------------------------------------------------------

message("Zonal mean per HUC10 ...")
huc10$`Wildfire hazard potential` <- exact_extract(
  rc, huc10, "mean", default_value = NA_real_, progress = FALSE
)

# ---- 4. Write output ---------------------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name",
                                   "Wildfire hazard potential")]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows; ",
        sum(!is.na(out$`Wildfire hazard potential`)), " computed)")

cat("\n=== Wildfire hazard potential distribution ===\n")
print(summary(out$`Wildfire hazard potential`))
cat("\nTop 10 highest WHP:\n")
print(head(out[order(-out$`Wildfire hazard potential`), ], 10))
cat("\nBottom 10 lowest WHP:\n")
print(head(out[order(out$`Wildfire hazard potential`), ], 10))
