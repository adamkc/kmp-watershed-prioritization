# Acquire and process the KMP Meadow Inventory (`merged_meadows_20260408.gpkg`).
# Computes per-HUC10:
#
#   * Meadow density (% area) -- fraction of each HUC10 area covered by
#                                inventoried KMP meadow polygons.
#
# Source: Klamath Meadows Partnership in-house meadow polygon compilation
# (manually placed under `data/source/kmp-inventory/`). 6,041 polygons,
# all classified `HGM1 = "Riparian Low Gradient"`. CRS: NAD83 / UTM 10N
# (EPSG:26910). Authoritative within the KMP zone (preferred over CDFW
# ACE meadows / USFS LMM where coverage exists).
#
# Note: this is the *actual* meadow inventory, distinct from
# `Lost meadow potential` (LMM model output of where meadows COULD be).
# Both can coexist in the metric set.
#
# Output: data/source/kmp-inventory/huc10_kmp_meadow_density.csv

suppressPackageStartupMessages({
  library(sf)
  library(readr)
})

sf::sf_use_s2(FALSE)

GPKG       <- "data/source/kmp-inventory/merged_meadows_20260408.gpkg"
SRC_DIR    <- "data/source/kmp-inventory"
OUT_CSV    <- file.path(SRC_DIR, "huc10_kmp_meadow_density.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070  # CONUS Albers Equal Area, m

# ---- 1. Load and reproject meadow polygons ----------------------------

m <- read_sf(GPKG)
message("Meadow polygons: ", nrow(m), "  (CRS: ",
        st_crs(m)$input, ")")
m <- st_zm(m, drop = TRUE, what = "ZM")
m <- st_transform(m, PLANAR_CRS)
m <- st_make_valid(m)
m_total_km2 <- sum(as.numeric(st_area(m))) / 1e6
message(sprintf("Total meadow area: %.1f km^2", m_total_km2))

# ---- 2. Load HUC10s ---------------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
huc10$huc_area_m2 <- as.numeric(st_area(huc10))
message("HUC10s: ", nrow(huc10))

# ---- 3. Intersect, sum meadow area per HUC10 -------------------------

message("Intersecting meadow polygons with HUC10s (",
        nrow(m), " x ", nrow(huc10), ") ...")
t0 <- Sys.time()
inter <- st_intersection(m, huc10[, c("huc10", "huc_area_m2")])
message(sprintf("  produced %d pieces in %.1fs",
                nrow(inter),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
inter$area_m2 <- as.numeric(st_area(inter))

agg <- aggregate(area_m2 ~ huc10, data = st_drop_geometry(inter), FUN = sum)
huc10 <- merge(huc10, agg, by = "huc10", all.x = TRUE)
huc10$meadow_area_m2 <- ifelse(is.na(huc10$area_m2), 0, huc10$area_m2)
huc10$`Meadow density (% area)` <-
  pmin(100 * huc10$meadow_area_m2 / huc10$huc_area_m2, 100)

# ---- 4. Output ---------------------------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name", "huc_area_m2",
                                   "meadow_area_m2",
                                   "Meadow density (% area)")]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Meadow density (% area) ===\n")
print(summary(out$`Meadow density (% area)`))
cat("\nTop 10 by meadow density:\n")
print(head(out[order(-out$`Meadow density (% area)`),
              c("huccode", "name", "Meadow density (% area)")], 10))
cat("\nHUCs with 0 meadows (showing 5):\n")
zero <- out[out$`Meadow density (% area)` == 0, ]
cat("  count:", nrow(zero), " of ", nrow(out), "\n")
print(head(zero[, c("huccode", "name")], 5))
