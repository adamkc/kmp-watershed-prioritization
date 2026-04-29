# Acquire and process USFS Landscape-scale Meadow Model (LMM) predictions.
# Computes per-HUC10:
#
#   * Lost meadow potential (% area) -- fraction of each HUC10 covered by
#     LMM-predicted meadow polygons (areas modelled as having historical
#     wet-meadow character that may have shifted to drier systems).
#
# Source: USFS LMM, manually placed at
# `data/source/lmm/lmm_predictions.geojson`. The source file packs all
# polygons into a single Feature whose geometry is a GeometryCollection
# nested two deep (top GeometryCollection -> inner GeometryCollection ->
# MultiPolygon with 24,000+ parts plus stray MultiLineStrings). Neither
# `st_collection_extract()` nor `ogr2ogr -explodecollections` recurses,
# so this script walks the JSON manually to extract the inner
# MultiPolygon, then proceeds with a standard intersect/area workflow.
#
# Output: data/source/lmm/huc10_lmm.csv

suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
  library(readr)
})

sf::sf_use_s2(FALSE)

SRC_DIR    <- "data/source/lmm"
RAW_GEOJSON  <- file.path(SRC_DIR, "lmm_predictions.geojson")
CLEAN_GEOJSON <- file.path(SRC_DIR, "lmm_clean.geojson")
OUT_CSV    <- file.path(SRC_DIR, "huc10_lmm.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070

# ---- 1. Walk the nested GeometryCollection, extract MultiPolygon -------

if (!file.exists(CLEAN_GEOJSON)) {
  message("Unwrapping nested GeometryCollection ...")
  j <- jsonlite::fromJSON(RAW_GEOJSON, simplifyVector = FALSE)

  walk_for_mp <- function(g) {
    if (g$type == "MultiPolygon" || g$type == "Polygon") return(g)
    if (g$type == "GeometryCollection") {
      for (child in g$geometries) {
        out <- walk_for_mp(child)
        if (!is.null(out)) return(out)
      }
    }
    NULL
  }
  mp_geom <- walk_for_mp(j$features[[1]]$geometry)
  if (is.null(mp_geom)) stop("No (Multi)Polygon found in LMM geojson")

  clean <- list(
    type = "FeatureCollection",
    crs = list(type = "name",
               properties = list(name = "urn:ogc:def:crs:OGC:1.3:CRS84")),
    features = list(list(
      type = "Feature",
      properties = list(),
      geometry = mp_geom
    ))
  )
  writeLines(jsonlite::toJSON(clean, auto_unbox = TRUE, digits = 7),
             CLEAN_GEOJSON)
  message("Wrote clean geojson: ", CLEAN_GEOJSON)
} else {
  message("Clean geojson already present: ", CLEAN_GEOJSON)
}

# ---- 2. Load & cast meadow polygons -------------------------------------

m <- read_sf(CLEAN_GEOJSON)
message("Read ", nrow(m), " feature(s); geometry: ",
        as.character(st_geometry_type(m)))
m <- st_zm(m, drop = TRUE, what = "ZM")
m <- st_cast(m, "POLYGON", warn = FALSE)
message("Cast to ", nrow(m), " individual polygons")
m <- st_transform(m, PLANAR_CRS)
m <- st_make_valid(m)

# ---- 3. HUC10s -----------------------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% metrics$huccode, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
huc10$huc_area_m2 <- as.numeric(st_area(huc10))
message("HUC10s: ", nrow(huc10))

# ---- 4. Intersect meadow polygons with HUC10 ---------------------------

message("Intersecting LMM polygons with HUC10s (",
        nrow(m), " x ", nrow(huc10), ") ...")
t0 <- Sys.time()
inter <- st_intersection(m, huc10[, c("huc10", "huc_area_m2")])
message("  produced ", nrow(inter), " pieces in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s")
inter$area_m2 <- as.numeric(st_area(inter))

agg <- aggregate(area_m2 ~ huc10, data = st_drop_geometry(inter), FUN = sum)
huc10 <- merge(huc10, agg, by = "huc10", all.x = TRUE)
huc10$lmm_area_m2 <- ifelse(is.na(huc10$area_m2), 0, huc10$area_m2)
huc10$`Lost meadow potential (% area)` <-
  pmin(100 * huc10$lmm_area_m2 / huc10$huc_area_m2, 100)

# ---- 5. Write output ----------------------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name", "huc_area_m2",
                                   "lmm_area_m2",
                                   "Lost meadow potential (% area)")]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Lost meadow potential (% area) ===\n")
print(summary(out$`Lost meadow potential (% area)`))
cat("\nTop 10 by lost-meadow potential:\n")
print(head(out[order(-out$`Lost meadow potential (% area)`),
              c("huccode", "name", "Lost meadow potential (% area)")], 10))
