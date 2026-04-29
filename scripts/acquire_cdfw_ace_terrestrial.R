# Acquire CDFW ACE Terrestrial Climate Resilience (BIOS ds2738) and
# Terrestrial Connectivity (BIOS ds2734) hex datasets, and compute
# per-HUC10 area-weighted means.
#
# Adds columns to the per-HUC10 frame:
#   * Climate refugia score          -- area-weighted mean of VegRefugiaScore
#                                       across hexes overlapping each HUC10
#   * Terrestrial connectivity rank  -- area-weighted mean of Connectivity_rank
#
# Source: CDFW ACE v3.0 / v3.2.3 hex-grid summaries. Both datasets cover
# the state in ~64,000 hexagonal cells. Direct paginated pull via the
# ArcGIS Feature Service, with a server-side spatial filter to the KMP
# bounding box (reduces ~64k -> ~24k hexes per dataset, ~12 pages each).
#
# Roll-up: each HUC10 is intersected with the hex layer; intersection
# areas weight the per-hex score, summed and divided by HUC10 area.
# CDFW datasets cover CA only -- 7 OR HUCs in region 1710 fall outside
# the dataset and are written as NA.
#
# Output: data/source/cdfw_ace/huc10_terrestrial_climate.csv

suppressPackageStartupMessages({
  library(sf)
  library(httr)
  library(readr)
})

sf::sf_use_s2(FALSE)

SRC_DIR    <- "data/source/cdfw_ace"
OUT_CSV    <- file.path(SRC_DIR, "huc10_terrestrial_climate.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
PLANAR_CRS <- 5070
PAGE_SIZE  <- 2000

# Service URLs (layer 0)
RESILIENCE_URL <- "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2738_fpu/FeatureServer/0"
CONNECT_URL    <- "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2734_fpu/FeatureServer/0"

dir.create(SRC_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. HUCs in 4326 (for bbox) and in EPSG:5070 (for area math) -------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
huc_all <- read_sf(HUC10_FILE)
huc10_4326 <- huc_all[huc_all$huc10 %in% metrics$huccode, ]
huc10_4326 <- st_zm(huc10_4326, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10_4326, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
huc10$huc_area_m2 <- as.numeric(st_area(huc10))
message("HUC10s: ", nrow(huc10))

# Bbox for the server-side filter (a touch of buffer)
bb <- st_bbox(huc10_4326)
bb <- bb + c(-0.2, -0.2, 0.2, 0.2)

# ---- 2. Paginated GeoJSON pull helper ----------------------------------

pull_paged_geojson <- function(layer_url, out_fields, bbox4326) {
  geom_str <- sprintf('{"xmin":%g,"ymin":%g,"xmax":%g,"ymax":%g}',
                      bbox4326["xmin"], bbox4326["ymin"],
                      bbox4326["xmax"], bbox4326["ymax"])
  pages <- list()
  offset <- 0
  repeat {
    message("  fetching offset=", offset, " ...")
    q <- list(
      where             = "1=1",
      outFields         = paste(out_fields, collapse = ","),
      geometry          = geom_str,
      geometryType      = "esriGeometryEnvelope",
      inSR              = 4326,
      spatialRel        = "esriSpatialRelIntersects",
      resultOffset      = offset,
      resultRecordCount = PAGE_SIZE,
      outSR             = 4326,
      f                 = "geojson"
    )
    r <- GET(paste0(layer_url, "/query"), query = q, timeout(120))
    stop_for_status(r)
    txt <- content(r, "text", encoding = "UTF-8")
    if (grepl('"features"\\s*:\\s*\\[\\s*\\]', txt)) break
    sf_page <- read_sf(txt)
    if (nrow(sf_page) == 0) break
    pages[[length(pages) + 1]] <- sf_page
    if (nrow(sf_page) < PAGE_SIZE) break
    offset <- offset + PAGE_SIZE
    if (offset > 200000) stop("Aborting: > 200k records, sanity bound hit")
  }
  if (length(pages) == 0) stop("No features returned")
  do.call(rbind, pages)
}

# ---- 3. Generic hex -> HUC10 area-weighted aggregation ----------------

# Returns a vector aligned to huc10$huc10 with the area-weighted mean of
# `value_col` from `hex_sf` over each HUC10.
aw_mean <- function(hex_sf, value_col) {
  hex_sf <- st_zm(hex_sf, drop = TRUE, what = "ZM")
  hex_sf <- st_transform(hex_sf, PLANAR_CRS)
  hex_sf <- st_make_valid(hex_sf)
  hex_sf$.val <- as.numeric(hex_sf[[value_col]])

  inter <- st_intersection(hex_sf[, ".val"],
                           huc10[, c("huc10", "huc_area_m2")])
  inter$area_m2 <- as.numeric(st_area(inter))
  d <- st_drop_geometry(inter)
  agg_num <- aggregate(I(.val * area_m2) ~ huc10, data = d, FUN = sum)
  agg_den <- aggregate(area_m2 ~ huc10, data = d, FUN = sum)
  agg <- merge(agg_num, agg_den, by = "huc10")
  names(agg)[2] <- "weighted_sum"
  agg$mean <- agg$weighted_sum / agg$area_m2
  agg$mean[match(huc10$huc10, agg$huc10)]
}

# ---- 4. Pull ds2738 + compute Climate refugia score --------------------

message("Pulling ds2738 (Terrestrial Climate Resilience) ...")
hex_res <- pull_paged_geojson(RESILIENCE_URL,
                              c("Hex_ID", "CLIM_RANK", "VegRefugiaScore"),
                              bb)
message("ds2738 hexes: ", nrow(hex_res))
write_csv(st_drop_geometry(hex_res),
          file.path(SRC_DIR, "terrestrial_resilience_hex_attrs.csv"))

message("Computing area-weighted mean per HUC10 ...")
huc10$`Climate refugia score` <- aw_mean(hex_res, "VegRefugiaScore")

# ---- 5. Pull ds2734 + compute Terrestrial connectivity rank -----------

message("Pulling ds2734 (Terrestrial Connectivity) ...")
hex_con <- pull_paged_geojson(CONNECT_URL,
                              c("Hex_ID", "Connectivity_rank"),
                              bb)
message("ds2734 hexes: ", nrow(hex_con))
write_csv(st_drop_geometry(hex_con),
          file.path(SRC_DIR, "terrestrial_connectivity_hex_attrs.csv"))

message("Computing area-weighted mean per HUC10 ...")
huc10$`Terrestrial connectivity rank` <- aw_mean(hex_con, "Connectivity_rank")

# ---- 6. Write output ---------------------------------------------------

out <- st_drop_geometry(huc10)[, c("huc10", "name",
                                   "Climate refugia score",
                                   "Terrestrial connectivity rank")]
names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Climate refugia score ===\n")
print(summary(out$`Climate refugia score`))
cat("\n=== Terrestrial connectivity rank ===\n")
print(summary(out$`Terrestrial connectivity rank`))

cat("\nTop 10 by Climate refugia score:\n")
print(head(out[order(-out$`Climate refugia score`), ], 10))
cat("\nTop 10 by Terrestrial connectivity rank:\n")
print(head(out[order(-out$`Terrestrial connectivity rank`), ], 10))
