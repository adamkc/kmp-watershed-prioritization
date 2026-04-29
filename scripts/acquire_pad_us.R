# Acquire and process PAD-US 4.1 (Protected Areas Database)
# Computes per-HUC10 federal ownership percentage from the Fee feature class.
#
# Output: data/source/pad_us/huc10_federal_ownership.csv
#   columns: huccode, name, huc_area_m2, fed_area_m2, federal_ownership_pct
#
# Source: USGS PAD-US 4.1, ScienceBase item 6759abcfd34edfeb8710a004
# https://www.sciencebase.gov/catalog/item/6759abcfd34edfeb8710a004
#
# CA-only: ScienceBase requires authentication for programmatic downloads,
# so the user manually downloaded `PADUS4_1_State_CA_GDB_KMZ.zip` and
# unzipped it under `data/source/pad_us/`. Oregon is not covered, so the
# 7 HUCs in HUC4 region 1710 are written as NA.
#
# Run: Rscript scripts/acquire_pad_us.R

suppressPackageStartupMessages({
  library(sf)
  library(readr)
})

sf::sf_use_s2(FALSE)

SRC_DIR    <- "data/source/pad_us"
GDB_PATH   <- file.path(SRC_DIR, "PADUS4_1_State_CA_GDB_KMZ",
                        "PADUS4_1_StateCA.gdb")
OUT_CSV    <- file.path(SRC_DIR, "huc10_federal_ownership.csv")
HUC10_FILE <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"

# Federal Mang_Type code in PAD-US (covers all federal managers:
# USFS, BLM, NPS, FWS, DOD, DOE, BIA, BOR, USACE, OTHF, etc.)
FED_CODE <- "FED"

# HUC4 prefixes covered by the CA-only PAD-US download. HUCs whose huccode
# starts with another prefix (e.g. "1710" = Oregon Closed Basins) cannot
# be reliably computed and are written as NA.
CA_HUC4_PREFIXES <- c("1801", "1802", "1803", "1804", "1805", "1806",
                      "1807", "1808", "1809", "1810")

# Equal-area CRS for area math (NAD83 / CONUS Albers Equal Area, meters)
PLANAR_CRS <- 5070

if (!dir.exists(GDB_PATH)) {
  stop("Expected PAD-US GDB at ", GDB_PATH, " -- did the manual download ",
       "land somewhere else?")
}

# ---- 1. Read the Fee feature class and filter to federal ----------------

layers <- st_layers(GDB_PATH)$name
message("Layers in GDB: ", paste(layers, collapse = ", "))

fee_layer <- grep("Fee", layers, value = TRUE)
fee_layer <- fee_layer[!grepl("Designation|Easement|Combined|Marine|Proclamation",
                              fee_layer)]
if (length(fee_layer) == 0) stop("No Fee layer found in ", GDB_PATH)
fee_layer <- fee_layer[1]
message("Reading layer: ", fee_layer)

# Some PAD-US polygons are stored as WKB type 12 (MultiSurface, i.e. curve
# polygons) which GEOS cannot process. Use gdal_utils() to translate the
# Fee layer to GeoPackage with -nlt MULTIPOLYGON, which linearises curves
# into standard polygon geometry. Also filter to FED at GDAL level to
# avoid loading 19,000 features when we only need ~300.
fed_gpkg <- tempfile(fileext = ".gpkg")
sf::gdal_utils(
  util = "vectortranslate",
  source = GDB_PATH,
  destination = fed_gpkg,
  options = c(
    "-f", "GPKG",
    "-nlt", "MULTIPOLYGON",
    "-dim", "XY",
    "-where", paste0("Mang_Type = '", FED_CODE, "'"),
    fee_layer
  )
)

fed <- read_sf(fed_gpkg)
file.remove(fed_gpkg)
message("Federal features (GDAL-filtered): ", nrow(fed))
message("Federal by Mang_Name:")
print(table(fed$Mang_Name, useNA = "ifany"))
message("Geometry types: ",
        paste(unique(as.character(st_geometry_type(fed))), collapse = ", "))

fed <- fed[, "Mang_Type"]
fed <- st_transform(fed, PLANAR_CRS)
fed <- st_make_valid(fed)

# ---- 2. Load HUC10 polygons restricted to those in the metrics CSV ------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
target_hucs <- metrics$huccode

huc10 <- read_sf(HUC10_FILE)
huc10 <- huc10[huc10$huc10 %in% target_hucs, ]
huc10 <- st_zm(huc10, drop = TRUE, what = "ZM")
huc10 <- st_transform(huc10, PLANAR_CRS)
huc10 <- st_make_valid(huc10)
huc10$huc_area_m2 <- as.numeric(st_area(huc10))
message("HUC10 features matched: ", nrow(huc10), " / ", length(target_hucs))

missing_hucs <- setdiff(target_hucs, huc10$huc10)
if (length(missing_hucs) > 0) {
  warning("HUCs in metrics.csv with no boundary: ",
          paste(missing_hucs, collapse = ", "))
}

# Split into CA-coverable and OR (uncomputable, will be NA)
ca_hucs <- huc10[substr(huc10$huc10, 1, 4) %in% CA_HUC4_PREFIXES, ]
or_hucs <- huc10[!substr(huc10$huc10, 1, 4) %in% CA_HUC4_PREFIXES, ]
message("HUCs covered by CA PAD-US: ", nrow(ca_hucs))
message("HUCs in OR / outside CA (will be NA): ", nrow(or_hucs))
if (nrow(or_hucs) > 0) {
  message("  uncovered HUC4 prefixes: ",
          paste(unique(substr(or_hucs$huc10, 1, 4)), collapse = ", "))
}

# ---- 3. Intersect federal lands with the CA HUC10s ----------------------

message("Intersecting federal polygons with CA HUC10s (",
        nrow(fed), " x ", nrow(ca_hucs), ") ...")
t0 <- Sys.time()
inter <- st_intersection(fed, ca_hucs[, c("huc10", "huc_area_m2")])
message("  intersection produced ", nrow(inter), " pieces in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s")
inter$area_m2 <- as.numeric(st_area(inter))

fed_area_per_huc <- aggregate(area_m2 ~ huc10, data = st_drop_geometry(inter),
                              FUN = sum)
ca_hucs <- merge(ca_hucs, fed_area_per_huc, by = "huc10", all.x = TRUE)
ca_hucs$fed_area_m2 <- ifelse(is.na(ca_hucs$area_m2), 0, ca_hucs$area_m2)
ca_hucs$federal_ownership_pct <- 100 * ca_hucs$fed_area_m2 / ca_hucs$huc_area_m2
ca_hucs$federal_ownership_pct <- pmin(pmax(ca_hucs$federal_ownership_pct, 0), 100)

# ---- 4. Build output frame: CA computed + OR as NA ----------------------

ca_out <- st_drop_geometry(ca_hucs)[, c("huc10", "name",
                                        "huc_area_m2", "fed_area_m2",
                                        "federal_ownership_pct")]

if (nrow(or_hucs) > 0) {
  or_out <- st_drop_geometry(or_hucs)[, c("huc10", "name", "huc_area_m2")]
  or_out$fed_area_m2 <- NA_real_
  or_out$federal_ownership_pct <- NA_real_
  out <- rbind(ca_out, or_out)
} else {
  out <- ca_out
}

names(out)[1] <- "huccode"
out <- out[order(out$huccode), ]

write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows; ",
        sum(!is.na(out$federal_ownership_pct)), " computed, ",
        sum(is.na(out$federal_ownership_pct)), " NA)")

# ---- 5. Summary ---------------------------------------------------------

computed <- out[!is.na(out$federal_ownership_pct), ]
cat("\n=== Federal ownership % distribution across ", nrow(computed),
    " computed HUCs ===\n", sep = "")
print(summary(computed$federal_ownership_pct))
cat("\nTop 10 most-federal HUCs:\n")
print(head(computed[order(-computed$federal_ownership_pct),
                    c("huccode", "name", "federal_ownership_pct")], 10))
cat("\nBottom 10 least-federal HUCs:\n")
print(head(computed[order(computed$federal_ownership_pct),
                    c("huccode", "name", "federal_ownership_pct")], 10))
