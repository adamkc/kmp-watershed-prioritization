# Build a demonstration custom-metric CSV for the 60 Sierra Nevada HUC10
# watersheds used in the lost-meadow model paper.
#
# Output: data/examples/sn60_lost_meadows_demo.csv
#   huccode, name, and four metrics:
#     Known meadow area (acres)  - SNMMPC v2 mapped meadows, summed per HUC10
#     Lost meadow area (acres)   - SN60 model predicted lost meadows
#     Unburned 30yr (%)          - complement of the 30-yr MTBS burn union
#     Unburned 5yr (%)           - complement of the  5-yr MTBS burn union
#
# The CSV is ready to drop into the app via Step 2 -> "upload a CSV".
#
# Run: Rscript scripts/prepare_sn60_demo.R
#
# Inputs (machine-specific; edit the paths below if they move):
#   - LOST_GPKG : per-HUC10 predicted lost-meadow polygons (defines the 60).
#                 One layer per HUC10, "<HUC10>_60global_meadowpreds",
#                 fields HUC10/UID/Area_ha (CRS EPSG:6414, m).
#   - MEADOW_SHP: SNMMPC v2 mapped-meadow polygons, field AREA_ACRE + HUC12.
#   - HUC10 boundaries come from the bundled all-CA layer.
#   - MTBS perimeters come from the local cache created by
#     scripts/acquire_mtbs.R (data/source/mtbs/perims/).

suppressPackageStartupMessages({
  library(sf)
  library(readr)
})
sf::sf_use_s2(FALSE)

LOST_GPKG   <- "C:/Users/adamk/Downloads/LostMeadowsPredictions_SN60Model.gpkg"
MEADOW_SHP  <- "Z:/GIS Data/Vector Data/Meadows/SNMMPC_v2.shp"
HUC10_CA    <- "data/kmp_huc10_ca.geojson"   # 1,128 CA HUC10s (local-only)
MTBS_SHP    <- "data/source/mtbs/perims/mtbs_perims_DD.shp"
OUT_CSV     <- "data/examples/sn60_lost_meadows_demo.csv"

PLANAR_CRS  <- 5070            # CONUS Albers equal-area (m) for area math
HA_TO_ACRE  <- 2.4710538
M2_TO_ACRE  <- 0.000247105

TODAY    <- Sys.Date()
CUT_30   <- TODAY - 365.25 * 30
CUT_5    <- TODAY - 365.25 * 5

# ---- 1. The 60 target HUC10s (from the gpkg layer names) -------------------

layers  <- st_layers(LOST_GPKG)$name
targets <- sub("_60global_meadowpreds$", "", layers)
stopifnot(length(targets) == 60, all(nchar(targets) == 10))
message("Target HUC10s: ", length(targets))

# ---- 2. HUC10 boundaries + area -------------------------------------------

huc <- st_read(HUC10_CA, quiet = TRUE)
huc <- huc[huc$huc10 %in% targets, ]
if (nrow(huc) != 60) stop("Only ", nrow(huc), "/60 HUC10s found in ", HUC10_CA)
huc <- st_transform(st_make_valid(st_zm(huc, drop = TRUE)), PLANAR_CRS)
huc$huc_area_m2 <- as.numeric(st_area(huc))

# ---- 3. Known meadow acreage (SNMMPC, summed by HUC10 from HUC12) ----------

md <- st_drop_geometry(st_read(MEADOW_SHP, quiet = TRUE))
md$huc10 <- substr(as.character(md$HUC12), 1, 10)
known <- aggregate(AREA_ACRE ~ huc10, data = md[md$huc10 %in% targets, ], FUN = sum)
message("Known-meadow HUC10s with data: ", nrow(known), "/60")

# ---- 4. Lost meadow acreage (SN60 model, sum Area_ha per layer) ------------

lost <- data.frame(
  huc10 = targets,
  lost_ac = vapply(targets, function(t) {
    a <- st_read(LOST_GPKG, layer = paste0(t, "_60global_meadowpreds"),
                 quiet = TRUE)
    sum(a$Area_ha, na.rm = TRUE) * HA_TO_ACRE
  }, numeric(1)),
  row.names = NULL
)

# ---- 5. Fire: % unburned in 30yr and 5yr (MTBS union complement) -----------

# Read only perimeters intersecting the SN60 bbox (native CRS is geographic).
bb   <- st_bbox(st_transform(st_buffer(st_transform(huc, PLANAR_CRS), 5000), 4326))
wkt  <- st_as_text(st_as_sfc(bb))
perims <- st_read(MTBS_SHP, wkt_filter = wkt, quiet = TRUE)
message("MTBS perimeters in SN60 bbox: ", nrow(perims))

date_col <- names(perims)[tolower(names(perims)) %in%
                          c("ig_date", "firedate", "startdate")][1]
if (is.na(date_col)) stop("No MTBS date column found: ",
                          paste(names(perims), collapse = ", "))
perims$.d <- as.Date(perims[[date_col]])
perims <- perims[!is.na(perims$.d), ]
message("MTBS date range: ", min(perims$.d), " to ", max(perims$.d),
        "  (5-yr window starts ", CUT_5, ")")
perims <- st_transform(st_make_valid(st_zm(perims, drop = TRUE)), PLANAR_CRS)

burned_pct <- function(fires) {
  if (nrow(fires) == 0) return(setNames(rep(0, nrow(huc)), huc$huc10))
  u <- st_make_valid(st_union(fires))
  inter <- st_intersection(huc[, "huc10"], u)
  inter$a <- as.numeric(st_area(inter))
  agg <- aggregate(a ~ huc10, data = st_drop_geometry(inter), FUN = sum)
  out <- setNames(rep(0, nrow(huc)), huc$huc10)
  out[agg$huc10] <- agg$a
  100 * out / huc$huc_area_m2[match(names(out), huc$huc10)]
}

message("Computing 30-yr burn union ...")
burn30 <- burned_pct(perims[perims$.d >= CUT_30, ])
message("Computing  5-yr burn union ...")
burn5  <- burned_pct(perims[perims$.d >= CUT_5, ])

# ---- 6. Assemble + write ---------------------------------------------------

out <- st_drop_geometry(huc)[, c("huc10", "name")]
out$`Known meadow area (acres)` <- round(
  known$AREA_ACRE[match(out$huc10, known$huc10)], 1)
out$`Known meadow area (acres)`[is.na(out$`Known meadow area (acres)`)] <- 0
out$`Lost meadow area (acres)`  <- round(
  lost$lost_ac[match(out$huc10, lost$huc10)], 1)
out$`Unburned 30yr (%)` <- round(pmax(100 - burn30[match(out$huc10, names(burn30))], 0), 1)
out$`Unburned 5yr (%)`  <- round(pmax(100 - burn5[ match(out$huc10, names(burn5))],  0), 1)

names(out)[1:2] <- c("huccode", "name")
out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("\nWrote ", OUT_CSV, " (", nrow(out), " HUC10 rows)")

cat("\n=== column summaries ===\n")
print(summary(out[, 3:6]))
cat("\n=== head ===\n")
print(utils::head(out, 6))
