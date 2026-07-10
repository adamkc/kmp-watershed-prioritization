# Build the sub-zone grouping lookup used by the app's Step-1 selector.
#
# Two grouping axes are derived here (HUC6 is handled in-app from the
# huccode prefix and needs no lookup):
#
#   region  - each HUC10 assigned to ONE of three ecological regions,
#             derived from the EPA Level III ecoregion layer
#             (data/ca_eco_l3) by majority area overlap, with a
#             nearest-ecoregion fallback for HUCs whose area sits
#             outside the CA-only layer (the 7 Oregon HUCs + a few
#             CA border slivers).
#
#   forest  - national-forest membership from the USFS Administrative
#             Forest Boundaries layer. ANY-overlap / multi-membership:
#             a HUC that touches two forests gets a row for each. HUCs
#             touching no forest get a single "Outside National Forest"
#             row so they remain selectable.
#
# Output: data/huc10_groupings.csv  (long format)
#   huccode, axis ("region"|"forest"), value
#
# Run: Rscript scripts/prepare_subzone_groupings.R
#
# Inputs:
#   data/ca_eco_l3/                       EPA Level III ecoregions (CA)
#   data/kmp_huc10.geojson                KMP HUC10 polygons
#   data/kmp_metrics.csv                  master metric table (huccodes)
#   data/source/usfs_forests/kmp_forests_raw.geojson
#       USFS Administrative Forest Boundaries clipped to the KMP bbox,
#       fetched from the EDW ArcGIS service (see acquire step in the
#       data-acquisition tracker). Field: forestname.

suppressPackageStartupMessages({
  library(sf)
  library(readr)
})
sf::sf_use_s2(FALSE)

PLANAR_CRS <- 5070  # NAD83 / CONUS Albers Equal Area (meters)

ECO_DIR     <- "data/ca_eco_l3"
HUC10_FILE  <- "data/kmp_huc10.geojson"
METRICS_CSV <- "data/kmp_metrics.csv"
FOREST_FILE <- "data/source/usfs_forests/kmp_forests_raw.geojson"
OUT_CSV     <- "data/huc10_groupings.csv"

# --- Editable mapping: EPA Level III ecoregion -> KMP region ----------------
# Three distinct groupings requested: Coastal/Foothills, Klamath,
# Cascades-Modoc. The Klamath Mountains and Coast Range ecoregions map
# cleanly. The "Central California Foothills and Coastal Mountains"
# ecoregion here spans coastal-draining basins (Russian, Napa, Sonoma,
# Bay, Clear Lake) and inner-Coast-Range Sacramento tributaries, so the
# bucket is named Coastal/Foothills to cover both. The volcanic east
# (Cascades, Eastern Cascades, Modoc/Basin-and-Range) forms
# Cascades-Modoc. Edit this table to re-bucket any ecoregion.
REGION_MAP <- c(
  "Coast Range"                                         = "Coastal/Foothills",
  "Central California Foothills and Coastal Mountains"  = "Coastal/Foothills",
  "Central California Valley"                            = "Coastal/Foothills",
  "Klamath Mountains/California High North Coast Range"  = "Klamath",
  "Cascades"                                            = "Cascades-Modoc",
  "Eastern Cascades Slopes and Foothills"               = "Cascades-Modoc",
  "Sierra Nevada"                                       = "Cascades-Modoc",
  "Northern Basin and Range"                            = "Cascades-Modoc",
  "Central Basin and Range"                             = "Cascades-Modoc"
)

OUTSIDE_FOREST <- "Outside National Forest"

# ---- Load inputs -----------------------------------------------------------

met <- read_csv(METRICS_CSV, show_col_types = FALSE,
                col_types = cols(huccode = col_character()))
target <- met$huccode

huc <- st_read(HUC10_FILE, quiet = TRUE)
huc <- huc[huc$huc10 %in% target, ]
huc <- st_zm(huc, drop = TRUE, what = "ZM")
huc <- st_transform(huc, PLANAR_CRS)
huc <- st_make_valid(huc)
message("HUC10 features: ", nrow(huc), " / ", length(target))

eco <- st_read(ECO_DIR, quiet = TRUE)
eco <- st_transform(eco, PLANAR_CRS)
eco <- st_make_valid(eco)

forests <- st_read(FOREST_FILE, quiet = TRUE)
forests <- st_transform(forests, PLANAR_CRS)
forests <- st_make_valid(forests)

# ---- Region: majority-area ecoregion, nearest fallback --------------------

inter <- suppressWarnings(st_intersection(huc[, "huc10"], eco[, "US_L3NAME"]))
inter$a <- as.numeric(st_area(inter))
best <- do.call(rbind, by(
  st_drop_geometry(inter), inter$huc10,
  function(d) d[which.max(d$a), c("huc10", "US_L3NAME")]
))
huc$l3 <- best$US_L3NAME[match(huc$huc10, best$huc10)]

na_l3 <- is.na(huc$l3)
if (any(na_l3)) {
  ni <- st_nearest_feature(st_point_on_surface(huc[na_l3, ]), eco)
  huc$l3[na_l3] <- eco$US_L3NAME[ni]
  message("Nearest-ecoregion fallback used for ", sum(na_l3), " HUC(s).")
}

unmapped <- setdiff(unique(huc$l3), names(REGION_MAP))
if (length(unmapped) > 0) {
  stop("Ecoregion(s) with no REGION_MAP entry: ",
       paste(unmapped, collapse = "; "),
       ". Add them to REGION_MAP.")
}
huc$region <- unname(REGION_MAP[huc$l3])

message("\nRegion assignment:")
print(table(huc$region))

# ---- Forest: any-overlap multi-membership ---------------------------------

hits <- st_intersects(huc, forests)
forest_rows <- do.call(rbind, lapply(seq_len(nrow(huc)), function(i) {
  fs <- forests$forestname[hits[[i]]]
  fs <- unique(fs[!is.na(fs)])
  if (length(fs) == 0) fs <- OUTSIDE_FOREST
  data.frame(huccode = huc$huc10[i], axis = "forest", value = fs,
             stringsAsFactors = FALSE)
}))

message("\nForest membership (HUCs per forest; a HUC can appear twice):")
print(sort(table(forest_rows$value), decreasing = TRUE))
multi <- table(forest_rows$huccode[forest_rows$value != OUTSIDE_FOREST])
message("HUCs in >1 forest: ", sum(multi > 1))

# ---- Assemble long output --------------------------------------------------

region_rows <- data.frame(huccode = huc$huc10, axis = "region",
                          value = huc$region, stringsAsFactors = FALSE)

out <- rbind(region_rows, forest_rows)
out <- out[order(out$axis, out$value, out$huccode), ]
write_csv(out, OUT_CSV)
message("\nWrote ", OUT_CSV, " (", nrow(out), " rows: ",
        nrow(region_rows), " region, ", nrow(forest_rows), " forest)")

# ---- Diagnostic map (optional; only if ggplot2 present) -------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  huc_wgs <- st_transform(huc, 4326)
  p <- ggplot2::ggplot(huc_wgs) +
    ggplot2::geom_sf(ggplot2::aes(fill = region), color = "grey40",
                     linewidth = 0.1) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = "Region") +
    ggplot2::labs(title = "KMP HUC10s by region (ecoregion-derived)",
                  subtitle = "Edit REGION_MAP in scripts/prepare_subzone_groupings.R to re-bucket") +
    ggplot2::theme_void(base_size = 11)
  png_path <- "data/source/usfs_forests/region_map_preview.png"
  ggplot2::ggsave(png_path, p, width = 7, height = 8, dpi = 110, bg = "white")
  message("Wrote preview map: ", png_path)
}
