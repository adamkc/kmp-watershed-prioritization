# Acquire USFS Administrative Forest Boundaries for the KMP zone.
#
# Downloads the national-forest polygons that intersect the KMP bounding
# box from the USFS EDW ArcGIS service and writes them to
# data/source/usfs_forests/kmp_forests_raw.geojson. That file feeds
# scripts/prepare_subzone_groupings.R, which computes per-HUC10 forest
# membership for the app's Step-1 "By national forest" selector.
#
# Source: USFS EDW "Administrative Forest Boundaries - National Extent"
#   https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer/0
#   Field of interest: forestname.
#
# Run: Rscript scripts/acquire_usfs_forests.R
#
# Uses curl (system2) for the fetch -- GDAL /vsicurl and download.file
# proved unreliable in some shells here, and curl ships with modern
# Windows and git.

suppressPackageStartupMessages(library(sf))

OUT_DIR  <- "data/source/usfs_forests"
OUT_FILE <- file.path(OUT_DIR, "kmp_forests_raw.geojson")
BOUNDARY <- "data/kmp_boundary.geojson"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# KMP bounding box in WGS84 (xmin,ymin,xmax,ymax) with a small pad so
# forests grazing the edge are still captured.
b   <- st_bbox(st_transform(st_read(BOUNDARY, quiet = TRUE), 4326))
pad <- 0.05
env <- sprintf("%.5f,%.5f,%.5f,%.5f",
               b[["xmin"]] - pad, b[["ymin"]] - pad,
               b[["xmax"]] + pad, b[["ymax"]] + pad)

base <- paste0("https://apps.fs.usda.gov/arcx/rest/services/EDW/",
               "EDW_ForestSystemBoundaries_01/MapServer/0/query")
query <- paste0(
  base,
  "?where=1%3D1",
  "&geometry=", utils::URLencode(env, reserved = TRUE),
  "&geometryType=esriGeometryEnvelope",
  "&inSR=4326&spatialRel=esriSpatialRelIntersects",
  "&outFields=forestname%2Cregion%2Cforestnumber",
  "&returnGeometry=true&outSR=4326&f=geojson"
)

message("Fetching USFS forests intersecting KMP bbox ...")
status <- system2("curl", c("-s", "-m", "120", "-o", shQuote(OUT_FILE),
                            shQuote(query)))
if (status != 0) stop("curl failed (exit ", status, ").")

f <- st_read(OUT_FILE, quiet = TRUE)
if (nrow(f) == 0) stop("No features returned -- check the service / bbox.")
message("Wrote ", OUT_FILE, " (", nrow(f), " forests):")
print(sort(f$forestname))
