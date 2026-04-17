# Prepare the California HUC10 boundary layer for the prioritization tool.
#
# Reads the USGS CA HUC10 shapefile, reprojects to WGS84, simplifies
# with Visvalingam, and writes to data/kmp_huc10.geojson -- overwriting
# the KMP-only subset. This quietly expands the tool's coverage to any
# HUC10 in California without changing any app-level behavior: users
# uploading a CSV with non-KMP HUC10s will now see their watersheds
# render on the map.
#
# Run from the project root:
#   Rscript scripts/prepare_ca_huc10.R

library(sf)
library(rmapshaper)

src <- "Z:/GIS Data/Vector Data/WatershedBoundaries/watersheds_HUC10_CA/WBD_USGS_HUC10_CA.shp"
out <- "data/kmp_huc10.geojson"

x <- st_read(src, quiet = TRUE) |> st_transform(4326)
cat(sprintf("Source:   %d features, %d vertices\n",
            nrow(x),
            sum(sapply(st_geometry(x), function(g) nrow(st_coordinates(g))))))

# Keep only the columns the app expects; normalize names to lowercase.
x <- x[, c("HUC10", "Name", "AreaSqKm")]
names(x) <- c("huc10", "name", "areasqkm", "geometry")

# Visvalingam simplify. keep = 0.04 is aggressive but keeps HUC shapes
# recognizable at web-map zooms. keep_shapes = TRUE preserves small
# coastal or inland-basin features that would otherwise collapse.
x_simp <- ms_simplify(x, keep = 0.04, method = "vis", keep_shapes = TRUE)
cat(sprintf("Simplified: %d vertices (keep = 0.04)\n",
            sum(sapply(st_geometry(x_simp), function(g) nrow(st_coordinates(g))))))

st_write(x_simp, out, delete_dsn = TRUE, quiet = TRUE)
size_mb <- round(file.info(out)$size / 1024 / 1024, 2)
cat(sprintf("Wrote %s (%.2f MB, %d features)\n", out, size_mb, nrow(x_simp)))
