# Simplify the KMP zone boundary shapefile into a small, smooth GeoJSON
# suitable for display as a static overlay in the Shiny app.
#
# Run from the project root:
#   Rscript scripts/prepare_kmp_boundary.R

library(sf)
library(rmapshaper)

src <- "C:/Users/adamk/Documents/Work/Klamath Meadows Partnership/SpatialDatasets/KMP_Boundary_July2022_DRAFT.shp"
out <- "data/kmp_boundary.geojson"

b <- st_read(src, quiet = TRUE) |> st_transform(4326)
cat(sprintf("Source: %d feature(s), %d vertices\n",
            nrow(b), sum(sapply(st_geometry(b), function(g) nrow(st_coordinates(g))))))

# Visvalingam-Whyatt simplification, keeping 2% of vertices.
# keep_shapes = TRUE so small islands survive even at aggressive
# simplification levels.
b_simp <- ms_simplify(b, keep = 0.02, method = "vis", keep_shapes = TRUE)

cat(sprintf("Simplified: %d vertices (keep = 0.02, Visvalingam)\n",
            sum(sapply(st_geometry(b_simp), function(g) nrow(st_coordinates(g))))))

st_write(b_simp, out, delete_dsn = TRUE, quiet = TRUE)

size_kb <- round(file.info(out)$size / 1024, 1)
cat(sprintf("Wrote %s (%.1f KB)\n", out, size_kb))
