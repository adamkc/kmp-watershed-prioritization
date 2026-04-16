# Regenerate per-level HUC GeoJSONs from the source GPKG.
# Run from the project root:
#   Rscript scripts/split_huc_layers.R
# Re-run whenever KMP boundary source is updated.

library(sf)

src <- "C:/Users/adamk/Documents/Work/Klamath Meadows Partnership/OutputSpatialDatasets/KMP_HUC_Boundaries_Simplified.gpkg"
out_dir <- "data"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

layers <- st_layers(src)$name
cat("Layers found:", paste(layers, collapse = ", "), "\n\n")

for (lyr in layers) {
  x <- st_read(src, layer = lyr, quiet = TRUE)

  # Normalize column names to lowercase
  names(x) <- tolower(names(x))

  # Reproject to WGS84 if needed
  if (is.na(st_crs(x)) || st_crs(x)$epsg != 4326) {
    x <- st_transform(x, 4326)
  }

  # Derive HUC level from layer name (e.g., "WBDHU10" -> "huc10")
  # Fallback: use layer name lowercased
  level <- regmatches(lyr, regexpr("[0-9]+", lyr))
  out_name <- if (length(level) && nchar(level)) {
    paste0("kmp_huc", level, ".geojson")
  } else {
    paste0("kmp_", tolower(lyr), ".geojson")
  }

  out_path <- file.path(out_dir, out_name)
  st_write(x, out_path, delete_dsn = TRUE, quiet = TRUE)

  size_kb <- round(file.info(out_path)$size / 1024, 1)
  cat(sprintf(
    "  %-30s  %4d features  %7.1f KB  cols: %s\n",
    out_name, nrow(x), size_kb, paste(names(x), collapse = ", ")
  ))
}

cat("\nDone. Outputs in:", out_dir, "\n")
