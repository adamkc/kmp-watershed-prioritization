# Generate a simplified California + Oregon state-outline GeoJSON
# for use as the inset ("you are here") map in the Report tab.
#
# Pulls state polygons from the 'maps' package (offline / no internet
# at runtime) and simplifies enough to be tiny while still recognizable.
#
# Run from the project root:
#   Rscript scripts/prepare_context_states.R

library(sf)
library(maps)

# The maps-package state polygons have self-crossings that sf's default
# s2 geometry engine rejects. Disable s2 for this generation step.
sf::sf_use_s2(FALSE)

states <- maps::map("state",
                    regions = c("california", "oregon"),
                    plot = FALSE, fill = TRUE) |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326) |>
  sf::st_simplify(dTolerance = 0.02, preserveTopology = TRUE)

out <- "data/context_states.geojson"
sf::st_write(states, out, delete_dsn = TRUE, quiet = TRUE)

size_kb <- round(file.info(out)$size / 1024, 1)
cat(sprintf("Wrote %s (%.1f KB, %d state(s))\n",
            out, size_kb, nrow(states)))
