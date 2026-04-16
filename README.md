# KMP Watershed Prioritization Tool

An interactive Shiny app for ranking HUC watersheds in the Klamath
Meadows Partnership (KMP) zone under user-selected criteria and
weight combinations. Users pick a prebuilt scenario (or build a
custom metric list), adjust metric weights with live sliders, and
see the resulting composite score as a choropleth map, a ranked
table, and a Monte Carlo sensitivity analysis.

## What it does

- Joins a tabular metrics file (CSV) to bundled KMP-zone HUC
  boundaries (HUC4 through HUC12)
- Bins each metric via Jenks natural breaks (continuous) or linear
  rescale (ordinal / low-cardinality), handling zero-inflation and
  missing values
- Applies per-metric direction so `bin 5` always means "pushes this
  HUC to the top of the ranking"
- Combines binned metrics into a weighted-mean composite score,
  producing ranks that react live to weight slider changes
- Monte Carlo sensitivity: perturb weights by a user-set percentage,
  resample 1000+ times, and report per-HUC rank stability
- Generates a shareable Markdown / HTML report of the current
  analysis state, with PDF-print styles

## Running locally

**Prerequisites:** R 4.4 or later, and the packages listed in the
header of `app.R` (`shiny`, `bslib`, `leaflet`, `sf`, `DT`,
`readr`, `classInt`, `yaml`, `ggplot2`, `scales`, `markdown`).

Install them once with:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "sf", "DT", "readr",
  "classInt", "yaml", "ggplot2", "scales", "markdown"
))
```

Launch the app from the project root:

```r
shiny::runApp()
```

Or from a terminal:

```bash
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```

## Project structure

```
app.R                        main Shiny app
R/                           analytical modules (auto-sourced)
  input.R                    read CSV, validate huccode, detect HUC level,
                             load bundled boundaries, join input to geometry
  columns.R                  classify each column as identifier, label,
                             numeric-continuous, or numeric-ordinal
  score.R                    Jenks / ordinal binning, direction inversion,
                             weighted composite, rank
  scenarios.R                load + validate data/scenarios.yaml
  metrics.R                  load + validate data/metrics.yaml
  sensitivity.R              Monte Carlo weight perturbation + summary plot
  report.R                   build Markdown report + HTML template
data/
  kmp_metrics.csv            master metrics table (currently simulated)
  scenarios.yaml             prebuilt scenario catalog
  metrics.yaml               per-metric category + direction
  kmp_boundary.geojson       KMP zone outline (simplified)
  kmp_huc{4,6,8,10,12}.geojson  bundled HUC boundaries by level
  examples/                  regression fixtures
scripts/
  split_huc_layers.R         regenerate per-level GeoJSONs from GPKG
  prepare_kmp_boundary.R     simplify source KMP shapefile to GeoJSON
  generate_simulated_metrics.R  reproducible simulated dataset
  test_*.R                   module smoke tests
```

## Customizing for your analysis

The tool is designed so non-developers can refine content without
touching code. Most changes live in two YAML files:

### Add or refine a scenario

Open `data/scenarios.yaml` and add a new entry under `scenarios:`:

```yaml
- id: your_scenario_id
  name: Your Scenario Name
  description: >
    Plain-English summary shown in the app and the report.
  weights:
    "Simulated: Recent fire (% area, 10yr)": 2
    "Simulated: Climate resiliency index": 1
    "Simulated: Lost meadow potential": 3
```

Metric names must match column names in `data/kmp_metrics.csv`
exactly. The app validates this at startup and reports missing
metric references in the Diagnostics tab.

### Add or adjust metric metadata

Open `data/metrics.yaml` to change a metric's category or direction:

```yaml
- name: "Simulated: Invasive plant extent"
  category: Restoration
  direction: negative   # low raw value -> high priority score
```

`direction: negative` inverts the metric's bin scores so "bin 5"
still means "high priority". `category` drives grouping in the
custom-metric picker modal.

### Replace the master data

Drop a new `data/kmp_metrics.csv` into place. Required schema:

- `huccode` column: plain digit strings at a single HUC level
  (4, 6, 8, 10, or 12 digits). No scientific notation.
- Optional `name` column for display labels.
- Any number of numeric metric columns.

The app auto-detects HUC level from the code length and loads the
matching bundled boundary file. Metric metadata and scenarios that
reference unknown metric names surface in Diagnostics.

### Regenerate bundled boundaries

If HUC boundary sources change, re-run `scripts/split_huc_layers.R`.
If the KMP zone outline changes, re-run
`scripts/prepare_kmp_boundary.R`. Both write to `data/`.

## Data sources

- **HUC boundaries:** USGS Watershed Boundary Dataset (public domain).
  Bundled boundaries are Visvalingam-Whyatt simplified for web
  display; regenerate from authoritative WBD for analysis elsewhere.
- **KMP zone outline:** Klamath Meadows Partnership, draft July 2022.
- **Simulated metrics:** randomly generated for development and
  demonstration; not real observations. Column names are prefixed
  `Simulated:` so they cannot be mistaken for production data.

## Testing

Quick smoke tests for each module live in `scripts/`:

```bash
Rscript scripts/test_input.R        # CSV read + validation
Rscript scripts/test_columns.R      # column classifier
Rscript scripts/test_scenarios.R    # scenario loader + validation
Rscript scripts/test_sensitivity.R  # Monte Carlo end-to-end
```

Each prints human-readable output and exits clean on success.

## Deployment

The app is deployed to GitHub Pages as a static shinylive bundle.
The deploy runs automatically on every push to `main` via
`.github/workflows/deploy.yml` — no server or R hosting required.
Page URL: _see the GitHub repository "About" field._

## License

MIT. See [LICENSE](LICENSE).

## Acknowledgments

Developed for the Klamath Meadows Partnership working group.
HUC boundary data from the USGS Watershed Boundary Dataset.
