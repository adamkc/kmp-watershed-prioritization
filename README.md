# KMP Watershed Prioritization Tool

An interactive Shiny app for ranking HUC watersheds in the Klamath
Meadows Partnership (KMP) zone under user-selected criteria and weight
combinations. Users pick a prebuilt scenario (or build a custom metric
list, or upload a CSV), adjust metric weights with live sliders, and
see the resulting composite score as a choropleth map, a ranked table,
a Monte Carlo sensitivity analysis, and a downloadable report.

> ⚠ The deployed master data is currently **simulated**. See
> [`docs/data-acquisition.md`](docs/data-acquisition.md) for the
> tracker that drives the data team's transition from simulated to
> real metrics. The amber banner at the top of the deployed app
> stays in place until the bundled `data/kmp_metrics.csv` carries
> real values.

## What it does

- Three-step workflow in a collapsible accordion sidebar:
  1. Sub-zone selector (Full KMP or any HUC6 within the zone)
  2. Metrics source — prebuilt scenario, custom checkbox-grouped picker, or CSV upload
  3. Weight sliders (one per active metric, with hover descriptions, direction arrows, missing-value annotations, zero-inflation flags, and "All to 1 / All to 0" bulk-set buttons)
- Map tab — interactive Leaflet choropleth of composite scores with KMP zone outline, top-3 rank badges, hover tooltips, and HUC6 sub-zone preview before metrics are picked
- Ranked HUCs tab — sortable DT table of rank / huccode / name / score plus per-metric direction-adjusted bin scores, with CSV download
- Sensitivity tab — Monte Carlo weight perturbation (user-controlled uncertainty %, draws); rank distribution boxplot for top-30 HUCs; per-HUC stability table
- Report tab — live markdown report with timestamp, configuration, scenario description, metrics table, top-10 ranking, and four embedded charts (locator-inset map, ranking bar chart, faceted per-metric maps, sensitivity rank distribution); downloadable as `.md` or self-contained `.html`
- Diagnostics tab — surfaces validation issues, scenario catalog mismatches, zero-inflated columns, and missing-value counts

## Analytical pipeline (high-level)

1. **Read** the master CSV (or uploaded file) via `R/input.R` — strict validation including HUC-code scientific-notation detection, mixed-scale rejection, and auto-detection of HUC level (4 / 6 / 8 / 10 / 12) from code length
2. **Classify columns** in `R/columns.R` — identifier / label / numeric-ordinal / numeric-continuous / empty, with zero-inflation flagging
3. **Bin** each metric in `R/score.R` — Jenks natural breaks for continuous, linear rescale for low-cardinality, fallback for zero-inflated columns; bins always rescaled to cover [1, n_classes]
4. **Apply directions** so "negative-direction" metrics (e.g., invasive plant extent) get inverted bins — "bin 5" always means "pushes this HUC toward the top of the ranking"
5. **Composite** is the weighted mean of bins, computed only over metrics with non-zero weight and non-NA values
6. **Rank** descending; ties share the lower rank
7. **Sensitivity** in `R/sensitivity.R` — Monte Carlo over (1 ± p) perturbations of each active weight; report per-HUC rank distribution and P(top 10%)

## Running locally

**Prerequisites:** R 4.4 or later, plus the packages listed at the top of `app.R`.

Install once:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "sf", "DT", "readr",
  "classInt", "yaml", "ggplot2", "scales", "markdown",
  "patchwork", "base64enc"
))
```

Launch from the project root:

```r
shiny::runApp()
# or
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```

## Project structure

```
app.R                            main Shiny app
R/                               analytical modules (auto-sourced)
  input.R                        CSV read + validation + boundary join
  columns.R                      column classifier
  score.R                        Jenks / ordinal binning + direction + composite + rank
  scenarios.R                    load + validate data/scenarios.yaml
  metrics.R                      load + validate data/metrics.yaml
  sensitivity.R                  Monte Carlo + rank distribution plot
  report.R                       markdown report + chart functions + standalone HTML wrapper
data/
  kmp_metrics.csv                master metrics table (currently simulated)
  scenarios.yaml                 prebuilt scenario catalog
  metrics.yaml                   per-metric category + direction + description
  kmp_boundary.geojson           KMP zone outline (simplified)
  kmp_huc{4,6,8,10,12}.geojson   bundled HUC boundaries by level
                                   (HUC10 covers all of California; others are KMP-only)
  context_states.geojson         CA + OR outlines for the report-map locator inset
  examples/                      regression fixtures (Scott River clean + broken)
scripts/
  split_huc_layers.R             regenerate per-level KMP GeoJSONs from GPKG
  prepare_kmp_boundary.R         simplify the source KMP boundary shapefile
  prepare_ca_huc10.R             rebuild data/kmp_huc10.geojson with statewide CA coverage
  prepare_context_states.R       generate CA + OR state outlines for the report inset
  generate_simulated_metrics.R   reproducible simulated demo dataset
  test_*.R                       module smoke tests
docs/
  data-acquisition.md            living tracker for sourcing real metric data
.github/workflows/
  deploy.yml                     CI: shinylive build + GitHub Pages publish
```

## Customizing for your analysis

Most changes live in two YAML files; non-developers can edit them safely.

### Add or refine a scenario

`data/scenarios.yaml`:

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

Metric names must match column names in `data/kmp_metrics.csv` exactly. Mismatches surface in the app's Diagnostics tab.

### Add or adjust metric metadata

`data/metrics.yaml`:

```yaml
- name: "Simulated: Invasive plant extent"
  category: Restoration
  direction: negative          # low raw value -> high priority score
  description: >
    Tooltip text shown on the slider in Step 3.
```

`category` drives grouping in the custom-metric picker modal.
`direction: negative` inverts bin scores so "bin 5" always means "high priority".
`description` becomes a hover tooltip on the slider's metric name.

### Replace the master data

Drop a new `data/kmp_metrics.csv` into place. Required schema:

- `huccode` — plain digit strings at a single HUC level (4 / 6 / 8 / 10 / 12). No scientific notation.
- `name` — optional, for display labels.
- Any number of numeric metric columns.

The app auto-detects HUC level from code length and loads the matching boundary file. Metric metadata and scenarios that reference unknown columns surface in Diagnostics.

For the data team's transition plan from simulated to real metrics, see
[`docs/data-acquisition.md`](docs/data-acquisition.md).

### Regenerate bundled boundaries

| Source change | Script |
|---|---|
| KMP HUC boundaries (HUC4–12 GPKG) | `scripts/split_huc_layers.R` |
| KMP zone outline (shapefile) | `scripts/prepare_kmp_boundary.R` |
| Statewide CA HUC10 (shapefile) | `scripts/prepare_ca_huc10.R` |
| CA + OR state outlines (no source — pulls from `maps` package) | `scripts/prepare_context_states.R` |

## Data sources

- **HUC boundaries:** USGS Watershed Boundary Dataset (public domain). Bundled boundaries are Visvalingam-Whyatt simplified for web display; regenerate from authoritative WBD for analysis elsewhere.
- **KMP zone outline:** Klamath Meadows Partnership working group, July 2022 draft.
- **CA + OR state outlines:** `maps` R package (public domain).
- **Simulated metrics:** randomly generated for development and demonstration. Column names are prefixed `Simulated:` so they cannot be mistaken for production data. See `scripts/generate_simulated_metrics.R` for the deterministic generator.

## Testing

Module smoke tests in `scripts/`:

```bash
Rscript scripts/test_input.R        # CSV read + validation
Rscript scripts/test_columns.R      # column classifier
Rscript scripts/test_scenarios.R    # scenario loader + validation
Rscript scripts/test_sensitivity.R  # Monte Carlo end-to-end
```

Each prints human-readable output and exits clean on success.

## Deployment

The app is deployed to GitHub Pages as a static shinylive bundle. The
deploy runs automatically on every push to `main` via
`.github/workflows/deploy.yml` — no server or R hosting required. The
post-build step patches the browser tab title from the shinylive
default to "KMP Watershed Prioritization".

Page URL: _see the GitHub repository "About" field._

## License

MIT. See [LICENSE](LICENSE).

## Acknowledgments

Developed for the Klamath Meadows Partnership working group.
HUC boundary data from the USGS Watershed Boundary Dataset.
