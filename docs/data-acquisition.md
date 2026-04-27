# Data acquisition tracker

This is the living checklist for sourcing the real spatial datasets that
feed `data/kmp_metrics.csv`. The current master file is **simulated demo
data** (see the amber banner in the deployed app) — when each metric
below moves from "Not started" to "Integrated", the corresponding column
in `kmp_metrics.csv` should be replaced with real values.

Update this file as you go. The status field is the single source of
truth — both the data team and the app developer can read it to know
what's done and what's pending.

---

## How to use this file

**Status values** (set per metric):

| Status | Meaning |
|---|---|
| `Not started` | Source identified, no acquisition attempt yet |
| `Sourced` | Data downloaded / received; raw file on disk |
| `Processed` | Cleaned, clipped to KMP zone, summarized to HUC10 (one value per HUC) |
| `Integrated` | Replacing the simulated column in `data/kmp_metrics.csv`; metric metadata in `data/metrics.yaml` updated; tested in app |

**When you start work on a metric:**
1. Update its row below: status, local path, point of contact, notes
2. Process the source data into a per-HUC10 attribute (one row per HUC10, one numeric value per metric)
3. Replace the simulated column in `data/kmp_metrics.csv`
4. Update `data/metrics.yaml` — keep the category, set the right direction, replace the placeholder description with a real one
5. Re-test in the app (especially: scenarios that reference this metric)
6. Mark this row "Integrated" and add the date

---

## Bundled spatial datasets — already in the tool

These are committed to the repo and ready to use. No further sourcing needed unless WBD or KMP boundary updates.

| Dataset | File | Source | Notes |
|---|---|---|---|
| KMP zone outline | `data/kmp_boundary.geojson` | KMP working group, July 2022 draft | 2,986 vertices (Visvalingam @ 2% keep). Regen via `scripts/prepare_kmp_boundary.R`. |
| HUC4 boundaries | `data/kmp_huc4.geojson` | USGS WBD | 4 features. Regen via `scripts/split_huc_layers.R`. |
| HUC6 boundaries | `data/kmp_huc6.geojson` | USGS WBD | 6 features. Used for sub-zone selector. |
| HUC8 boundaries | `data/kmp_huc8.geojson` | USGS WBD | 38 features. |
| HUC10 boundaries (statewide CA) | `data/kmp_huc10.geojson` | USGS WBD via `scripts/prepare_ca_huc10.R` | **1,128 features covering all of California** (203 within KMP zone). Quietly enables uploads for any CA HUC10. |
| HUC12 boundaries | `data/kmp_huc12.geojson` | USGS WBD | 837 features (KMP zone only). |
| CA + OR state outlines | `data/context_states.geojson` | `maps` R package via `scripts/prepare_context_states.R` | Used for the report-map locator inset. |

---

## Metric inventory

22 candidate metrics across 5 categories. Headers below match the existing simulated column names in `data/kmp_metrics.csv` and the entries in `data/metrics.yaml` — when sourcing real data, adopt the same naming so the app picks them up automatically (drop the `Simulated:` prefix once real data is in).

---

### Fire (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Recent fire (% area, 10yr) | ↑ | MTBS — Monitoring Trends in Burn Severity | https://www.mtbs.gov/ | Not started | | | Intersect MTBS perimeters with HUC10, compute fraction burned past 10 yr |
| Unburned 30yr (%) | ↑ | MTBS / FRAP | mtbs.gov / frap.fire.ca.gov | Not started | | | Complement of 30-yr burn union per HUC10 |
| Fire return departure | ↑ | LANDFIRE MFRI | https://landfire.gov/ | Not started | | | Compare LANDFIRE Mean Fire Return Interval to actual MTBS history |
| High severity fire (% area) | ↑ | MTBS severity raster | mtbs.gov | Not started | | | Sum class-4 (high severity) area per HUC10 |
| Wildfire hazard potential (optional) | ↑ | USFS WHP | https://wfhp.fs.fed.us/ | Not started | | | Optional addition; not in original list. Useful for "protect" framing. |

---

### Climate (6 metrics — includes the 2 snowpack additions)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Climate resiliency index | ↑ | TNC Resilient Land Mapping (Anderson et al.) | https://resilientlandscapes.org/ | Not started | | | Raster — zonal stats per HUC10 |
| Climate refugia score | ↑ | Morelli et al. refugia framework, USGS CASC | various | Not started | | | Alt: AdaptWest climate refugia for western NA |
| Snow persistence | ↑ | MODIS MOD10A1 daily snow cover | NSIDC | Not started | | | Long-term mean days-snow-present per HUC10 |
| Snowpack (peak SWE) | ↑ | SNODAS gridded SWE (NSIDC) or SNOTEL | nsidc.org | Not started | | | Mean April 1 SWE per HUC10 |
| Snowpack trajectory | ↑ | SNODAS time series 2003-present | nsidc.org | Not started | | | Linear trend of April 1 SWE per HUC10. For projections: BCM (Flint et al.) or CalAdapt |
| Summer water stress | ↓ | BCM Climatic Water Deficit (Flint et al.) | https://www.usgs.gov/.../basin-characterization-model | Not started | | | Alt: TerraClimate (UCSB) |

---

### Restoration potential (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Lost meadow potential | ↑ | USFS LMM (Landscape-scale Meadow Model) | USFS regional GIS | Not started | | | Original prioritization workbook used LMM high + medium suitability |
| Riparian restoration (ac) | ↑ | NHD hydrography + NLCD land cover | usgs.gov / mrlc.gov | Not started | | | Buffer NHD streams; non-natural cover within buffer = restoration acres |
| Meadow density (% area) | ↑ | CDFW Meadow Polygons | https://biogeodata.cnra.ca.gov/ | Not started | | | Alt: USFS LMM. Original workbook used "Inventoried Meadow Extent" |
| Invasive plant extent | ↓ | EDDMapS or Cal-IPC mapping | https://www.eddmaps.org/ / https://www.cal-ipc.org/ | Not started | | | National option: iMapInvasives |
| Beaver restoration potential (optional) | ↑ | BRAT — Beaver Restoration Assessment Tool | https://brat.riverscapes.xyz/ | Not started | | | Already HUC-level outputs for much of the West; optional addition |

---

### Biodiversity (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Fish of concern (# spp) | ↑ | NOAA Critical Habitat + CDFW BIOS + USFWS ECOS | various | Not started | | | Count listed fish species per HUC10. Salmonids likely dominate the signal in KMP zone |
| Rare amphibian richness | ↑ | CNDDB (CDFW) | https://wildlife.ca.gov/Data/CNDDB | Not started | | | Subscription required; cheap or free for conservation orgs |
| Rare plant richness | ↑ | CNDDB / CNPS Rare Plant Inventory | wildlife.ca.gov / rareplants.cnps.org | Not started | | | NatureServe Explorer is a free alternative |
| Aquatic connectivity | ↑ | CalFish Passage Assessment Database | https://www.calfish.org/ | Not started | | | Compute index from barrier density + passability per HUC10 |
| Coho habitat extent (optional) | ↑ | NOAA Critical Habitat | nmfs.noaa.gov | Not started | | | Existing KMP workbook had this; optional addition |

---

### Administrative / regulatory (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Federal ownership (%) | ↑ | PAD-US (Protected Areas Database) | https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-overview | Not started | | | Single download covers federal + state + local. Used in the original workbook |
| NEPA-ready area (ac) | ↑ | USFS PALS or regional equivalents | KMP team / USFS forest contacts | Not started | | | Less standardized; may require contacting individual forests / BLM districts |
| Regulatory ease | ↑ | Derived: jurisdictional overlay count | n/a | Not started | | | Composite score: count distinct jurisdictions per HUC10 from PAD-US + state + tribal layers |
| Tribal partnership (0-5) | ↑ | KMP internal records + BIA Tribal Trust Lands | KMP team / bia.gov | Not started | | | Partnership status is not in any spatial dataset; needs manual KMP attribution |
| Conservation easements | ↑ | NCED (National Conservation Easement Database) | https://www.conservationeasement.us/ | Not started | | | Easement coverage per HUC10. Optional supplement to "regulatory ease" |

---

## Phase 1 quick wins (start here)

These are direct downloads with minimal processing and high signal-to-effort ratio. Get them first — they unblock the most scenarios:

1. **PAD-US** → Federal ownership % (one download covers all ownership classes)
2. **MTBS** → Recent fire, high severity fire, unburned 30yr (three metrics from one dataset)
3. **CDFW Meadow polygons** → Meadow density
4. **TNC Resilient Land** → Climate resiliency
5. **NCED** → Conservation easements

After these five, the **KMP General** and **KMP Fire** scenarios are largely powered by real data.

## Phase 2 — derivations from multiple sources

6. **SNODAS time series** → Snowpack peak + trajectory
7. **BCM** → Summer water stress
8. **LANDFIRE + MTBS** → Fire return departure
9. **CNDDB** → Rare amphibian, rare plant, fish of concern richness

## Phase 3 — needs KMP coordination or bespoke compilation

10. **NEPA-ready area** — needs forest/district outreach
11. **Regulatory ease** — needs definition + composite construction
12. **Tribal partnership** — needs KMP internal records

---

## Data prep workflow — integrating a new metric

1. **Source the raw dataset** to `data/source/<metric_name>/` (or wherever — track in this file's "Local path" column).

2. **Process to per-HUC10 values.** The output should be a CSV (or computable table) with one row per HUC10 and one numeric column with the metric value. Use the bundled `data/kmp_huc10.geojson` as the spatial join target. Typical operations:
   - Reproject source to EPSG:4326 (or NAD83 / CA Albers for area work, then export 4326)
   - Intersect / extract zonal stats / count points by polygon
   - Aggregate to one value per HUC10

3. **Add the column to `data/kmp_metrics.csv`.** Use a clean column name (no `Simulated:` prefix). Match the existing naming style — short, parens for units (e.g., `Recent fire (% area, 10yr)`).

4. **Update `data/metrics.yaml`.** Replace or add an entry:
   ```yaml
   - name: "Recent fire (% area, 10yr)"
     category: Fire
     direction: positive
     description: >
       Fraction of the HUC that burned in the last 10 years per MTBS,
       intersected with HUC10 boundaries.
   ```

5. **Update scenarios in `data/scenarios.yaml`** if the metric's name changed (drop `Simulated:` prefix). The app validator will surface mismatches in the Diagnostics tab if you miss any.

6. **Test in the app.** Restart, pick the relevant scenario, verify the metric appears in Step 3 sliders with the right tooltip, choropleth lights up, sensitivity still runs.

7. **Mark "Integrated"** in this tracker with today's date.

---

## CSV schema reference

`data/kmp_metrics.csv` requirements (validated by `R/input.R`):

- Column `huccode`: 10-digit string, no scientific notation, no leading-zero loss. Save Excel column as Text before exporting.
- Column `name` (optional): plain text, used for display labels and report tables. Duplicate names are auto-disambiguated with the huccode in parentheses.
- All other numeric columns become metrics. Integer columns with ≤ 5 unique values are treated as ordinal and rescaled rather than Jenks-binned.
- Missing values: leave the cell blank. The validator parses `""` as NA. The app renders a "X HUCs missing" annotation on the corresponding slider.
- Direction is set in `data/metrics.yaml`, not in the CSV.

---

## When the simulated banner can come down

The app shows an amber "Demo deployment — simulated data" banner at the top of the sidebar. Remove this banner (in `app.R`, search for `"Demo deployment"`) once **all of the simulated metrics in the current scenarios** have been replaced with real data. Until then, leaving the banner protects against accidental misuse of the demo rankings for real decisions.
