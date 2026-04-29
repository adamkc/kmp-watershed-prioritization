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

**Workflow:** real metrics accumulate in a parallel master file
`data/kmp_metrics_real.csv` while the live app keeps reading the simulated
`data/kmp_metrics.csv`. Once 5–10 metrics are processed and merged into the
real file, we do a single full swap (rename real → live, update
`metrics.yaml` and `scenarios.yaml` to drop `Simulated:` prefixes, drop
the demo banner). This avoids partial mixed states where some columns are
real and some are simulated.

**Status values** (set per metric):

| Status | Meaning |
|---|---|
| `Not started` | Source identified, no acquisition attempt yet |
| `Sourced` | Data downloaded / received; raw file on disk under `data/source/<metric>/` |
| `Processed` | Summarized to HUC10 (one row per HUC10) and merged into `data/kmp_metrics_real.csv` under its real (un-prefixed) column name |
| `Integrated` | Reserved for the eventual full-swap moment when `kmp_metrics_real.csv` replaces the simulated master |

**When you start work on a metric:**
1. Update its row below: status, local path, point of contact, notes
2. Acquire the raw source into `data/source/<metric>/` (gitignored)
3. Process to a per-HUC10 CSV (`huccode` + numeric value column)
4. Merge that column into `data/kmp_metrics_real.csv` (left-join on
   `huccode`, real column name with no `Simulated:` prefix)
5. Mark this row "Processed" and add the date

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
| Recent fire (% area, 10yr) | ↑ | MTBS perimeters (BAB shapefile) | edcintl.cr.usgs.gov/.../mtbs_perimeter_data.zip | Processed | `data/source/mtbs/` | 2026-04-28 | `scripts/acquire_mtbs.R`. Union of perimeters with `ig_date >= today − 10y`, intersected with HUC10. |
| Unburned 30yr (%) | ↑ | MTBS perimeters (same file) | (same) | Processed | `data/source/mtbs/` | 2026-04-28 | Complement of 30-yr burn union per HUC10. Computed in same script. |
| Fire return departure | ↑ | LANDFIRE MFRI | https://landfire.gov/ | Not started | | | Compare LANDFIRE Mean Fire Return Interval to actual MTBS history |
| High severity fire (% area) | ↑ | USFS RAVG annual CBI-4 mosaics (class 4 = >75% basal area mortality) | https://www.fs.usda.gov/science-technology/disturbance/wildland-fire/ravg | Processed | `data/source/ravg/` | 2026-04-28 | `scripts/acquire_ravg.R`. Union of class-4 cells across 2016–2026, 30 m, NAD83/CONUS Albers. CONUS coverage so all 203 HUCs computed (incl. 7 OR HUCs). |
| Wildfire hazard potential | ↑ | USFS WHP 2023 continuous CONUS raster (`whp2023_cnt_conus.tif`) | https://www.firelab.org/project/wildfire-hazard-potential (manual download) | Processed | `data/source/wfp/` | 2026-04-28 | `scripts/acquire_whp.R`. 270 m raster in NAD83/CONUS Albers; area-weighted mean per HUC10 via `exact_extract`. |
| Moderate-high severity fire (% area) | ↑ | USFS RAVG CBI-4 mosaics (class ≥ 3 union, 10y) | https://www.fs.usda.gov/science-technology/disturbance/wildland-fire/ravg | Processed | `data/source/ravg/` | 2026-04-28 | Added beyond original spec. Captures stand-replacing-or-near-it fire — broader than strict high-severity. Computed in `scripts/acquire_ravg.R`. |
| Recent fire (% area, 3yr) | ↑ | USFS RAVG CBI-4 mosaics (class > 1 union, 3y) | (same) | Processed | `data/source/ravg/` | 2026-04-28 | Added beyond original spec. Stricter than the 10y MTBS-perimeter metric — only counts pixels with confirmed post-fire severity. Computed in `scripts/acquire_ravg.R`. |
| Reburn area (% area) | ↑ | USFS RAVG CBI-4 mosaics (count of class > 1 across years 2010–2026, threshold ≥ 2) | (same) | Processed | `data/source/ravg/` | 2026-04-28 | Added beyond original spec. Fraction of HUC10 area where ≥ 2 distinct years had class > 1 over the 17-year RAVG record. Strict definition of reburn (not "burned at all"). Computed in `scripts/acquire_ravg.R`. |

---

### Climate (6 metrics — includes the 2 snowpack additions)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Climate resiliency index | ↑ | TNC Resilient and Connected Network — Terrestrial Resilience subdataset | https://maps.tnc.org/resilientland/ (manual download of `RCN_Data.gdb`) | Processed | `data/source/tnc-rl/` | 2026-04-28 | `scripts/acquire_tnc_resilient.R`. Pixel = z-score × 1000 (range -3.501 to +3.500); zonal mean per HUC10. Used `Terrestrial_resilience` subdataset because GDAL OpenFileGDB doesn't decode the 32-bit `Resilient_Sites_Terrand_Coast` RAT correctly. |
| Climate refugia score | ↑ | CDFW ACE Terrestrial Climate Resilience v3.0 (BIOS ds2738), `VegRefugiaScore` field | https://services2.arcgis.com/Uq9r85Potqm3MfRV/.../biosds2738_fpu/FeatureServer | Processed | `data/source/cdfw_ace/` | 2026-04-28 | `scripts/acquire_cdfw_ace_terrestrial.R`. Hex-grid (~64k statewide); paginated server-side bbox filter to ~19k hexes overlapping KMP. Per-HUC10 = area-weighted mean of `VegRefugiaScore` over intersecting hexes. Note: 7 OR HUCs (1710 prefix) get values only from their CA overlap, which can be a tiny sliver — Mack Arch Cove value is essentially edge-effect noise. |
| Terrestrial connectivity rank | ↑ | CDFW ACE Terrestrial Connectivity v3.2.3 (BIOS ds2734), `Connectivity_rank` field | https://services2.arcgis.com/Uq9r85Potqm3MfRV/.../biosds2734_fpu/FeatureServer | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Added beyond original spec. Same script. Same OR-coverage caveat as Climate refugia score. Useful for terrestrial wildlife movement; complements `Aquatic connectivity` (still pending — needs CalFish Passage Assessment). |
| Snow persistence | ↑ | MODIS MOD10A1 daily snow cover | NSIDC | Not started | | | Long-term mean days-snow-present per HUC10. Needs Earthdata login to acquire — punted for now. |
| Snowpack (peak SWE, mm) | ↑ | SNODAS masked CONUS daily SWE (NOAA G02158, product code 1034) | https://noaadata.apps.nsidc.org/NOAA/G02158/ | Processed | `data/source/snodas/` | 2026-04-28 | `scripts/acquire_snodas.R`. April 1 grids 2010–2025 (16 years), pulled from NSIDC public archive (no auth). 30-arcsec geographic grid, parsed from .dat.gz binary using header from .txt.gz. Per-HUC10 = mean across years of bilinear-resampled SWE. |
| Snowpack trajectory (mm/yr) | ↑ | SNODAS April-1 SWE 2004–2025; OLS and Theil-Sen slopes computed but **not used** (see note below) | (same) | Excluded | `data/source/snodas/` | 2026-04-28 | **Not in `kmp_metrics_real.csv`** — see "SNODAS trend caveat" below. Computation kept in `scripts/acquire_snodas.R` and per-HUC values in `data/source/snodas/huc10_snowpack.csv` for reference; per-HUC plot at `docs/plots/snodas_swe_trends.png`. |
| Summer water stress (CWD, mm) | ↓ | BCM 1991–2020 mean Climatic Water Deficit (`cwd1991_2020_ave.asc`) | USGS BCM (Flint et al.) — manual download | Processed | `data/source/bcm/` | 2026-04-28 | `scripts/acquire_bcm.R` (parameterised over BCM variables). ESRI ASCII grid, 270 m, EPSG:3310; NoData -9999 force-masked; zonal mean per HUC10. CWD = unmet ET demand summed over the year — direct water-stress signal. Top HUCs: Sacramento Valley / Bay Estuaries; bottom HUCs: North Coast / Klamath maritime. Other BCM variables on disk (aet, ppt, pet, pck, run, str, tmn, tmx, rch) can be added by extending `VARS` in the script. The earlier `Mean annual recharge (mm)` column was dropped — recharge is biased low in groundwater discharge zones (alluvial valleys), so it doesn't cleanly map to "water stress" the way CWD does. |

---

### Restoration potential (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Lost meadow potential (% area) | ↑ | USFS LMM (Landscape-scale Meadow Model) — meadow-prediction polygons | USFS regional GIS (manual placement of `lmm_predictions.geojson`) | Processed | `data/source/lmm/` | 2026-04-28 | `scripts/acquire_lmm.R`. Source geojson packed all polygons into a single Feature with two-level nested GeometryCollections — script unwraps via `jsonlite` and writes a clean intermediate `lmm_clean.geojson`. 24,241 meadow polygons, intersected with HUC10. |
| Riparian restoration (ac) | ↑ | NHD hydrography + NLCD land cover | usgs.gov / mrlc.gov | Not started | | | Buffer NHD streams; non-natural cover within buffer = restoration acres |
| Meadow density (% area) | ↑ | KMP Meadow Inventory (`merged_meadows_20260408.gpkg`, 6,041 polygons, all HGM1 = "Riparian Low Gradient") | KMP partnership in-house compilation (manual placement) | Processed | `data/source/kmp-inventory/` | 2026-04-28 | `scripts/acquire_kmp_meadow_inventory.R`. Authoritative within the KMP zone. CRS NAD83/UTM 10N (EPSG:26910), reprojected to EPSG:5070 for area math; intersection with HUC10 + sum / HUC area × 100. 47 of 203 HUCs have inventoried meadows; 156 are 0 (the 7 OR HUCs read 0 because KMP inventory doesn't extend into Oregon — not strictly the same as "no meadows there"). Distinct from `Lost meadow potential (% area)` (LMM-modeled), which sees more HUCs because it predicts where meadows *could* be. |
| Invasive plant extent | ↓ | EDDMapS or Cal-IPC mapping | https://www.eddmaps.org/ / https://www.cal-ipc.org/ | Not started | | | National option: iMapInvasives |
| Beaver restoration potential (optional) | ↑ | BRAT — Beaver Restoration Assessment Tool | https://brat.riverscapes.xyz/ | Not started | | | Already HUC-level outputs for much of the West; optional addition |

---

### Biodiversity (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Fish of concern (# spp) | ↑ | CDFW ACE Aquatic Biodiversity Summary v3.0 (BIOS ds2768), `RarFish` field | https://services2.arcgis.com/Uq9r85Potqm3MfRV/.../biosds2768_fpu/FeatureServer | Processed | `data/source/cdfw_ace/` | 2026-04-28 | `scripts/acquire_cdfw_ace_aquatic.R`. Direct ArcGIS Feature Service pull (3 paginated pages, 4,473 HUC12 records). Per-HUC10 = max(RarFish) over child HUC12s. CDFW dataset includes cross-border OR HUC12s, so all 203 HUCs covered. |
| Rare amphibian richness | ↑ | CDFW ACE Aquatic Biodiversity Summary v3.0 (BIOS ds2768), `RarAqAmph` field | (same service) | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Same script as fish-of-concern. Aquatic amphibians only — does not include terrestrial amphibians (CNDDB still pending if those are wanted). |
| Rare plant richness | ↑ | CNDDB / CNPS Rare Plant Inventory | wildlife.ca.gov / rareplants.cnps.org | Not started | | | NatureServe Explorer is a free alternative |
| Aquatic connectivity | ↑ | CalFish Passage Assessment Database | https://www.calfish.org/ | Not started | | | Compute index from barrier density + passability per HUC10 |
| Coho habitat extent (optional) | ↑ | NOAA Critical Habitat | nmfs.noaa.gov | Not started | | | Existing KMP workbook had this; optional addition |
| Native fish richness (# spp) | ↑ | CDFW ACE Aquatic Native Fish Richness v3.0 (BIOS ds2744), `NtvFish` field | https://services2.arcgis.com/Uq9r85Potqm3MfRV/.../biosds2744_fpu/FeatureServer | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Added beyond original spec. `scripts/acquire_cdfw_ace_native_fish.R`. Per-HUC10 = max(NtvFish) over child HUC12s. HUC10s absent from the source dataset are treated as 0 (CDFW found no native fish), not NA. |
| Coho salmon present | ↑ | CDFW ACE Aquatic Species List v3.0 (BIOS ds2740, Extended Table layer 1) — `Sci_Name = Oncorhynchus kisutch` | https://services2.arcgis.com/Uq9r85Potqm3MfRV/.../biosds2740_fpu/FeatureServer/1 | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Added beyond original spec. `scripts/acquire_cdfw_ace_species.R`. Binary 0/1; 1 if any child HUC12 lists coho. 47-page paginated table pull (92,119 species×HUC12 records cached at `ace_species_huc12_raw.csv`). |
| Chinook salmon present | ↑ | (same source, `O. tshawytscha`) | (same) | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Same script. Binary 0/1. |
| Steelhead/rainbow trout present | ↑ | (same source, `O. mykiss` incl. all forms) | (same) | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Same script. Binary 0/1. Combines anadromous steelhead and resident rainbow trout (same species). |
| Coastal cutthroat trout present | ↑ | (same source, `O. clarkii clarkii`) | (same) | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Same script. Binary 0/1. |
| Salmonid species (# spp) | ↑ | (same source, all `Oncorhynchus` and `Salvelinus`) | (same) | Processed | `data/source/cdfw_ace/` | 2026-04-28 | Distinct salmonid binomials per HUC10. 0–7 species range. |

---

### Administrative / regulatory (5 metrics)

| Metric | Dir | Source dataset | Source URL / contact | Status | Local path | Updated | Notes |
|---|---|---|---|---|---|---|---|
| Federal ownership (%) | ↑ | PAD-US 4.1 — Fee class, Mang_Type=FED | ScienceBase 6759abcfd34edfeb8710a004 (CA state GDB, manual download) | Processed | `data/source/pad_us/` | 2026-04-28 | `scripts/acquire_pad_us.R`. CA-only (ScienceBase auth wall blocks scripted DL); 7 OR HUCs in region 1710 written as NA. 196/203 HUCs computed. |
| NEPA-ready area (ac) | ↑ | USFS PALS or regional equivalents | KMP team / USFS forest contacts | Not started | | | Less standardized; may require contacting individual forests / BLM districts |
| Regulatory ease | ↑ | Derived: jurisdictional overlay count | n/a | Not started | | | Composite score: count distinct jurisdictions per HUC10 from PAD-US + state + tribal layers |
| Tribal partnership (0-5) | ↑ | KMP internal records + BIA Tribal Trust Lands | KMP team / bia.gov | Not started | | | Partnership status is not in any spatial dataset; needs manual KMP attribution |
| Conservation easements | ↑ | NCED (National Conservation Easement Database) | https://www.conservationeasement.us/ | Not started | | | Easement coverage per HUC10. Optional supplement to "regulatory ease" |

---

## SNODAS trend caveat

Snowpack **trajectory** metrics (OLS or Theil-Sen slope of April-1 SWE
across years) are intentionally **excluded** from `data/kmp_metrics_real.csv`,
even though the per-HUC values were computed and saved in
`data/source/snodas/huc10_snowpack.csv`.

**Why:** SNODAS production began in late 2003, so the longest possible
window is ~22 years (2004–2025). Within that window April-1 SWE has
been dominated by extreme single years — the 2015 drought (regional
mean ≈ 0 mm) and the 2023 record snow year (regional mean ≈ 146 mm,
≈ 7× the early-period average). Both OLS and Theil-Sen fits over this
window come out slightly positive (region-mean OLS +2.06 mm/yr,
Theil-Sen +1.23 mm/yr; per-pixel TS median = 0 mm/yr), which contradicts
the multi-decade decline in Sierra/Cascade snowpack documented in the
literature.

A short, outlier-dominated observed record cannot be relied on to
indicate the **future** trajectory of snowpack. If a snowpack-trajectory
metric is desired later, candidate longer / forward-looking sources
include:

- **SNOTEL station trends** (USDA NRCS, ~1980–present at point sites,
  spatially interpolate to HUC10)
- **CalAdapt LOCA downscaled projections** (modeled SWE 1950–2099) —
  switches the metric meaning from "observed past trend" to
  "modeled projected change"
- **Livneh et al. gridded historical SWE** (1915–2018, ~6 km)

Until one of these is integrated, only `Snowpack (peak SWE, mm)` is
exposed as a real metric. The trajectory plot at
`docs/plots/snodas_swe_trends.png` documents the decision.

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
