# Acquire CDFW ACE Aquatic Biodiversity Summary (BIOS ds2768) from the
# public ArcGIS Feature Service. Computes per-HUC10:
#
#   * Fish of concern (# spp)   -- max(RarFish) across HUC12s in each HUC10
#   * Rare amphibian richness   -- max(RarAqAmph) across HUC12s in each HUC10
#
# Source: CDFW Areas of Conservation Emphasis (ACE) v3.0, Aquatic
# Biodiversity Summary, dataset ds2768. Pre-summarised at the HUC12
# level (4,473 polygons statewide). Direct pull from the service:
#   https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2768_fpu/FeatureServer/0
# Pagination is required (service caps at 2000 records/request); we
# request 3 pages with `resultOffset` and stitch them together.
#
# Roll-up: each HUC12 lives within exactly one HUC10 (HUC12 = HUC10 + 2
# more digits). Because species lists are not exposed -- only rolled-up
# counts -- we take the max count across HUC12 children as a lower-bound
# estimate of the HUC10 richness.
#
# Output: data/source/cdfw_ace/huc10_ace_aquatic.csv

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
})

SERVICE_URL <- "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2768_fpu/FeatureServer/0/query"
PAGE_SIZE  <- 2000
SRC_DIR    <- "data/source/cdfw_ace"
OUT_CSV    <- file.path(SRC_DIR, "huc10_ace_aquatic.csv")
RAW_CSV    <- file.path(SRC_DIR, "ace_aquatic_huc12_raw.csv")
METRICS_CSV <- "data/kmp_metrics.csv"

dir.create(SRC_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Page through the feature service -------------------------------

fetch_page <- function(offset) {
  message("  fetching offset=", offset, " ...")
  q <- list(
    where               = "1=1",
    outFields           = "HUC12,Name,RarFish,RarAqAmph",
    returnGeometry      = "false",
    resultOffset        = offset,
    resultRecordCount   = PAGE_SIZE,
    f                   = "json"
  )
  r <- GET(SERVICE_URL, query = q, timeout(60))
  stop_for_status(r)
  d <- fromJSON(content(r, "text", encoding = "UTF-8"),
                simplifyVector = TRUE)
  if (!is.null(d$error)) stop("Service error: ", d$error$message)
  feats <- d$features
  if (is.null(feats) || length(feats) == 0) return(NULL)
  feats$attributes
}

pages <- list()
offset <- 0
repeat {
  page <- fetch_page(offset)
  if (is.null(page) || nrow(page) == 0) break
  pages[[length(pages) + 1]] <- page
  if (nrow(page) < PAGE_SIZE) break
  offset <- offset + PAGE_SIZE
  if (offset > 50000) stop("Aborting: more than 50k records, sanity bound hit")
}

ace <- do.call(rbind, pages)
message("Fetched ", nrow(ace), " HUC12 records")
write_csv(ace, RAW_CSV)

# ---- 2. Filter to KMP-zone HUC10s and roll up via max() ----------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
target_hucs <- metrics$huccode

# Each HUC12 belongs to the HUC10 == first 10 chars of its HUC12 code
ace$huccode <- substr(ace$HUC12, 1, 10)
ace_kmp <- ace[ace$huccode %in% target_hucs, ]
message("HUC12 records within KMP HUC10s: ", nrow(ace_kmp), " / ", nrow(ace),
        " (", length(unique(ace_kmp$huccode)), " HUC10s with coverage)")

agg <- aggregate(cbind(RarFish, RarAqAmph) ~ huccode, data = ace_kmp,
                 FUN = max, na.rm = TRUE)
names(agg)[2:3] <- c("Fish of concern (# spp)", "Rare amphibian richness")

# ---- 3. Build full per-HUC10 frame: 203 rows, NA for HUCs not in CA ----

out <- data.frame(huccode = target_hucs, stringsAsFactors = FALSE)
out <- merge(out, agg, by = "huccode", all.x = TRUE)
out <- out[order(out$huccode), ]

n_na <- sum(is.na(out$`Fish of concern (# spp)`))
message("HUC10s computed: ", nrow(out) - n_na,
        " / ", nrow(out), "   (", n_na, " NA, mostly Oregon)")

# Inspect which HUC10s are NA
na_hucs <- out$huccode[is.na(out$`Fish of concern (# spp)`)]
if (length(na_hucs) > 0) {
  message("NA HUC4 prefixes: ",
          paste(unique(substr(na_hucs, 1, 4)), collapse = ", "))
}

write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Fish of concern (# spp) ===\n")
print(summary(out$`Fish of concern (# spp)`))
cat("\n=== Rare amphibian richness ===\n")
print(summary(out$`Rare amphibian richness`))

cat("\nTop 10 by Fish of concern:\n")
top_fish <- head(out[order(-out$`Fish of concern (# spp)`),
                     c("huccode", "Fish of concern (# spp)",
                       "Rare amphibian richness")], 10)
print(top_fish)
