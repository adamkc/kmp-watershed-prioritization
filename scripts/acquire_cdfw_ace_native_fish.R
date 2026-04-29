# Acquire CDFW ACE Aquatic Native Fish Richness (BIOS ds2744) from the
# public ArcGIS Feature Service. Computes per-HUC10:
#
#   * Native fish richness (# spp)  -- max(NtvFish) across HUC12s in HUC10
#
# Source: CDFW Areas of Conservation Emphasis (ACE) v3.0, Aquatic Native
# Fish Richness, dataset ds2744. Pre-summarised at the HUC12 level
# (3,076 polygons -- fewer than ds2768 because it only covers HUCs that
# host native fish). Direct pull from the service:
#   https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2744_fpu/FeatureServer/0
#
# Roll-up: each HUC12 lives within exactly one HUC10. As with the
# rare-species pull in `acquire_cdfw_ace_aquatic.R`, we take max() across
# child HUC12s as a lower-bound estimate of HUC10 richness (full species
# lists are not exposed). HUC10s with no child HUC12 in the dataset are
# treated as 0 native-fish (rather than NA), since absence from this
# dataset means CDFW found no native fish at all there.
#
# Output: data/source/cdfw_ace/huc10_ace_native_fish.csv

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
})

SERVICE_URL <- "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2744_fpu/FeatureServer/0/query"
PAGE_SIZE  <- 2000
SRC_DIR    <- "data/source/cdfw_ace"
OUT_CSV    <- file.path(SRC_DIR, "huc10_ace_native_fish.csv")
RAW_CSV    <- file.path(SRC_DIR, "ace_native_fish_huc12_raw.csv")
METRICS_CSV <- "data/kmp_metrics.csv"

dir.create(SRC_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Page through the feature service -------------------------------

fetch_page <- function(offset) {
  message("  fetching offset=", offset, " ...")
  q <- list(
    where             = "1=1",
    outFields         = "HUC12,Name,NtvFish",
    returnGeometry    = "false",
    resultOffset      = offset,
    resultRecordCount = PAGE_SIZE,
    f                 = "json"
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

# ---- 2. Roll up to HUC10 ----------------------------------------------

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
target_hucs <- metrics$huccode

ace$huccode <- substr(ace$HUC12, 1, 10)
ace_kmp <- ace[ace$huccode %in% target_hucs, ]
message("HUC12 records within KMP HUC10s: ", nrow(ace_kmp), " / ", nrow(ace),
        " (", length(unique(ace_kmp$huccode)), " HUC10s with native fish)")

agg <- aggregate(NtvFish ~ huccode, data = ace_kmp, FUN = max, na.rm = TRUE)
names(agg)[2] <- "Native fish richness (# spp)"

# Build full per-HUC10 frame; HUC10s missing from ds2744 = 0 native fish
out <- data.frame(huccode = target_hucs, stringsAsFactors = FALSE)
out <- merge(out, agg, by = "huccode", all.x = TRUE)
out$`Native fish richness (# spp)`[is.na(out$`Native fish richness (# spp)`)] <- 0
out <- out[order(out$huccode), ]

write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

cat("\n=== Native fish richness (# spp) ===\n")
print(summary(out$`Native fish richness (# spp)`))
cat("\nTop 10 HUCs by native fish richness:\n")
print(head(out[order(-out$`Native fish richness (# spp)`), ], 10))
cat("\nHUCs with 0 native fish (showing 5):\n")
print(head(out[out$`Native fish richness (# spp)` == 0, ], 5))
