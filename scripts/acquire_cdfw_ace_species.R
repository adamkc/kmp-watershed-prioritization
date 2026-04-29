# Acquire CDFW ACE Aquatic Species List (BIOS ds2740, Extended Table) and
# extract per-HUC10 presence of major salmonid species.
#
# Output columns added to per-HUC10 frame:
#   * Coho salmon present                  -- 0/1 (Oncorhynchus kisutch)
#   * Chinook salmon present               -- 0/1 (O. tshawytscha)
#   * Steelhead/rainbow trout present      -- 0/1 (O. mykiss, all forms)
#   * Coastal cutthroat trout present      -- 0/1 (O. clarkii clarkii)
#   * Salmonid species (# spp)             -- distinct species count from
#                                             {Oncorhynchus, Salvelinus}
#                                             across HUC12s in the HUC10
#
# Source: CDFW ACE v3.0, Aquatic Species List, dataset ds2740 -- Extended
# Table at FeatureServer layer 1 (table-only, no geometry, 92k rows
# linking HUC12 -> species). Direct paginated pull from:
#   https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2740_fpu/FeatureServer/1
#
# Roll-up: presence per HUC12 -> max() per HUC10 (i.e. species marked
# present if any child HUC12 lists it). HUC10s absent from the source
# table are treated as 0 (no salmonid records).
#
# Output: data/source/cdfw_ace/huc10_ace_salmonids.csv
# Raw cache (for any later pull on a different species): the full
# species×HUC12 table is written to ace_species_huc12_raw.csv.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
})

SERVICE_URL <- "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds2740_fpu/FeatureServer/1/query"
PAGE_SIZE  <- 2000
SRC_DIR    <- "data/source/cdfw_ace"
OUT_CSV    <- file.path(SRC_DIR, "huc10_ace_salmonids.csv")
RAW_CSV    <- file.path(SRC_DIR, "ace_species_huc12_raw.csv")
METRICS_CSV <- "data/kmp_metrics.csv"

dir.create(SRC_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Page through the table (cache locally) -------------------------

if (!file.exists(RAW_CSV)) {
  fetch_page <- function(offset) {
    message("  fetching offset=", offset, " ...")
    q <- list(
      where             = "1=1",
      outFields         = "HUC12,Sci_Name,Com_Name,Rare,Model,Observation",
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
    if (offset > 1e6) stop("Aborting: > 1M records, sanity bound hit")
  }
  raw <- do.call(rbind, pages)
  message("Fetched ", nrow(raw), " species-HUC12 records")
  write_csv(raw, RAW_CSV)
} else {
  message("Using cached raw table at ", RAW_CSV)
  raw <- read_csv(RAW_CSV, show_col_types = FALSE,
                  col_types = cols(HUC12 = col_character()))
}

# ---- 2. Subset to salmonids and roll up to HUC10 -----------------------

# Salmonid genera: Oncorhynchus (Pacific salmon/trout), Salvelinus (chars
# incl. bull trout). Salmo (Atlantic salmon, brown trout) not native to CA
# and absent from the dataset above.
salm <- raw[grepl("^Oncorhynchus|^Salvelinus", raw$Sci_Name), ]
salm$huccode <- substr(salm$HUC12, 1, 10)
message("Salmonid records: ", nrow(salm))

# Helper: per-HUC10 presence (0/1) of any species in a Sci_Name pattern
present_per_huc10 <- function(pattern, all_huccodes) {
  hits <- unique(salm$huccode[grepl(pattern, salm$Sci_Name)])
  as.integer(all_huccodes %in% hits)
}

metrics <- read_csv(METRICS_CSV, show_col_types = FALSE,
                    col_types = cols(huccode = col_character()))
target_hucs <- metrics$huccode

out <- data.frame(huccode = target_hucs, stringsAsFactors = FALSE)

out$`Coho salmon present`              <- present_per_huc10(
  "^Oncorhynchus kisutch", target_hucs)
out$`Chinook salmon present`           <- present_per_huc10(
  "^Oncorhynchus tshawytscha", target_hucs)
out$`Steelhead/rainbow trout present`  <- present_per_huc10(
  "^Oncorhynchus mykiss", target_hucs)
out$`Coastal cutthroat trout present`  <- present_per_huc10(
  "^Oncorhynchus clarkii clarkii", target_hucs)

# Distinct salmonid species per HUC10 (collapse subspecies/ESUs to species
# level by taking the binomial = first two whitespace-separated tokens)
salm$species <- vapply(strsplit(salm$Sci_Name, " "), function(x) {
  paste(x[1:min(2, length(x))], collapse = " ")
}, character(1))
sp_per_huc <- aggregate(species ~ huccode, data = salm,
                        FUN = function(x) length(unique(x)))
names(sp_per_huc)[2] <- "Salmonid species (# spp)"
out <- merge(out, sp_per_huc, by = "huccode", all.x = TRUE)
out$`Salmonid species (# spp)`[is.na(out$`Salmonid species (# spp)`)] <- 0

out <- out[order(out$huccode), ]
write_csv(out, OUT_CSV)
message("Wrote ", OUT_CSV, " (", nrow(out), " rows)")

# ---- 3. Summaries ------------------------------------------------------

cat("\n=== HUC10 presence rates ===\n")
for (col in c("Coho salmon present", "Chinook salmon present",
              "Steelhead/rainbow trout present",
              "Coastal cutthroat trout present")) {
  cat(sprintf("  %-35s present in %3d / %d HUCs (%4.1f%%)\n",
              col, sum(out[[col]]), nrow(out),
              100 * mean(out[[col]])))
}

cat("\n=== Salmonid species count distribution ===\n")
print(summary(out$`Salmonid species (# spp)`))

cat("\nTop 10 salmonid-rich HUCs:\n")
print(head(out[order(-out$`Salmonid species (# spp)`), ], 10))
