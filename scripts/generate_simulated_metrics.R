# Generate a simulated HUC10-scale metrics table for development and
# demo use until the real KMP metrics table is assembled. Writes to
# data/kmp_metrics.csv (overwrites previous content).
#
# All metric names are prefixed with "Simulated:" so there is no
# confusion between synthetic demo data and real values.
#
# Run from the project root:
#   Rscript scripts/generate_simulated_metrics.R

library(sf)

set.seed(42)  # deterministic -- regen produces identical output

h10 <- st_read("data/kmp_huc10.geojson", quiet = TRUE)
n <- nrow(h10)
cat(sprintf("Generating simulated metrics for %d HUC10s...\n", n))


# ---- Helpers ----------------------------------------------------------------

# Zero-inflated beta: mass at 0 with probability p_zero, otherwise Beta(a,b).
zinf_beta <- function(n, p_zero, a, b) {
  ifelse(runif(n) < p_zero, 0, rbeta(n, a, b))
}

# Zero-inflated gamma-like area (for "N acres with X%"), lots of zeros common.
zinf_area <- function(n, p_zero, mean, sd) {
  ifelse(runif(n) < p_zero, 0, pmax(0, rnorm(n, mean, sd)))
}


# ---- Build the data frame ---------------------------------------------------

df <- data.frame(
  huccode = as.character(h10$huc10),
  name    = h10$name,
  stringsAsFactors = FALSE
)


# --- Fire (4 metrics) ---
df$`Simulated: Recent fire (% area, 10yr)`  <- round(zinf_beta(n, 0.55, 2.0, 5), 4)
df$`Simulated: Unburned 30yr (%)`           <- round(rbeta(n, 2.5, 1.8), 4)
df$`Simulated: Fire return departure`       <- round(rnorm(n, 0.4, 1.1), 3)
df$`Simulated: High severity fire (% area)` <- round(zinf_beta(n, 0.45, 1.5, 8), 4)

# --- Climate (4 metrics) ---
df$`Simulated: Climate resiliency index`    <- round(rbeta(n, 3, 3) * 10, 2)
df$`Simulated: Climate refugia score`       <- round(rbeta(n, 2, 3), 4)
df$`Simulated: Snow persistence`            <- round(rbeta(n, 4, 2.5), 4)
df$`Simulated: Summer water stress`         <- round(rbeta(n, 3, 3), 4)

# --- Restoration potential (4 metrics) ---
df$`Simulated: Lost meadow potential`       <- round(rgamma(n, 2, 0.1), 1)
df$`Simulated: Riparian restoration (ac)`   <- round(zinf_area(n, 0.15, 180, 140), 1)
df$`Simulated: Meadow density (% area)`     <- round(rbeta(n, 1.2, 15), 4)
df$`Simulated: Invasive plant extent`       <- round(rbeta(n, 2, 4), 4)

# --- Biodiversity (4 metrics) ---
df$`Simulated: Fish of concern (# spp)`     <- rbinom(n, size = 8,  prob = 0.25)
df$`Simulated: Rare amphibian richness`     <- rbinom(n, size = 6,  prob = 0.30)
df$`Simulated: Rare plant richness`         <- rbinom(n, size = 15, prob = 0.25)
df$`Simulated: Aquatic connectivity`        <- round(runif(n, 0, 100), 1)

# --- Administrative / regulatory (4 metrics) ---
df$`Simulated: Regulatory ease (0-5)`       <- sample(0:5, n, replace = TRUE,
                                                      prob = c(.10,.15,.25,.25,.15,.10))
df$`Simulated: Federal ownership (%)`       <- round(rbeta(n, 1.5, 1.5), 4)
df$`Simulated: NEPA-ready area (ac)`        <- round(zinf_area(n, 0.70, 200, 220), 1)
df$`Simulated: Tribal partnership (0-5)`    <- sample(0:5, n, replace = TRUE,
                                                      prob = c(.20,.20,.20,.20,.10,.10))


# ---- Write ------------------------------------------------------------------

out <- "data/kmp_metrics.csv"
write.csv(df, out, row.names = FALSE)

cat(sprintf("\nWrote %s\n", out))
cat(sprintf("  %d rows, %d columns\n", nrow(df), ncol(df)))
cat(sprintf("  %d metrics (excluding huccode, name)\n\n", ncol(df) - 2L))

# Quick distribution summary
cat("Metric ranges:\n")
for (cn in names(df)[-(1:2)]) {
  x <- df[[cn]]
  cat(sprintf("  %-46s  min %8.3f  max %8.3f  n_zero %3d\n",
              cn, min(x), max(x), sum(x == 0)))
}
