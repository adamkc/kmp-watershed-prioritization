# Scoring: bin each metric into integer classes (Jenks for continuous,
# linear rescale for ordinal), then combine bin scores with user weights
# into a composite 1..n_classes score per HUC.
#
# Missing-value handling: a HUC missing a metric drops that metric from
# its composite (numerator and denominator both exclude it), so HUCs
# with different missingness patterns remain comparable on the same scale.

N_CLASSES_DEFAULT <- 5L


#' Bin a continuous numeric vector via Jenks natural breaks.
#'
#' Returns integer bin indices in [1, n_classes], with NAs preserved.
#' Falls back to ordinal_bin() when the column has too few unique
#' values for Jenks to carve meaningfully.
#'
#' Zero-inflated columns can cause Jenks to produce duplicate break
#' points (multiple breaks land on the same stacked value). Duplicate
#' breaks are collapsed and the resulting bins are rescaled to cover
#' [1, n_classes] evenly, so every metric contributes on the same
#' nominal range regardless of how many natural classes Jenks found.
jenks_bin <- function(x, n_classes = N_CLASSES_DEFAULT) {
  non_na <- x[!is.na(x)]
  if (length(unique(non_na)) <= n_classes) {
    return(ordinal_bin(x, n_classes))
  }
  breaks <- classInt::classIntervals(non_na, n = n_classes, style = "jenks")$brks
  breaks <- unique(breaks)
  if (length(breaks) < 2) {
    return(ordinal_bin(x, n_classes))
  }
  bins <- as.integer(cut(x, breaks = breaks, include.lowest = TRUE))
  rescale_bins(bins, n_classes)
}


#' Rescale observed bin indices to cover the full [1, n_classes] range.
#'
#' If fewer than n_classes distinct bins are present, the observed
#' values are mapped evenly onto 1..n_classes. This keeps metrics
#' comparable in a weighted average even when Jenks can only carve
#' 2 or 3 classes (e.g. zero-inflated data).
rescale_bins <- function(bins, n_classes) {
  u <- sort(unique(bins[!is.na(bins)]))
  if (length(u) == 0L) return(bins)
  if (length(u) == 1L) {
    bins[!is.na(bins)] <- n_classes
    return(bins)
  }
  if (length(u) == n_classes) return(bins)
  target <- as.integer(round(
    1 + (n_classes - 1) * (seq_along(u) - 1) / (length(u) - 1)
  ))
  remap <- setNames(target, as.character(u))
  as.integer(remap[as.character(bins)])
}


#' Linearly rescale an ordinal / low-cardinality vector to integer bins.
#'
#' Values at the min become bin 1, values at the max become n_classes,
#' everything between is linear + rounded. Constant columns get bin
#' n_classes (harmless: a constant metric has no discriminating power
#' and will wash out in the weighted average).
ordinal_bin <- function(x, n_classes = N_CLASSES_DEFAULT) {
  out <- rep(NA_integer_, length(x))
  mask <- !is.na(x)
  non_na <- x[mask]
  if (length(unique(non_na)) <= 1) {
    out[mask] <- n_classes
    return(out)
  }
  rng <- range(non_na)
  scaled <- 1 + (n_classes - 1) * (x - rng[1]) / (rng[2] - rng[1])
  as.integer(round(scaled))
}


#' Compute bin scores for every scoring column in the input.
#'
#' @param df             Validated input dataframe.
#' @param classification Output of classify_columns().
#' @param n_classes      Number of bins (default 5).
#' @return Data frame, one column per scoring metric, containing
#'         integer bin scores with NAs preserved.
compute_bin_scores <- function(df, classification,
                               n_classes = N_CLASSES_DEFAULT) {
  stopifnot(is.data.frame(df), is.data.frame(classification))
  scoring <- classification[classification$use_in_score, ]
  if (nrow(scoring) == 0) {
    return(data.frame(row.names = seq_len(nrow(df))))
  }

  cols <- lapply(seq_len(nrow(scoring)), function(i) {
    nm <- scoring$name[i]
    ty <- scoring$type[i]
    x  <- df[[nm]]
    if (ty == "numeric-ordinal") ordinal_bin(x, n_classes) else jenks_bin(x, n_classes)
  })
  names(cols) <- scoring$name
  as.data.frame(cols, check.names = FALSE)
}


#' Combine bin scores with weights into a composite per HUC.
#'
#' Composite_i = sum_j(bin_ij * w_j) / sum_j(w_j)
#'   where the sum is taken only over metrics j with w_j > 0
#'   AND bin_ij not NA. Returns NA if a HUC has no scorable metrics.
#'
#' The result is on the same 1..n_classes scale as bin scores, so
#' 5 means "top bin in every weighted metric" and 1 means the opposite.
#'
#' @param bin_scores Data frame from compute_bin_scores().
#' @param weights    Named numeric vector keyed by column name.
#'                   Columns absent from weights get weight 0 (excluded).
#' @return Numeric vector of composite scores, one per row.
composite_score <- function(bin_scores, weights) {
  stopifnot(is.data.frame(bin_scores), is.numeric(weights))
  n <- nrow(bin_scores)
  if (ncol(bin_scores) == 0 || n == 0) return(rep(NA_real_, n))

  # Align weights to bin_scores columns; missing weights -> 0.
  w <- setNames(rep(0, ncol(bin_scores)), names(bin_scores))
  common <- intersect(names(weights), names(bin_scores))
  w[common] <- as.numeric(weights[common])

  active <- which(w > 0)
  if (length(active) == 0) return(rep(NA_real_, n))

  bin_mat  <- as.matrix(bin_scores[, active, drop = FALSE])
  w_vec    <- w[active]
  w_mat    <- matrix(w_vec, nrow = n, ncol = length(w_vec), byrow = TRUE)
  mask     <- !is.na(bin_mat)

  numer <- rowSums(bin_mat * w_mat, na.rm = TRUE)
  denom <- rowSums(mask * w_mat)
  denom[denom == 0] <- NA_real_
  numer / denom
}


#' Rank HUCs by composite score; 1 = highest-scoring.
#' Ties share the lower rank ('min' ties method).
rank_hucs <- function(composite) {
  as.integer(rank(-composite, na.last = "keep", ties.method = "min"))
}
