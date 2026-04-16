# Column classifier: walks the input dataframe and tags each column with
# its role and basic statistics. Output drives:
#   - which columns get weight sliders (numeric-continuous, numeric-ordinal)
#   - which columns skip Jenks (numeric-ordinal used directly)
#   - per-slider annotations (missing-value count, zero-inflation flag)

# Default threshold for calling a numeric column "ordinal" rather than
# "continuous". With 25 HUC12s in the Scott River test case, columns like
# `Coho score` (0-5), `Cascades frog score` (0 or 2) have few unique values.
ORDINAL_MAX_UNIQUE <- 5L

# Columns at or above this share of zeros get a zero-inflation flag.
# Jenks on a 90%-zero column produces nonsense breaks; the UI should warn.
ZERO_INFLATED_SHARE <- 0.5


#' Classify each column in a validated input dataframe.
#'
#' @param df  A dataframe that has already been through validate_input().
#'            The identifier column should be named "huccode".
#' @param id_col  Name of the identifier column. Skipped from scoring.
#' @param ordinal_max_unique  Numeric columns with <= this many unique
#'                            non-NA values are classified as ordinal.
#'
#' @return A data.frame with one row per column:
#'   name           original column name
#'   type           "identifier" | "label" | "numeric-continuous"
#'                  | "numeric-ordinal" | "empty"
#'   n              rows with non-NA values
#'   n_missing      rows with NA
#'   n_unique       distinct non-NA values
#'   n_zero         zero values (numeric only; NA otherwise)
#'   min, max       range (numeric only)
#'   zero_inflated  TRUE if numeric and n_zero/n >= ZERO_INFLATED_SHARE
#'   use_in_score   default recommendation for scoring inclusion
classify_columns <- function(df,
                             id_col = "huccode",
                             ordinal_max_unique = ORDINAL_MAX_UNIQUE) {
  stopifnot(is.data.frame(df))

  rows <- lapply(names(df), function(cn) {
    x <- df[[cn]]
    n_missing <- sum(is.na(x))
    non_na <- x[!is.na(x)]
    n <- length(non_na)
    n_unique <- length(unique(non_na))
    is_num <- is.numeric(x)

    type <- if (identical(cn, id_col)) {
      "identifier"
    } else if (n == 0) {
      "empty"
    } else if (is_num) {
      if (n_unique <= ordinal_max_unique) "numeric-ordinal" else "numeric-continuous"
    } else {
      "label"
    }

    n_zero <- if (is_num) sum(non_na == 0) else NA_integer_
    min_v  <- if (is_num && n > 0) min(non_na) else NA_real_
    max_v  <- if (is_num && n > 0) max(non_na) else NA_real_
    zero_inflated <- is_num && n > 0 && (n_zero / n) >= ZERO_INFLATED_SHARE

    data.frame(
      name          = cn,
      type          = type,
      n             = as.integer(n),
      n_missing     = as.integer(n_missing),
      n_unique      = as.integer(n_unique),
      n_zero        = as.integer(n_zero),
      min           = min_v,
      max           = max_v,
      zero_inflated = zero_inflated,
      use_in_score  = type %in% c("numeric-continuous", "numeric-ordinal"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


#' Return just the names of columns that should get weight sliders,
#' in their original CSV order.
scoring_columns <- function(classification) {
  classification$name[classification$use_in_score]
}
