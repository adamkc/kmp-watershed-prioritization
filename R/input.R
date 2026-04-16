# Input handling: read user CSV, validate huccode column,
# auto-detect HUC level, and join to bundled boundary geometry.
#
# These are pure functions with no Shiny dependencies so they can be
# tested from scripts/ or a future testthat suite.

SUPPORTED_HUC_LEVELS <- c(4L, 6L, 8L, 10L, 12L)


#' Read a user-uploaded CSV into a tibble.
#'
#' Uses readr so that thousand-separated numbers in quoted strings
#' (e.g. "1,283.14") parse correctly. Forces `huccode` to character
#' so leading zeros and long IDs survive.
read_input_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      huccode = readr::col_character(),
      .default = readr::col_guess()
    ),
    locale = readr::locale(grouping_mark = ","),
    show_col_types = FALSE
  )
}


#' Detect HUC level from a vector of HUC codes.
#'
#' Returns one of SUPPORTED_HUC_LEVELS, or throws a clear error.
#' Checks the *raw* code strings for Excel scientific-notation corruption
#' before stripping non-digits, since "1.80102E+11" -> "18010211" would
#' otherwise look like a valid 8-digit HUC8 purely by coincidence.
detect_huc_level <- function(codes) {
  codes_chr <- as.character(codes)
  codes_chr <- codes_chr[!is.na(codes_chr) & nzchar(codes_chr)]

  if (length(codes_chr) == 0) {
    stop("No valid HUC codes found in `huccode` column.", call. = FALSE)
  }

  # Catch Excel scientific notation (e.g. "1.80102E+11") explicitly.
  sci_pattern <- "^-?\\d+\\.?\\d*[eE][+-]?\\d+$"
  if (any(grepl(sci_pattern, codes_chr))) {
    stop(
      "HUC codes appear to be in scientific notation (e.g. '1.80102E+11'). ",
      "This is typically an Excel export issue -- long numeric IDs get ",
      "truncated to scientific form. Double-check the huccode column ",
      "formatting in your source file before re-exporting.",
      call. = FALSE
    )
  }

  # HUC codes should be pure digits; flag anything with decimal points
  # that slipped past the scientific-notation check.
  if (any(grepl("\\.", codes_chr))) {
    stop(
      "HUC codes contain decimal points. HUC IDs should be pure digit strings. ",
      "Check the huccode column in your source file.",
      call. = FALSE
    )
  }

  clean <- gsub("\\D", "", codes_chr)
  lens <- nchar(clean)
  unique_lens <- sort(unique(lens))

  if (any(unique_lens < 4)) {
    stop(
      "HUC codes appear truncated (some < 4 digits). ",
      "Double-check the huccode column formatting in your source file ",
      "before re-exporting.",
      call. = FALSE
    )
  }

  if (length(unique_lens) > 1) {
    tab <- table(lens)
    summary <- paste(
      sprintf("%d x HUC%s", as.integer(tab), names(tab)),
      collapse = ", "
    )
    stop(
      "Input file mixes HUC scales (", summary, "). ",
      "The tool analyzes one scale at a time. ",
      "Please export at a single HUC level.",
      call. = FALSE
    )
  }

  level <- unique_lens
  if (!level %in% SUPPORTED_HUC_LEVELS) {
    stop(
      "Detected HUC code length ", level, " digits, which is not a supported scale. ",
      "Supported: HUC", paste(SUPPORTED_HUC_LEVELS, collapse = ", HUC"), ".",
      call. = FALSE
    )
  }

  as.integer(level)
}


#' Validate input dataframe and derive HUC level.
#'
#' Returns a list with `data` (df with normalized huccode column)
#' and `huc_level` (integer).
validate_input <- function(df) {
  col_match <- which(tolower(names(df)) == "huccode")
  if (length(col_match) == 0) {
    stop(
      "Input file is missing required column `huccode`. ",
      "Found columns: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
  names(df)[col_match] <- "huccode"

  # Drop rows with no huccode -- handles trailing empty Excel rows.
  df <- df[!is.na(df$huccode) & nzchar(as.character(df$huccode)), , drop = FALSE]

  # Detect level from the raw strings (before digit-stripping) so that
  # scientific-notation corruption is caught.
  level <- detect_huc_level(df$huccode)

  # Only now normalize to pure digits for downstream joins.
  df$huccode <- gsub("\\D", "", as.character(df$huccode))

  list(data = df, huc_level = level)
}


#' Load bundled boundary geometry for a given HUC level.
load_boundaries <- function(level, data_dir = "data") {
  path <- file.path(data_dir, paste0("kmp_huc", level, ".geojson"))
  if (!file.exists(path)) {
    stop("Boundary file not found: ", path, call. = FALSE)
  }
  sf::st_read(path, quiet = TRUE)
}


#' Join validated input data to boundary geometry.
#'
#' Returns a list describing the join:
#'   sf            - sf object of matched HUCs (input cols + geometry)
#'   n_input       - rows in input
#'   n_matched     - rows successfully joined
#'   unmatched_ids - huccodes in input but not in boundaries
#'   n_unused_geom - boundary features not referenced by any input row
join_input_to_boundaries <- function(df, boundaries, level) {
  huc_col <- paste0("huc", level)
  if (!huc_col %in% names(boundaries)) {
    stop("Expected column `", huc_col, "` in boundary file.", call. = FALSE)
  }

  matched <- merge(
    boundaries,
    df,
    by.x = huc_col,
    by.y = "huccode",
    all.x = FALSE
  )

  unmatched_ids <- setdiff(df$huccode, boundaries[[huc_col]])
  n_unused_geom <- length(setdiff(boundaries[[huc_col]], df$huccode))

  list(
    sf            = matched,
    n_input       = nrow(df),
    n_matched     = nrow(matched),
    unmatched_ids = unmatched_ids,
    n_unused_geom = n_unused_geom
  )
}
