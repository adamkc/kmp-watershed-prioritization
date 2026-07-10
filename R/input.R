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


#' Resolve a remote URL for an on-demand boundary file.
#'
#' The deployed shinylive bundle ships only the layers needed eagerly
#' (HUC10 KMP-only for the default map, HUC6 for the sub-zone list).
#' Bigger / rarer layers -- HUC4/8/12 and the statewide all-CA HUC10
#' (kmp_huc10_ca.geojson, needed only when someone uploads non-KMP
#' HUC10s) -- are hosted as plain static files under the site's
#' `boundaries/` folder and fetched on demand. `data/asset_base.txt`
#' holds that folder's absolute URL and is written by the deploy
#' workflow; it is absent in local checkouts (where every layer is on
#' disk), so this returns NULL there.
remote_boundary_url <- function(fname, data_dir = "data") {
  base_file <- file.path(data_dir, "asset_base.txt")
  if (!file.exists(base_file)) return(NULL)
  base <- trimws(readLines(base_file, warn = FALSE)[1])
  if (length(base) == 0 || !nzchar(base)) return(NULL)
  if (!grepl("/$", base)) base <- paste0(base, "/")
  paste0(base, fname)
}


#' Fetch a boundary file from the site's `boundaries/` folder into the
#' session temp dir (cached, so repeat uploads don't refetch). Returns
#' the local path, or NULL if there is no asset base or the fetch fails.
fetch_boundary <- function(fname, data_dir = "data") {
  url <- remote_boundary_url(fname, data_dir)
  if (is.null(url)) return(NULL)
  dest <- file.path(tempdir(), fname)
  if (!file.exists(dest)) {
    ok <- tryCatch({
      utils::download.file(url, dest, quiet = TRUE, mode = "wb")
      file.exists(dest) && file.info(dest)$size > 0
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) {
      if (file.exists(dest)) unlink(dest)
      return(NULL)
    }
  }
  dest
}


#' HUC10 boundaries, covering whatever huccodes `need` requires.
#'
#' The default KMP analysis only touches the 203 KMP HUC10s, so the
#' small committed kmp_huc10.geojson is used whenever it covers `need`.
#' A custom upload can reference any California HUC10, which needs the
#' statewide kmp_huc10_ca.geojson (~1,128 features): used from disk if
#' present (local dev), otherwise fetched once from `boundaries/`.
load_huc10_boundaries <- function(data_dir = "data", need = NULL) {
  ca_local  <- file.path(data_dir, "kmp_huc10_ca.geojson")
  kmp_local <- file.path(data_dir, "kmp_huc10.geojson")

  kmp <- NULL
  if (file.exists(kmp_local)) {
    kmp <- sf::st_read(kmp_local, quiet = TRUE)
    if (is.null(need) || all(need %in% kmp$huc10)) return(kmp)
  }

  # Need HUC10s beyond the KMP set (a statewide upload). Use the full
  # all-CA layer: local if present, else fetched from boundaries/.
  if (file.exists(ca_local)) return(sf::st_read(ca_local, quiet = TRUE))
  dest <- fetch_boundary("kmp_huc10_ca.geojson", data_dir)
  if (!is.null(dest)) return(sf::st_read(dest, quiet = TRUE))

  # Couldn't get the fuller layer; fall back to whatever we have so the
  # KMP HUCs at least still map (non-KMP uploads will show as unmatched).
  if (!is.null(kmp)) return(kmp)
  stop("No HUC10 boundary file available.", call. = FALSE)
}


#' Load boundary geometry for a given HUC level.
#'
#' Prefers a local file on disk; for levels not bundled in the
#' lightweight deploy (HUC4/8/12, statewide HUC10) it fetches from the
#' site's `boundaries/` folder. `need` is the set of huccodes the caller
#' must be able to map -- used at HUC10 to decide between the small
#' KMP-only file and the statewide layer.
load_boundaries <- function(level, data_dir = "data", need = NULL) {
  if (level == 10L) return(load_huc10_boundaries(data_dir, need))

  fname <- paste0("kmp_huc", level, ".geojson")
  path  <- file.path(data_dir, fname)
  if (file.exists(path)) return(sf::st_read(path, quiet = TRUE))

  dest <- fetch_boundary(fname, data_dir)
  if (is.null(dest)) {
    stop("Could not load the HUC", level, " boundary layer (not on disk ",
         "and no remote asset base configured). The ranking table still ",
         "works; only the map needs the boundary geometry.", call. = FALSE)
  }
  sf::st_read(dest, quiet = TRUE)
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
