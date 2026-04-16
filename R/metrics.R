# Load metric metadata (category + direction) from data/metrics.yaml.
#
# Metrics not declared in the YAML default to direction = positive and
# category = "Other", so uploaded CSVs with unknown columns still work.

DEFAULT_METRIC_CATEGORY    <- "Other"
DEFAULT_METRIC_DIRECTION   <- "positive"
DEFAULT_METRIC_DESCRIPTION <- ""


#' Load metric metadata.
#'
#' @return list with
#'   $meta           named list keyed by metric name; each entry has
#'                   $category, $direction, $description
#'   $category_order character vector of preferred category ordering
load_metrics_meta <- function(path = "data/metrics.yaml") {
  if (!file.exists(path)) {
    return(list(meta = list(), category_order = character()))
  }
  raw <- yaml::read_yaml(path)
  meta <- list()
  if (!is.null(raw$metrics)) {
    for (m in raw$metrics) {
      meta[[m$name]] <- list(
        category    = m$category    %||% DEFAULT_METRIC_CATEGORY,
        direction   = m$direction   %||% DEFAULT_METRIC_DIRECTION,
        description = trimws(m$description %||% DEFAULT_METRIC_DESCRIPTION)
      )
    }
  }
  list(
    meta           = meta,
    category_order = if (is.null(raw$category_order)) character() else raw$category_order
  )
}


#' Get category for a metric name (default "Other").
metric_category <- function(metrics_meta, name) {
  entry <- metrics_meta$meta[[name]]
  if (is.null(entry)) DEFAULT_METRIC_CATEGORY else entry$category
}


#' Get direction for a metric name (default "positive").
metric_direction <- function(metrics_meta, name) {
  entry <- metrics_meta$meta[[name]]
  if (is.null(entry)) DEFAULT_METRIC_DIRECTION else entry$direction
}


#' Get description (tooltip text) for a metric name. Empty string if none.
metric_description <- function(metrics_meta, name) {
  entry <- metrics_meta$meta[[name]]
  if (is.null(entry)) DEFAULT_METRIC_DESCRIPTION else entry$description
}


#' Group metric names by category, respecting the declared category_order.
#'
#' @param metric_names  Character vector of metrics to group.
#' @param metrics_meta  Output of load_metrics_meta().
#' @return A named list, names = category, values = character vector
#'         of metric names in that category (alphabetized within).
group_by_category <- function(metric_names, metrics_meta) {
  cats <- vapply(metric_names, function(m) metric_category(metrics_meta, m),
                 character(1))
  grouped <- split(metric_names, cats)

  # Order: declared category_order first, then anything else alphabetically.
  known <- intersect(metrics_meta$category_order, names(grouped))
  rest  <- sort(setdiff(names(grouped), known))
  grouped[c(known, rest)]
}
