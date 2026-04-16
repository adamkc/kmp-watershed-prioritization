# Load and validate prebuilt scenario configurations from data/scenarios.yaml.
#
# A scenario defines a named set of metrics and their starting weights.
# The app renders one button per scenario; clicking loads that scenario's
# metrics into the active list and initializes sliders to its weights.

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Load all scenarios from a YAML file.
#'
#' @return A list of scenario objects, each with $id, $name,
#'         $description, and $weights (named numeric vector).
load_scenarios <- function(path = "data/scenarios.yaml") {
  if (!file.exists(path)) {
    stop("Scenario file not found: ", path, call. = FALSE)
  }
  raw <- yaml::read_yaml(path)
  if (is.null(raw$scenarios) || length(raw$scenarios) == 0) {
    return(list())
  }
  lapply(raw$scenarios, function(s) {
    weights <- if (is.null(s$weights)) numeric() else unlist(s$weights)
    storage.mode(weights) <- "double"
    list(
      id          = s$id,
      name        = s$name,
      description = trimws(s$description %||% ""),
      weights     = weights
    )
  })
}


#' Validate scenarios against the master metrics table.
#'
#' Reports any scenario that references metrics not present in the
#' master data. Doesn't stop -- callers decide how to handle.
#'
#' @param scenarios          Output of load_scenarios().
#' @param available_metrics  Character vector of valid column names.
#' @return list(ok, issues) -- issues is a character vector.
validate_scenarios <- function(scenarios, available_metrics) {
  issues <- character()
  for (s in scenarios) {
    missing <- setdiff(names(s$weights), available_metrics)
    if (length(missing) > 0) {
      issues <- c(issues, sprintf(
        "Scenario '%s' references unknown metric(s): %s",
        s$id, paste(missing, collapse = ", ")
      ))
    }
    if (length(s$weights) == 0) {
      issues <- c(issues, sprintf("Scenario '%s' has no weights defined.", s$id))
    }
  }
  list(ok = length(issues) == 0, issues = issues)
}


#' Look up a scenario by id.
get_scenario <- function(scenarios, id) {
  for (s in scenarios) if (identical(s$id, id)) return(s)
  NULL
}
