#' Check that the activity remains within the Type I thermal capacity limit
#'
#' AMS-I.E applies to small-scale renewable thermal energy activities. This
#' helper enforces the 45 MW thermal equivalent capacity threshold defined for
#' Type I methodologies.
#'
#' @param thermal_capacity_mw Thermal capacity of the project in megawatts
#'   thermal (MWth).
#' @return `TRUE` when the capacity is less than or equal to 45 MWth and
#'   `FALSE` otherwise.
#' @examples
#' check_applicability_capacity_limit(thermal_capacity_mw = 20)
#' @export
check_applicability_capacity_limit <- function(thermal_capacity_mw) {
  if (!is.numeric(thermal_capacity_mw)) {
    stop("`thermal_capacity_mw` must be numeric.", call. = FALSE)
  }
  all(thermal_capacity_mw <= 45, na.rm = TRUE)
}

#' Check that non-renewable biomass is present in the baseline
#'
#' AMS-I.E requires that the displaced biomass is demonstrably
#' non-renewable. This helper verifies that the supplied fractions fall within
#' the 0–1 interval and that at least one observation indicates a strictly
#' positive non-renewable share.
#'
#' @param non_renewable_fraction Vector of non-renewable biomass fractions.
#' @return Logical scalar indicating whether the fractions are valid and contain
#'   a positive value.
#' @examples
#' check_applicability_non_renewable_fraction(c(0.8, 0.75))
#' @export
check_applicability_non_renewable_fraction <- function(non_renewable_fraction) {
  if (!is.numeric(non_renewable_fraction)) {
    stop("`non_renewable_fraction` must be numeric.", call. = FALSE)
  }
  if (length(non_renewable_fraction) == 0) {
    return(FALSE)
  }
  all(non_renewable_fraction >= 0 & non_renewable_fraction <= 1, na.rm = TRUE) &&
    any(non_renewable_fraction > 0, na.rm = TRUE)
}

#' Check that the project technology relies on renewable biomass
#'
#' Projects under AMS-I.E must replace non-renewable biomass with renewable
#' energy sources (e.g. sustainably harvested biomass or solar thermal). This
#' helper ensures the renewable fraction provided is sufficiently high.
#'
#' @param renewable_fraction Numeric vector describing the renewable fraction of
#'   project energy generation.
#' @param threshold Minimum acceptable renewable fraction (default 0.9).
#' @return Logical scalar equal to `TRUE` when the renewable fraction meets or
#'   exceeds `threshold`.
#' @examples
#' check_applicability_project_renewable_fraction(renewable_fraction = 0.95)
#' @export
check_applicability_project_renewable_fraction <- function(renewable_fraction,
                                                           threshold = 0.9) {
  if (!is.numeric(renewable_fraction)) {
    stop("`renewable_fraction` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1) {
    stop("`threshold` must be a single numeric value.", call. = FALSE)
  }
  all(renewable_fraction >= threshold, na.rm = TRUE)
}
