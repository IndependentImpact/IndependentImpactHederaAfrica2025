#' Check AMS-I.B installed capacity threshold
#'
#' Validates the AMS-I.B applicability condition that renewable mechanical systems
#' must remain within the 15 MW small-scale threshold.
#'
#' @param capacity_kw Rated mechanical capacity in kilowatts (kW).
#' @param threshold_kw Threshold in kW, defaulting to 15000 (15 MW).
#' @return Logical indicating whether the capacity condition is satisfied.
#' @examples
#' check_applicability_mechanical_capacity(capacity_kw = 750)
#' check_applicability_mechanical_capacity(capacity_kw = 20000)
#' @export
check_applicability_mechanical_capacity <- function(capacity_kw, threshold_kw = 15000) {
  if (!is.numeric(capacity_kw) || length(capacity_kw) != 1) {
    stop("`capacity_kw` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(threshold_kw) || length(threshold_kw) != 1) {
    stop("`threshold_kw` must be a single numeric value.", call. = FALSE)
  }
  if (capacity_kw < 0) {
    stop("`capacity_kw` must be non-negative.", call. = FALSE)
  }

  capacity_kw <= threshold_kw
}

#' Check AMS-I.B renewable energy driver
#'
#' Ensures the mechanical equipment is predominantly driven by renewable energy sources
#' such as wind, water, or sustainably harvested biomass. The helper verifies the reported
#' renewable share against a user-supplied minimum fraction.
#'
#' @param renewable_fraction Share of mechanical energy provided by renewable sources (0-1).
#' @param minimum_fraction Minimum renewable share required for eligibility (default 0.9).
#' @return Logical indicating whether the renewable energy driver criterion is met.
#' @examples
#' check_applicability_renewable_driver(renewable_fraction = 0.95)
#' check_applicability_renewable_driver(renewable_fraction = 0.6)
#' @export
check_applicability_renewable_driver <- function(renewable_fraction, minimum_fraction = 0.9) {
  if (!is.numeric(renewable_fraction) || length(renewable_fraction) != 1) {
    stop("`renewable_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (renewable_fraction < 0 || renewable_fraction > 1) {
    stop("`renewable_fraction` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    stop("`minimum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  renewable_fraction >= minimum_fraction
}

#' Check AMS-I.B service displacement condition
#'
#' Confirms that the renewable mechanical system replaces baseline fossil-fuel equipment by
#' assessing the share of service output previously delivered by fossil energy.
#'
#' @param fossil_service_share Share of the baseline mechanical service provided by fossil fuels (0-1).
#' @param minimum_fraction Minimum share that must be displaced (default 0.5).
#' @return Logical indicating whether the displacement criterion is satisfied.
#' @examples
#' check_applicability_service_displacement(fossil_service_share = 0.8)
#' check_applicability_service_displacement(fossil_service_share = 0.3)
#' @export
check_applicability_service_displacement <- function(fossil_service_share, minimum_fraction = 0.5) {
  if (!is.numeric(fossil_service_share) || length(fossil_service_share) != 1) {
    stop("`fossil_service_share` must be a single numeric value.", call. = FALSE)
  }
  if (fossil_service_share < 0 || fossil_service_share > 1) {
    stop("`fossil_service_share` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    stop("`minimum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  fossil_service_share >= minimum_fraction
}
