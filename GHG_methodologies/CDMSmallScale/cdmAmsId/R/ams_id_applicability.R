#' Check AMS-I.D mini-grid capacity threshold
#'
#' Validates the AMS-I.D applicability condition that renewable electricity
#' systems supplying a captive mini-grid remain within the 15 MW small-scale
#' limit for Type I CDM project activities.
#'
#' @param capacity_kw Rated electrical capacity in kilowatts (kW).
#' @param threshold_kw Threshold in kW, defaulting to 15000 (15 MW).
#' @return Logical indicating whether the capacity condition is satisfied.
#' @examples
#' check_applicability_mini_grid_capacity(capacity_kw = 8000)
#' check_applicability_mini_grid_capacity(capacity_kw = 25000)
#' @export
check_applicability_mini_grid_capacity <- function(capacity_kw, threshold_kw = 15000) {
  if (!is.numeric(capacity_kw) || length(capacity_kw) != 1) {
    stop("`capacity_kw` must be a single numeric value.", call. = FALSE)
  }
  if (capacity_kw < 0) {
    stop("`capacity_kw` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(threshold_kw) || length(threshold_kw) != 1) {
    stop("`threshold_kw` must be a single numeric value.", call. = FALSE)
  }
  if (threshold_kw <= 0) {
    stop("`threshold_kw` must be positive.", call. = FALSE)
  }

  capacity_kw <= threshold_kw
}

#' Check AMS-I.D renewable penetration of supply
#'
#' Ensures the captive mini-grid is predominantly supplied by renewable energy
#' resources (e.g. solar PV, wind, small hydro). The helper compares the
#' reported renewable share to a user-defined minimum fraction.
#'
#' @param renewable_fraction Share of total electricity supplied by renewable sources (0-1).
#' @param minimum_fraction Minimum renewable share required for eligibility (default 0.75).
#' @return Logical indicating whether the renewable supply criterion is met.
#' @examples
#' check_applicability_renewable_penetration(renewable_fraction = 0.85)
#' check_applicability_renewable_penetration(renewable_fraction = 0.6)
#' @export
check_applicability_renewable_penetration <- function(renewable_fraction, minimum_fraction = 0.75) {
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

#' Check AMS-I.D captive demand displacement
#'
#' Confirms the renewable mini-grid displaces fossil-fuel-based captive
#' generation by evaluating the share of baseline electricity served by fossil
#' resources.
#'
#' @param baseline_fossil_share Share of baseline electricity generated using fossil fuels (0-1).
#' @param minimum_fraction Minimum fossil share that must be displaced (default 0.5).
#' @return Logical indicating whether the displacement criterion is satisfied.
#' @examples
#' check_applicability_baseline_fossil_share(baseline_fossil_share = 0.8)
#' check_applicability_baseline_fossil_share(baseline_fossil_share = 0.3)
#' @export
check_applicability_baseline_fossil_share <- function(baseline_fossil_share, minimum_fraction = 0.5) {
  if (!is.numeric(baseline_fossil_share) || length(baseline_fossil_share) != 1) {
    stop("`baseline_fossil_share` must be a single numeric value.", call. = FALSE)
  }
  if (baseline_fossil_share < 0 || baseline_fossil_share > 1) {
    stop("`baseline_fossil_share` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    stop("`minimum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  baseline_fossil_share >= minimum_fraction
}
