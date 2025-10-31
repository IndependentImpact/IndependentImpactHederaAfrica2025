#' Check AMS-I.C thermal capacity threshold
#'
#' Validates the AMS-I.C applicability condition that thermal energy systems remain
#' within the 45 MW thermal small-scale limit established for Type I CDM projects.
#'
#' @param capacity_mwth Rated thermal capacity in megawatts thermal (MWth).
#' @param threshold_mwth Threshold in MWth, defaulting to 45.
#' @return Logical indicating whether the capacity condition is satisfied.
#' @examples
#' check_applicability_thermal_capacity(capacity_mwth = 10)
#' check_applicability_thermal_capacity(capacity_mwth = 60)
#' @export
check_applicability_thermal_capacity <- function(capacity_mwth, threshold_mwth = 45) {
  if (!is.numeric(capacity_mwth) || length(capacity_mwth) != 1) {
    stop("`capacity_mwth` must be a single numeric value.", call. = FALSE)
  }
  if (capacity_mwth < 0) {
    stop("`capacity_mwth` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(threshold_mwth) || length(threshold_mwth) != 1) {
    stop("`threshold_mwth` must be a single numeric value.", call. = FALSE)
  }
  if (threshold_mwth <= 0) {
    stop("`threshold_mwth` must be positive.", call. = FALSE)
  }

  capacity_mwth <= threshold_mwth
}

#' Check AMS-I.C renewable thermal supply share
#'
#' Ensures the thermal energy system is predominantly supplied by renewable energy
#' resources such as biomass, solar thermal, or geothermal heat. The helper compares
#' the reported renewable share to a user-defined minimum fraction.
#'
#' @param renewable_fraction Share of total useful thermal energy supplied by renewable sources (0-1).
#' @param minimum_fraction Minimum renewable share required for eligibility (default 0.75).
#' @return Logical indicating whether the renewable supply criterion is met.
#' @examples
#' check_applicability_renewable_supply(renewable_fraction = 0.85)
#' check_applicability_renewable_supply(renewable_fraction = 0.6)
#' @export
check_applicability_renewable_supply <- function(renewable_fraction, minimum_fraction = 0.75) {
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

#' Check AMS-I.C fossil fuel displacement
#'
#' Confirms the project activity displaces fossil-fuel-based thermal energy by testing
#' the share of baseline heat supplied by fossil resources.
#'
#' @param fossil_heat_share Share of baseline thermal energy generated using fossil fuels (0-1).
#' @param minimum_fraction Minimum fossil share that must be displaced (default 0.5).
#' @return Logical indicating whether the displacement criterion is satisfied.
#' @examples
#' check_applicability_fossil_displacement(fossil_heat_share = 0.8)
#' check_applicability_fossil_displacement(fossil_heat_share = 0.3)
#' @export
check_applicability_fossil_displacement <- function(fossil_heat_share, minimum_fraction = 0.5) {
  if (!is.numeric(fossil_heat_share) || length(fossil_heat_share) != 1) {
    stop("`fossil_heat_share` must be a single numeric value.", call. = FALSE)
  }
  if (fossil_heat_share < 0 || fossil_heat_share > 1) {
    stop("`fossil_heat_share` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    stop("`minimum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  fossil_heat_share >= minimum_fraction
}
