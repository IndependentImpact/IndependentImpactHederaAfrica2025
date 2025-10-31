#' Check AMS-I.J solar thermal capacity threshold
#'
#' Validates the AMS-I.J applicability condition that small-scale solar water
#' heating systems remain within the 45 MW thermal limit for Type I activities.
#'
#' @param capacity_mwth Rated thermal capacity of the solar water heating system
#'   in megawatts thermal (MWth).
#' @param threshold_mwth Threshold in MWth, defaulting to 45.
#' @return Logical indicating whether the capacity condition is satisfied.
#' @examples
#' check_applicability_swh_capacity(capacity_mwth = 12)
#' check_applicability_swh_capacity(capacity_mwth = 60)
#' @export
check_applicability_swh_capacity <- function(capacity_mwth, threshold_mwth = 45) {
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

#' Check AMS-I.J minimum solar resource availability
#'
#' Ensures the project is implemented in a location with sufficient solar
#' irradiation to support solar water heating system performance. The helper
#' compares the reported annual global horizontal irradiation to a minimum value
#' representing viable solar resource potential.
#'
#' @param annual_irradiation_kwhm2 Annual solar irradiation in kilowatt-hours per
#'   square metre (kWh/m^2).
#' @param minimum_irradiation Minimum irradiation level required for eligibility
#'   in kWh/m^2 (default 1200).
#' @return Logical indicating whether the solar resource criterion is met.
#' @examples
#' check_applicability_solar_resource(annual_irradiation_kwhm2 = 1800)
#' check_applicability_solar_resource(annual_irradiation_kwhm2 = 950)
#' @export
check_applicability_solar_resource <- function(annual_irradiation_kwhm2,
                                               minimum_irradiation = 1200) {
  if (!is.numeric(annual_irradiation_kwhm2) || length(annual_irradiation_kwhm2) != 1) {
    stop("`annual_irradiation_kwhm2` must be a single numeric value.", call. = FALSE)
  }
  if (annual_irradiation_kwhm2 < 0) {
    stop("`annual_irradiation_kwhm2` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(minimum_irradiation) || length(minimum_irradiation) != 1) {
    stop("`minimum_irradiation` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_irradiation < 0) {
    stop("`minimum_irradiation` must be non-negative.", call. = FALSE)
  }

  annual_irradiation_kwhm2 >= minimum_irradiation
}

#' Check AMS-I.J auxiliary energy contribution
#'
#' Confirms that auxiliary or backup fossil fuel energy does not dominate the
#' solar water heating system. The helper evaluates the share of useful thermal
#' output provided by auxiliary sources and compares it to a maximum allowable
#' fraction.
#'
#' @param backup_fraction Fraction of useful thermal output supplied by backup or
#'   auxiliary fossil fuel systems (0-1).
#' @param maximum_fraction Maximum allowable backup fraction (default 0.2).
#' @return Logical indicating whether the auxiliary energy criterion is
#'   satisfied.
#' @examples
#' check_applicability_backup_fraction(backup_fraction = 0.1)
#' check_applicability_backup_fraction(backup_fraction = 0.35)
#' @export
check_applicability_backup_fraction <- function(backup_fraction, maximum_fraction = 0.2) {
  if (!is.numeric(backup_fraction) || length(backup_fraction) != 1) {
    stop("`backup_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (backup_fraction < 0 || backup_fraction > 1) {
    stop("`backup_fraction` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(maximum_fraction) || length(maximum_fraction) != 1) {
    stop("`maximum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (maximum_fraction < 0 || maximum_fraction > 1) {
    stop("`maximum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  backup_fraction <= maximum_fraction
}

#' Assess combined AMS-I.J applicability conditions
#'
#' Evaluates primary quantitative applicability criteria for AMS-I.J solar water
#' heating projects, returning a tibble summarising each test and whether it
#' passed. Use the helper to create a transparent record of the applicability
#' assessment.
#'
#' @param capacity_mwth Rated solar thermal capacity in MWth.
#' @param annual_irradiation_kwhm2 Annual solar irradiation in kWh/m^2.
#' @param backup_fraction Fraction of useful thermal output provided by auxiliary systems.
#' @param capacity_threshold_mwth Capacity threshold in MWth (default 45).
#' @param irradiation_minimum_kwhm2 Minimum annual irradiation required (default 1200).
#' @param backup_maximum_fraction Maximum allowable auxiliary fraction (default 0.2).
#' @return A tibble with one row per applicability condition and a `passes` column.
#' @examples
#' assess_ams_ij_applicability(
#'   capacity_mwth = 20,
#'   annual_irradiation_kwhm2 = 1900,
#'   backup_fraction = 0.1
#' )
#' @export
assess_ams_ij_applicability <- function(capacity_mwth,
                                        annual_irradiation_kwhm2,
                                        backup_fraction,
                                        capacity_threshold_mwth = 45,
                                        irradiation_minimum_kwhm2 = 1200,
                                        backup_maximum_fraction = 0.2) {
  tibble::tibble(
    condition = c(
      "Capacity below small-scale threshold",
      "Solar resource sufficient",
      "Auxiliary fraction within limits"
    ),
    passes = c(
      check_applicability_swh_capacity(capacity_mwth, capacity_threshold_mwth),
      check_applicability_solar_resource(annual_irradiation_kwhm2, irradiation_minimum_kwhm2),
      check_applicability_backup_fraction(backup_fraction, backup_maximum_fraction)
    )
  )
}
