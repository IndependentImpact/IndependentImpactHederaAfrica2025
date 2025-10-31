#' Check AMS-II.C energy savings threshold
#'
#' Verifies the AMS-II.C small-scale eligibility criterion that annual energy
#' savings remain within the 60 GWh cap for Type II activities. Projects may
#' optionally provide a custom threshold should the UNFCCC update the limit.
#'
#' @param annual_energy_savings_mwh Estimated annual energy savings in MWh.
#' @param threshold_mwh Threshold in MWh (default 60 GWh expressed as 60,000 MWh).
#' @return Logical indicating whether the threshold is respected.
#' @examples
#' check_applicability_energy_savings(annual_energy_savings_mwh = 42000)
#' check_applicability_energy_savings(annual_energy_savings_mwh = 72000)
#' @export
check_applicability_energy_savings <- function(annual_energy_savings_mwh,
                                               threshold_mwh = 60000) {
  if (!is.numeric(annual_energy_savings_mwh) || length(annual_energy_savings_mwh) != 1) {
    stop("`annual_energy_savings_mwh` must be a single numeric value.", call. = FALSE)
  }
  if (annual_energy_savings_mwh < 0) {
    stop("`annual_energy_savings_mwh` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(threshold_mwh) || length(threshold_mwh) != 1) {
    stop("`threshold_mwh` must be a single numeric value.", call. = FALSE)
  }
  if (threshold_mwh <= 0) {
    stop("`threshold_mwh` must be positive.", call. = FALSE)
  }

  annual_energy_savings_mwh <= threshold_mwh
}

#' Check AMS-II.C technology eligibility
#'
#' Ensures that all technologies included in the project belong to the list of
#' measures covered by AMS-II.C. The default catalogue includes efficient
#' lighting, motors, HVAC upgrades, refrigeration, and other demand-side
#' efficiency interventions.
#'
#' @param technology Vector of technology identifiers.
#' @param allowed_technologies Character vector of eligible technologies.
#' @return Logical indicating whether every technology is permitted.
#' @examples
#' check_applicability_technology(c("efficient_lighting", "efficient_motors"))
#' check_applicability_technology(c("efficient_lighting", "biomass_boiler"))
#' @export
check_applicability_technology <- function(technology,
                                           allowed_technologies = c(
                                             "efficient_lighting",
                                             "efficient_motors",
                                             "hvac_optimization",
                                             "refrigeration_efficiency",
                                             "variable_speed_drives",
                                             "compressed_air_improvement"
                                           )) {
  if (!is.character(technology)) {
    stop("`technology` must be a character vector.", call. = FALSE)
  }
  if (!is.character(allowed_technologies) || length(allowed_technologies) == 0) {
    stop("`allowed_technologies` must be a non-empty character vector.", call. = FALSE)
  }

  all(technology %in% allowed_technologies)
}

#' Check AMS-II.C monitoring approach
#'
#' Confirms that the monitoring arrangement adheres to AMS-II.C requirements by
#' validating the stated measurement approach. Typical options include continuous
#' metering, calibrated sampling campaigns, or manufacturer data corroborated by
#' spot measurements.
#'
#' @param monitoring_approach Character string describing the monitoring method.
#' @param allowed_approaches Vector of accepted monitoring approaches.
#' @return Logical indicating whether the monitoring approach is acceptable.
#' @examples
#' check_applicability_monitoring("continuous_metering")
#' check_applicability_monitoring("engineering_estimate")
#' @export
check_applicability_monitoring <- function(monitoring_approach,
                                           allowed_approaches = c(
                                             "continuous_metering",
                                             "periodic_sampling",
                                             "calibrated_nameplate"
                                           )) {
  if (!is.character(monitoring_approach) || length(monitoring_approach) != 1) {
    stop("`monitoring_approach` must be a single character value.", call. = FALSE)
  }
  if (!is.character(allowed_approaches) || length(allowed_approaches) == 0) {
    stop("`allowed_approaches` must be a non-empty character vector.", call. = FALSE)
  }

  monitoring_approach %in% allowed_approaches
}

#' Assess combined AMS-II.C applicability conditions
#'
#' Aggregates key quantitative applicability checks for AMS-II.C demand-side
#' energy efficiency projects, returning a tidy summary of results for audit
#' documentation.
#'
#' @param annual_energy_savings_mwh Estimated annual energy savings in MWh.
#' @param technologies Character vector describing deployed technologies.
#' @param monitoring_approach Character string naming the monitoring method.
#' @param threshold_mwh Maximum allowed annual energy savings (default 60,000 MWh).
#' @param allowed_technologies Eligible technologies (default provided list).
#' @param allowed_approaches Acceptable monitoring approaches (default provided list).
#' @return A tibble with each check and whether it passes.
#' @examples
#' assess_ams_iic_applicability(
#'   annual_energy_savings_mwh = 42000,
#'   technologies = c("efficient_lighting", "variable_speed_drives"),
#'   monitoring_approach = "continuous_metering"
#' )
#' @export
assess_ams_iic_applicability <- function(annual_energy_savings_mwh,
                                         technologies,
                                         monitoring_approach,
                                         threshold_mwh = 60000,
                                         allowed_technologies = c(
                                           "efficient_lighting",
                                           "efficient_motors",
                                           "hvac_optimization",
                                           "refrigeration_efficiency",
                                           "variable_speed_drives",
                                           "compressed_air_improvement"
                                         ),
                                         allowed_approaches = c(
                                           "continuous_metering",
                                           "periodic_sampling",
                                           "calibrated_nameplate"
                                         )) {
  tibble::tibble(
    condition = c(
      "Energy savings within small-scale threshold",
      "Technologies eligible under AMS-II.C",
      "Monitoring approach compliant"
    ),
    passes = c(
      check_applicability_energy_savings(annual_energy_savings_mwh, threshold_mwh),
      check_applicability_technology(technologies, allowed_technologies),
      check_applicability_monitoring(monitoring_approach, allowed_approaches)
    )
  )
}
