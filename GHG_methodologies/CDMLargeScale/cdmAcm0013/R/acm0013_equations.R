#' Baseline emissions under ACM0013
#'
#' Computes baseline emissions for ACM0013 projects by applying the combined
#' margin emission factor of the displaced grid generation to the monitored
#' electricity sent out by the project plant.
#'
#' @param electricity_generated_mwh Numeric vector of electricity delivered to
#'   the grid in megawatt-hours under the baseline scenario.
#' @param baseline_emission_factor_tco2_per_mwh Numeric vector of baseline grid
#'   emission factors expressed in tonnes of CO2 per megawatt-hour.
#' @return Numeric vector of baseline emissions in tonnes of CO2 equivalent.
#' @examples
#' calculate_baseline_emissions_acm0013(40000, 0.85)
#' calculate_baseline_emissions_acm0013(c(30000, 45000), baseline_emission_factor_tco2_per_mwh = 0.9)
#' @export
calculate_baseline_emissions_acm0013 <- function(electricity_generated_mwh,
                                                 baseline_emission_factor_tco2_per_mwh) {
  if (!is.numeric(electricity_generated_mwh) || !is.numeric(baseline_emission_factor_tco2_per_mwh)) {
    rlang::abort("All inputs must be numeric.")
  }
  if (any(electricity_generated_mwh < 0, na.rm = TRUE)) {
    rlang::abort("`electricity_generated_mwh` must be non-negative.")
  }
  if (any(baseline_emission_factor_tco2_per_mwh < 0, na.rm = TRUE)) {
    rlang::abort("`baseline_emission_factor_tco2_per_mwh` must be non-negative.")
  }

  electricity_generated_mwh * baseline_emission_factor_tco2_per_mwh
}

#' Project emissions under ACM0013
#'
#' Calculates project emissions for ACM0013 fossil-fuel plants using monitored
#' fuel use, auxiliary electricity, and other project emission sources.
#'
#' @param fuel_consumed Numeric vector of fossil fuel consumption in the units
#'   matching `fuel_emission_factor_tco2_per_unit`.
#' @param fuel_emission_factor_tco2_per_unit Numeric vector of emission factors
#'   for the consumed fuel expressed in tonnes of CO2 per fuel unit.
#' @param auxiliary_electricity_mwh Numeric vector of auxiliary electricity
#'   imported from the grid in megawatt-hours. Defaults to `0`.
#' @param auxiliary_emission_factor_tco2_per_mwh Numeric vector of emission
#'   factors for the auxiliary electricity in tonnes CO2 per MWh. Defaults to
#'   `0`.
#' @param other_project_emissions_tco2e Numeric vector of any additional project
#'   emissions (e.g. start-up fuels) already expressed in tonnes CO2e. Defaults
#'   to `0`.
#' @return Numeric vector of project emissions in tonnes CO2e.
#' @examples
#' calculate_project_emissions_acm0013(100000, 0.000074, auxiliary_electricity_mwh = 500,
#'   auxiliary_emission_factor_tco2_per_mwh = 0.9, other_project_emissions_tco2e = 25)
#' @export
calculate_project_emissions_acm0013 <- function(fuel_consumed,
                                                fuel_emission_factor_tco2_per_unit,
                                                auxiliary_electricity_mwh = 0,
                                                auxiliary_emission_factor_tco2_per_mwh = 0,
                                                other_project_emissions_tco2e = 0) {
  inputs <- list(
    fuel_consumed = fuel_consumed,
    fuel_emission_factor_tco2_per_unit = fuel_emission_factor_tco2_per_unit,
    auxiliary_electricity_mwh = auxiliary_electricity_mwh,
    auxiliary_emission_factor_tco2_per_mwh = auxiliary_emission_factor_tco2_per_mwh,
    other_project_emissions_tco2e = other_project_emissions_tco2e
  )

  purrr::iwalk(inputs, function(value, name) {
    if (!is.numeric(value)) {
      rlang::abort(sprintf("`%s` must be numeric.", name))
    }
  })

  if (any(fuel_consumed < 0, na.rm = TRUE)) {
    rlang::abort("`fuel_consumed` must be non-negative.")
  }
  if (any(fuel_emission_factor_tco2_per_unit < 0, na.rm = TRUE)) {
    rlang::abort("`fuel_emission_factor_tco2_per_unit` must be non-negative.")
  }
  if (any(auxiliary_electricity_mwh < 0, na.rm = TRUE)) {
    rlang::abort("`auxiliary_electricity_mwh` must be non-negative.")
  }
  if (any(auxiliary_emission_factor_tco2_per_mwh < 0, na.rm = TRUE)) {
    rlang::abort("`auxiliary_emission_factor_tco2_per_mwh` must be non-negative.")
  }
  if (any(other_project_emissions_tco2e < 0, na.rm = TRUE)) {
    rlang::abort("`other_project_emissions_tco2e` must be non-negative.")
  }

  fuel_emissions <- fuel_consumed * fuel_emission_factor_tco2_per_unit
  auxiliary_emissions <- auxiliary_electricity_mwh * auxiliary_emission_factor_tco2_per_mwh

  fuel_emissions + auxiliary_emissions + other_project_emissions_tco2e
}

#' Leakage emissions under ACM0013
#'
#' Aggregates leakage emissions that may arise from upstream fuel production or
#' the decommissioning of displaced units.
#'
#' @param upstream_fuel_emissions_tco2e Numeric vector of upstream fuel supply
#'   chain emissions attributable to the project, in tonnes CO2e. Defaults to `0`.
#' @param displaced_unit_emissions_tco2e Numeric vector of emissions from the
#'   decommissioning, retrofit, or continued operation of the displaced unit
#'   attributable to the project, expressed in tonnes CO2e. Defaults to `0`.
#' @param other_leakage_tco2e Numeric vector of any other leakage sources in
#'   tonnes CO2e. Defaults to `0`.
#' @return Numeric vector of leakage emissions in tonnes CO2e.
#' @examples
#' calculate_leakage_emissions_acm0013(5, 12, 3)
#' @export
calculate_leakage_emissions_acm0013 <- function(upstream_fuel_emissions_tco2e = 0,
                                                displaced_unit_emissions_tco2e = 0,
                                                other_leakage_tco2e = 0) {
  inputs <- list(
    upstream_fuel_emissions_tco2e = upstream_fuel_emissions_tco2e,
    displaced_unit_emissions_tco2e = displaced_unit_emissions_tco2e,
    other_leakage_tco2e = other_leakage_tco2e
  )

  purrr::iwalk(inputs, function(value, name) {
    if (!is.numeric(value)) {
      rlang::abort(sprintf("`%s` must be numeric.", name))
    }
    if (any(value < 0, na.rm = TRUE)) {
      rlang::abort(sprintf("`%s` must be non-negative.", name))
    }
  })

  upstream_fuel_emissions_tco2e + displaced_unit_emissions_tco2e + other_leakage_tco2e
}

#' Net emission reductions for ACM0013
#'
#' Computes the net emission reductions delivered by an ACM0013 project by
#' subtracting project and leakage emissions from the baseline emissions.
#'
#' @param baseline_emissions_tco2e Numeric vector of baseline emissions in tonnes
#'   CO2e.
#' @param project_emissions_tco2e Numeric vector of project emissions in tonnes
#'   CO2e.
#' @param leakage_emissions_tco2e Numeric vector of leakage emissions in tonnes
#'   CO2e.
#' @return Numeric vector of net emission reductions in tonnes CO2e.
#' @examples
#' calculate_net_emission_reductions_acm0013(50000, 42000, 1200)
#' @export
calculate_net_emission_reductions_acm0013 <- function(baseline_emissions_tco2e,
                                                      project_emissions_tco2e,
                                                      leakage_emissions_tco2e) {
  inputs <- list(
    baseline_emissions_tco2e = baseline_emissions_tco2e,
    project_emissions_tco2e = project_emissions_tco2e,
    leakage_emissions_tco2e = leakage_emissions_tco2e
  )

  purrr::iwalk(inputs, function(value, name) {
    if (!is.numeric(value)) {
      rlang::abort(sprintf("`%s` must be numeric.", name))
    }
  })

  baseline_emissions_tco2e - project_emissions_tco2e - leakage_emissions_tco2e
}
