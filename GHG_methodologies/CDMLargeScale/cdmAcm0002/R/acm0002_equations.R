#' Calculate net electricity generation for ACM0002
#'
#' Computes the net electricity supplied to the grid by subtracting auxiliary
#' consumption from gross generation, aligning with ACM0002 equation (1).
#'
#' @param gross_generation_mwh Gross electricity generated during the
#'   monitoring period in megawatt-hours.
#' @param auxiliary_consumption_mwh Auxiliary electricity consumption within
#'   the plant in megawatt-hours.
#' @return Numeric vector of net electricity exported to the grid.
#' @examples
#' calculate_net_electricity_generation(1000, 50)
#' @export
calculate_net_electricity_generation <- function(gross_generation_mwh,
                                                 auxiliary_consumption_mwh) {
  if (!is.numeric(gross_generation_mwh) || any(gross_generation_mwh < 0)) {
    rlang::abort("`gross_generation_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(auxiliary_consumption_mwh) || any(auxiliary_consumption_mwh < 0)) {
    rlang::abort("`auxiliary_consumption_mwh` must be a non-negative numeric vector.")
  }
  if (!(length(auxiliary_consumption_mwh) %in% c(1, length(gross_generation_mwh)))) {
    rlang::abort("`auxiliary_consumption_mwh` must be length 1 or match `gross_generation_mwh`.")
  }

  gross_generation_mwh - auxiliary_consumption_mwh
}

#' Calculate baseline emissions for ACM0002
#'
#' Applies the combined margin emission factor to net electricity exported to
#' estimate baseline emissions per ACM0002.
#'
#' @param net_electricity_mwh Net electricity delivered to the grid in MWh.
#' @param combined_margin_ef Emission factor in tonnes CO2e per MWh for the
#'   grid's combined margin.
#' @return Numeric vector of baseline emissions in tonnes of CO2e.
#' @examples
#' calculate_baseline_emissions(950, 0.8)
#' @export
calculate_baseline_emissions <- function(net_electricity_mwh,
                                         combined_margin_ef) {
  if (!is.numeric(net_electricity_mwh) || any(net_electricity_mwh < 0)) {
    rlang::abort("`net_electricity_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(combined_margin_ef) || any(combined_margin_ef < 0)) {
    rlang::abort("`combined_margin_ef` must be a non-negative numeric vector.")
  }
  if (!(length(combined_margin_ef) %in% c(1, length(net_electricity_mwh)))) {
    rlang::abort("`combined_margin_ef` must be length 1 or match `net_electricity_mwh`.")
  }

  net_electricity_mwh * combined_margin_ef
}

#' Calculate project emissions for ACM0002
#'
#' Computes project emissions from auxiliary fossil fuel consumption and
#' electricity imports, capturing ACM0002's treatment of fossil back-up
#' systems.
#'
#' @param fossil_fuel_tj Fossil fuel use in terajoules.
#' @param fossil_emission_factor Emission factor in tonnes CO2e per terajoule.
#' @param electricity_import_mwh Electricity imported from the grid in MWh.
#' @param import_emission_factor Emission factor for imported electricity in
#'   tonnes CO2e per MWh.
#' @return Numeric vector of project emissions in tonnes CO2e.
#' @examples
#' calculate_project_emissions(0.1, 74, 10, 0.8)
#' @export
calculate_project_emissions <- function(fossil_fuel_tj,
                                        fossil_emission_factor,
                                        electricity_import_mwh = 0,
                                        import_emission_factor = 0) {
  if (!is.numeric(fossil_fuel_tj) || any(fossil_fuel_tj < 0)) {
    rlang::abort("`fossil_fuel_tj` must be a non-negative numeric vector.")
  }
  if (!is.numeric(fossil_emission_factor) || any(fossil_emission_factor < 0)) {
    rlang::abort("`fossil_emission_factor` must be a non-negative numeric vector.")
  }
  if (!(length(fossil_emission_factor) %in% c(1, length(fossil_fuel_tj)))) {
    rlang::abort("`fossil_emission_factor` must be length 1 or match `fossil_fuel_tj`.")
  }
  if (!is.numeric(electricity_import_mwh) || any(electricity_import_mwh < 0)) {
    rlang::abort("`electricity_import_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(import_emission_factor) || any(import_emission_factor < 0)) {
    rlang::abort("`import_emission_factor` must be a non-negative numeric vector.")
  }
  if (!(length(import_emission_factor) %in% c(1, length(electricity_import_mwh)))) {
    rlang::abort("`import_emission_factor` must be length 1 or match `electricity_import_mwh`.")
  }

  fossil_fuel_tj * fossil_emission_factor + electricity_import_mwh * import_emission_factor
}

#' Calculate emission reductions for ACM0002
#'
#' Derives emission reductions by subtracting project and leakage emissions
#' from baseline emissions.
#'
#' @param baseline_emissions Baseline emissions in tonnes CO2e.
#' @param project_emissions Project emissions in tonnes CO2e.
#' @param leakage_emissions Leakage emissions in tonnes CO2e. Defaults to zero.
#' @return Numeric vector of emission reductions in tonnes CO2e.
#' @examples
#' calculate_emission_reductions(800, 25, 5)
#' @export
calculate_emission_reductions <- function(baseline_emissions,
                                          project_emissions,
                                          leakage_emissions = 0) {
  if (!is.numeric(baseline_emissions)) {
    rlang::abort("`baseline_emissions` must be numeric.")
  }
  if (!is.numeric(project_emissions)) {
    rlang::abort("`project_emissions` must be numeric.")
  }
  if (!is.numeric(leakage_emissions)) {
    rlang::abort("`leakage_emissions` must be numeric.")
  }

  baseline_emissions - project_emissions - leakage_emissions
}
