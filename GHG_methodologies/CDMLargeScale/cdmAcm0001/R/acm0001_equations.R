#' Convert methane volume to mass under standard conditions
#'
#' Translates captured landfill gas methane volume into tonnes of methane using
#' the ACM0001 default density of 0.000716 tonnes per cubic metre.
#'
#' @param methane_volume_m3 Numeric vector of methane volumes in cubic metres.
#' @param methane_density_t_per_m3 Methane density in tonnes per cubic metre.
#'   Defaults to 0.000716 t/m3 consistent with ACM0001 guidance.
#' @return Numeric vector of methane mass in tonnes.
#' @examples
#' convert_methane_volume_to_mass(1000)
#' @export
convert_methane_volume_to_mass <- function(methane_volume_m3,
                                           methane_density_t_per_m3 = 0.000716) {
  if (!is.numeric(methane_volume_m3) || any(methane_volume_m3 < 0)) {
    rlang::abort("`methane_volume_m3` must be a non-negative numeric vector.")
  }
  if (!is.numeric(methane_density_t_per_m3) || any(methane_density_t_per_m3 <= 0)) {
    rlang::abort("`methane_density_t_per_m3` must contain positive numeric values.")
  }
  if (!(length(methane_density_t_per_m3) %in% c(1, length(methane_volume_m3)))) {
    rlang::abort("`methane_density_t_per_m3` must be length 1 or match `methane_volume_m3`.")
  }

  methane_volume_m3 * methane_density_t_per_m3
}

#' Calculate ACM0001 baseline emissions from landfill methane
#'
#' Estimates baseline greenhouse gas emissions assuming methane not destroyed
#' by the project is released to the atmosphere while allowing for methane
#' oxidation in the landfill cover.
#'
#' @param methane_generation_m3 Methane generated during the period in cubic
#'   metres.
#' @param baseline_capture_efficiency Fraction of methane that would be captured
#'   in the baseline scenario. Defaults to 0 (no capture).
#' @param oxidation_fraction Fraction of methane oxidised in the landfill cover
#'   in the absence of the project. Defaults to 0.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#' @param gwp_ch4 Global warming potential for methane. Defaults to 28 (AR5).
#' @return Numeric vector of baseline emissions in tonnes CO2e.
#' @examples
#' calculate_baseline_emissions_acm0001(5000)
#' calculate_baseline_emissions_acm0001(5000, oxidation_fraction = 0.1)
#' @export
calculate_baseline_emissions_acm0001 <- function(methane_generation_m3,
                                                 baseline_capture_efficiency = 0,
                                                 oxidation_fraction = 0,
                                                 methane_density_t_per_m3 = 0.000716,
                                                 gwp_ch4 = 28) {
  if (!is.numeric(methane_generation_m3) || any(methane_generation_m3 < 0)) {
    rlang::abort("`methane_generation_m3` must be a non-negative numeric vector.")
  }
  if (!is.numeric(baseline_capture_efficiency) ||
      any(baseline_capture_efficiency < 0 | baseline_capture_efficiency > 1)) {
    rlang::abort("`baseline_capture_efficiency` must be numeric between 0 and 1.")
  }
  if (!(length(baseline_capture_efficiency) %in% c(1, length(methane_generation_m3)))) {
    rlang::abort("`baseline_capture_efficiency` must be length 1 or match `methane_generation_m3`.")
  }
  if (!is.numeric(oxidation_fraction) ||
      any(oxidation_fraction < 0 | oxidation_fraction > 1)) {
    rlang::abort("`oxidation_fraction` must be numeric between 0 and 1.")
  }
  if (!(length(oxidation_fraction) %in% c(1, length(methane_generation_m3)))) {
    rlang::abort("`oxidation_fraction` must be length 1 or match `methane_generation_m3`.")
  }
  if (!is.numeric(methane_density_t_per_m3) || any(methane_density_t_per_m3 <= 0)) {
    rlang::abort("`methane_density_t_per_m3` must contain positive numeric values.")
  }
  if (!(length(methane_density_t_per_m3) %in% c(1, length(methane_generation_m3)))) {
    rlang::abort("`methane_density_t_per_m3` must be length 1 or match `methane_generation_m3`.")
  }
  if (!is.numeric(gwp_ch4) || any(gwp_ch4 <= 0)) {
    rlang::abort("`gwp_ch4` must contain positive numeric values.")
  }
  if (!(length(gwp_ch4) %in% c(1, length(methane_generation_m3)))) {
    rlang::abort("`gwp_ch4` must be length 1 or match `methane_generation_m3`.")
  }

  methane_released_m3 <- methane_generation_m3 * (1 - baseline_capture_efficiency) *
    (1 - oxidation_fraction)
  convert_methane_volume_to_mass(methane_released_m3, methane_density_t_per_m3) * gwp_ch4
}

#' Calculate methane destroyed under ACM0001
#'
#' Converts captured methane and destruction efficiency into methane mass
#' destroyed by flaring or utilisation systems.
#'
#' @param methane_captured_m3 Methane captured by the project in cubic metres.
#' @param destruction_efficiency Fraction of captured methane that is destroyed
#'   through flaring or utilisation.
#' @param methane_density_t_per_m3 Methane density in tonnes per cubic metre.
#' @return Numeric vector of methane mass destroyed in tonnes.
#' @examples
#' calculate_methane_destroyed_acm0001(4000, 0.9)
#' @export
calculate_methane_destroyed_acm0001 <- function(methane_captured_m3,
                                                destruction_efficiency,
                                                methane_density_t_per_m3 = 0.000716) {
  if (!is.numeric(methane_captured_m3) || any(methane_captured_m3 < 0)) {
    rlang::abort("`methane_captured_m3` must be a non-negative numeric vector.")
  }
  if (!is.numeric(destruction_efficiency) ||
      any(destruction_efficiency < 0 | destruction_efficiency > 1)) {
    rlang::abort("`destruction_efficiency` must be numeric between 0 and 1.")
  }
  if (!(length(destruction_efficiency) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`destruction_efficiency` must be length 1 or match `methane_captured_m3`.")
  }
  if (!is.numeric(methane_density_t_per_m3) || any(methane_density_t_per_m3 <= 0)) {
    rlang::abort("`methane_density_t_per_m3` must contain positive numeric values.")
  }
  if (!(length(methane_density_t_per_m3) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`methane_density_t_per_m3` must be length 1 or match `methane_captured_m3`.")
  }

  destroyed_m3 <- methane_captured_m3 * destruction_efficiency
  convert_methane_volume_to_mass(destroyed_m3, methane_density_t_per_m3)
}

#' Express methane destruction as CO2e under ACM0001
#'
#' Calculates the greenhouse gas impact of methane destruction by combining
#' methane mass destroyed with the methane global warming potential.
#'
#' @inheritParams calculate_methane_destroyed_acm0001
#' @param gwp_ch4 Global warming potential of methane. Defaults to 28 (AR5).
#' @return Numeric vector of methane destruction expressed in tonnes CO2e.
#' @examples
#' calculate_methane_destruction_co2e_acm0001(4000, 0.9)
#' @export
calculate_methane_destruction_co2e_acm0001 <- function(methane_captured_m3,
                                                       destruction_efficiency,
                                                       methane_density_t_per_m3 = 0.000716,
                                                       gwp_ch4 = 28) {
  destroyed_mass <- calculate_methane_destroyed_acm0001(
    methane_captured_m3,
    destruction_efficiency,
    methane_density_t_per_m3
  )

  if (!is.numeric(gwp_ch4) || any(gwp_ch4 <= 0)) {
    rlang::abort("`gwp_ch4` must contain positive numeric values.")
  }
  if (!(length(gwp_ch4) %in% c(1, length(destroyed_mass)))) {
    rlang::abort("`gwp_ch4` must be length 1 or match `methane_captured_m3`.")
  }

  destroyed_mass * gwp_ch4
}

#' Calculate ACM0001 project emissions
#'
#' Computes project emissions from methane slip, auxiliary fossil fuel use, and
#' electricity imports.
#'
#' @param methane_captured_m3 Methane captured by the project in cubic metres.
#' @param destruction_efficiency Fraction of captured methane destroyed (flared
#'   or utilised).
#' @param methane_density_t_per_m3 Methane density in tonnes per cubic metre.
#' @param gwp_ch4 Global warming potential of methane.
#' @param auxiliary_fuel_tj Auxiliary fossil fuel consumption in terajoules.
#' @param auxiliary_ef_t_per_tj Emission factor for auxiliary fuel in tonnes
#'   CO2e per terajoule.
#' @param electricity_import_mwh Electricity imported from the grid in MWh.
#' @param import_ef_t_per_mwh Emission factor for imported electricity in tonnes
#'   CO2e per MWh.
#' @return Numeric vector of project emissions in tonnes CO2e.
#' @examples
#' calculate_project_emissions_acm0001(4000, 0.9, auxiliary_fuel_tj = 0.01,
#'                                     auxiliary_ef_t_per_tj = 74)
#' @export
calculate_project_emissions_acm0001 <- function(methane_captured_m3,
                                                destruction_efficiency,
                                                methane_density_t_per_m3 = 0.000716,
                                                gwp_ch4 = 28,
                                                auxiliary_fuel_tj = 0,
                                                auxiliary_ef_t_per_tj = 0,
                                                electricity_import_mwh = 0,
                                                import_ef_t_per_mwh = 0) {
  if (!is.numeric(methane_captured_m3) || any(methane_captured_m3 < 0)) {
    rlang::abort("`methane_captured_m3` must be a non-negative numeric vector.")
  }
  if (!is.numeric(destruction_efficiency) ||
      any(destruction_efficiency < 0 | destruction_efficiency > 1)) {
    rlang::abort("`destruction_efficiency` must be numeric between 0 and 1.")
  }
  if (!(length(destruction_efficiency) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`destruction_efficiency` must be length 1 or match `methane_captured_m3`.")
  }
  if (!is.numeric(methane_density_t_per_m3) || any(methane_density_t_per_m3 <= 0)) {
    rlang::abort("`methane_density_t_per_m3` must contain positive numeric values.")
  }
  if (!(length(methane_density_t_per_m3) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`methane_density_t_per_m3` must be length 1 or match `methane_captured_m3`.")
  }
  if (!is.numeric(gwp_ch4) || any(gwp_ch4 <= 0)) {
    rlang::abort("`gwp_ch4` must contain positive numeric values.")
  }
  if (!(length(gwp_ch4) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`gwp_ch4` must be length 1 or match `methane_captured_m3`.")
  }
  if (!is.numeric(auxiliary_fuel_tj) || any(auxiliary_fuel_tj < 0)) {
    rlang::abort("`auxiliary_fuel_tj` must be a non-negative numeric vector.")
  }
  if (!is.numeric(auxiliary_ef_t_per_tj) || any(auxiliary_ef_t_per_tj < 0)) {
    rlang::abort("`auxiliary_ef_t_per_tj` must be a non-negative numeric vector.")
  }
  if (!(length(auxiliary_ef_t_per_tj) %in% c(1, length(auxiliary_fuel_tj)))) {
    rlang::abort("`auxiliary_ef_t_per_tj` must be length 1 or match `auxiliary_fuel_tj`.")
  }
  if (!is.numeric(electricity_import_mwh) || any(electricity_import_mwh < 0)) {
    rlang::abort("`electricity_import_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(import_ef_t_per_mwh) || any(import_ef_t_per_mwh < 0)) {
    rlang::abort("`import_ef_t_per_mwh` must be a non-negative numeric vector.")
  }
  if (!(length(import_ef_t_per_mwh) %in% c(1, length(electricity_import_mwh)))) {
    rlang::abort("`import_ef_t_per_mwh` must be length 1 or match `electricity_import_mwh`.")
  }

  methane_slip_m3 <- methane_captured_m3 * (1 - destruction_efficiency)
  methane_slip_mass_t <- convert_methane_volume_to_mass(methane_slip_m3, methane_density_t_per_m3)
  methane_slip_mass_t * gwp_ch4 + auxiliary_fuel_tj * auxiliary_ef_t_per_tj +
    electricity_import_mwh * import_ef_t_per_mwh
}

#' Calculate ACM0001 leakage emissions
#'
#' Estimates leakage from increased methane generation in upstream waste
#' handling or fossil fuel displacement effects captured in ACM0001.
#'
#' @param leakage_fraction Fraction of captured methane attributed to leakage
#'   pathways (e.g. due to waste diversion).
#' @param methane_captured_m3 Methane captured in cubic metres.
#' @param methane_density_t_per_m3 Methane density in tonnes per cubic metre.
#' @param gwp_ch4 Global warming potential for methane.
#' @return Numeric vector of leakage emissions in tonnes CO2e.
#' @examples
#' calculate_leakage_emissions_acm0001(0.05, 3000)
#' @export
calculate_leakage_emissions_acm0001 <- function(leakage_fraction,
                                                methane_captured_m3,
                                                methane_density_t_per_m3 = 0.000716,
                                                gwp_ch4 = 28) {
  if (!is.numeric(leakage_fraction) ||
      any(leakage_fraction < 0 | leakage_fraction > 1)) {
    rlang::abort("`leakage_fraction` must be numeric between 0 and 1.")
  }
  if (!is.numeric(methane_captured_m3) || any(methane_captured_m3 < 0)) {
    rlang::abort("`methane_captured_m3` must be a non-negative numeric vector.")
  }
  if (!(length(leakage_fraction) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`leakage_fraction` must be length 1 or match `methane_captured_m3`.")
  }
  if (!is.numeric(methane_density_t_per_m3) || any(methane_density_t_per_m3 <= 0)) {
    rlang::abort("`methane_density_t_per_m3` must contain positive numeric values.")
  }
  if (!(length(methane_density_t_per_m3) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`methane_density_t_per_m3` must be length 1 or match `methane_captured_m3`.")
  }
  if (!is.numeric(gwp_ch4) || any(gwp_ch4 <= 0)) {
    rlang::abort("`gwp_ch4` must contain positive numeric values.")
  }
  if (!(length(gwp_ch4) %in% c(1, length(methane_captured_m3)))) {
    rlang::abort("`gwp_ch4` must be length 1 or match `methane_captured_m3`.")
  }

  leakage_m3 <- methane_captured_m3 * leakage_fraction
  convert_methane_volume_to_mass(leakage_m3, methane_density_t_per_m3) * gwp_ch4
}

#' Calculate ACM0001 emission reductions
#'
#' Returns the emission reductions as baseline minus project minus leakage
#' emissions.
#'
#' @param baseline_emissions Baseline emissions in tonnes CO2e.
#' @param project_emissions Project emissions in tonnes CO2e.
#' @param leakage_emissions Leakage emissions in tonnes CO2e.
#' @return Numeric vector of emission reductions in tonnes CO2e.
#' @examples
#' calculate_emission_reductions_acm0001(1000, 50, 10)
#' @export
calculate_emission_reductions_acm0001 <- function(baseline_emissions,
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
