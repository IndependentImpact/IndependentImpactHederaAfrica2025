#' Convert methane volume to mass
#'
#' Translates methane volume expressed at standard conditions into tonnes of
#' methane using a default density of 0.0007168 tonnes per cubic metre.
#'
#' @param volume_m3 Numeric vector of methane volumes in cubic metres.
#' @param methane_density Density of methane in tonnes per cubic metre. Defaults
#'   to `0.0007168`, equivalent to 0.7168 kg/m3 at standard temperature and
#'   pressure.
#' @return Numeric vector of methane mass in tonnes.
#' @examples
#' convert_methane_volume_to_mass(100)
#' convert_methane_volume_to_mass(c(50, 75), methane_density = 0.0007)
#' @export
convert_methane_volume_to_mass <- function(volume_m3, methane_density = 0.0007168) {
  if (!is.numeric(volume_m3)) {
    rlang::abort("`volume_m3` must be numeric.")
  }
  if (!is.numeric(methane_density) || length(methane_density) != 1 ||
      is.na(methane_density) || methane_density <= 0) {
    rlang::abort("`methane_density` must be a single positive numeric value.")
  }

  volume_m3 * methane_density
}

#' Calculate COD removed under ACM0010
#'
#' Estimates the chemical oxygen demand (COD) removed during wastewater
#' treatment given influent and effluent concentrations and daily flow.
#'
#' @param cod_in_mg_l Influent COD concentration in mg/L.
#' @param cod_out_mg_l Effluent COD concentration in mg/L.
#' @param flow_m3 Daily wastewater flow in cubic metres.
#' @return Numeric vector of COD removed in kilograms per day.
#' @examples
#' calculate_cod_removed_acm0010(4000, 800, 250)
#' @export
calculate_cod_removed_acm0010 <- function(cod_in_mg_l, cod_out_mg_l, flow_m3) {
  if (!is.numeric(cod_in_mg_l) || !is.numeric(cod_out_mg_l) || !is.numeric(flow_m3)) {
    rlang::abort("All inputs must be numeric.")
  }
  if (any(cod_in_mg_l < cod_out_mg_l, na.rm = TRUE)) {
    rlang::abort("`cod_in_mg_l` must be greater than or equal to `cod_out_mg_l`.")
  }
  if (any(flow_m3 <= 0, na.rm = TRUE)) {
    rlang::abort("`flow_m3` must be positive.")
  }

  (cod_in_mg_l - cod_out_mg_l) * flow_m3 / 1000
}

#' Baseline methane emissions under ACM0010
#'
#' Computes baseline methane emissions associated with uncontrolled anaerobic
#' wastewater or manure management systems.
#'
#' @param cod_removed_kg COD removed in kilograms per day.
#' @param methane_conversion_factor Methane conversion factor (MCF) representing
#'   the fraction of degradable organic content converted to methane. Must lie
#'   between 0 and 1.
#' @param methane_fraction Fraction of methane in biogas. Defaults to `0.67`.
#' @param gwp_ch4 Global warming potential for methane. Defaults to `28` based on
#'   the IPCC Fifth Assessment Report.
#' @return Numeric vector of baseline emissions in tonnes CO2e per day.
#' @examples
#' cod_removed <- calculate_cod_removed_acm0010(3500, 500, 200)
#' calculate_baseline_emissions_acm0010(cod_removed, 0.8)
#' @export
calculate_baseline_emissions_acm0010 <- function(cod_removed_kg,
                                                 methane_conversion_factor,
                                                 methane_fraction = 0.67,
                                                 gwp_ch4 = 28) {
  if (!is.numeric(cod_removed_kg) || !is.numeric(methane_conversion_factor)) {
    rlang::abort("`cod_removed_kg` and `methane_conversion_factor` must be numeric.")
  }
  if (any(methane_conversion_factor < 0 | methane_conversion_factor > 1,
          na.rm = TRUE)) {
    rlang::abort("`methane_conversion_factor` must lie between 0 and 1.")
  }
  if (!is.numeric(methane_fraction) || length(methane_fraction) != 1 ||
      methane_fraction <= 0 || methane_fraction > 1) {
    rlang::abort("`methane_fraction` must be a single value between 0 and 1.")
  }
  if (!is.numeric(gwp_ch4) || length(gwp_ch4) != 1 || gwp_ch4 <= 0) {
    rlang::abort("`gwp_ch4` must be a single positive numeric value.")
  }

  methane_produced_tonnes <- cod_removed_kg * methane_conversion_factor * 0.00025 * methane_fraction
  methane_produced_tonnes * gwp_ch4
}

#' Project emissions for ACM0010 systems
#'
#' Calculates project methane emissions resulting from uncombusted methane in
#' the digester gas handling system.
#'
#' @param methane_recovered_m3 Volume of methane recovered in cubic metres.
#' @param combustion_efficiency Fraction of recovered methane that is combusted
#'   (0-1).
#' @param methane_density Density of methane in tonnes per cubic metre. Defaults
#'   to `0.0007168`.
#' @param gwp_ch4 Global warming potential for methane. Defaults to `28`.
#' @return Numeric vector of project emissions in tonnes CO2e.
#' @examples
#' calculate_project_emissions_acm0010(1200, 0.95)
#' @export
calculate_project_emissions_acm0010 <- function(methane_recovered_m3,
                                                combustion_efficiency,
                                                methane_density = 0.0007168,
                                                gwp_ch4 = 28) {
  if (!is.numeric(methane_recovered_m3) || !is.numeric(combustion_efficiency)) {
    rlang::abort("`methane_recovered_m3` and `combustion_efficiency` must be numeric.")
  }
  if (any(combustion_efficiency < 0 | combustion_efficiency > 1, na.rm = TRUE)) {
    rlang::abort("`combustion_efficiency` must lie between 0 and 1.")
  }
  if (!is.numeric(methane_density) || length(methane_density) != 1 || methane_density <= 0) {
    rlang::abort("`methane_density` must be a single positive numeric value.")
  }
  if (!is.numeric(gwp_ch4) || length(gwp_ch4) != 1 || gwp_ch4 <= 0) {
    rlang::abort("`gwp_ch4` must be a single positive numeric value.")
  }

  methane_mass <- convert_methane_volume_to_mass(methane_recovered_m3, methane_density)
  methane_slip <- methane_mass * (1 - combustion_efficiency)
  methane_slip * gwp_ch4
}

#' Leakage emissions for ACM0010 projects
#'
#' Calculates emissions associated with auxiliary electricity consumption or fuel
#' use that are attributable to the project.
#'
#' @param electricity_consumed_mwh Electricity consumed from the grid in MWh.
#' @param grid_emission_factor Emission factor of the connected grid in tCO2e per
#'   MWh.
#' @param additional_leakage Additional leakage emissions in tonnes CO2e to add
#'   (e.g. transport of digestate).
#' @return Numeric vector of leakage emissions in tonnes CO2e.
#' @examples
#' calculate_leakage_emissions_acm0010(50, 0.8, 5)
#' @export
calculate_leakage_emissions_acm0010 <- function(electricity_consumed_mwh,
                                                grid_emission_factor,
                                                additional_leakage = 0) {
  if (!is.numeric(electricity_consumed_mwh) || !is.numeric(grid_emission_factor) ||
      !is.numeric(additional_leakage)) {
    rlang::abort("All inputs must be numeric.")
  }
  if (any(electricity_consumed_mwh < 0, na.rm = TRUE)) {
    rlang::abort("`electricity_consumed_mwh` must be non-negative.")
  }
  if (any(grid_emission_factor < 0, na.rm = TRUE)) {
    rlang::abort("`grid_emission_factor` must be non-negative.")
  }

  electricity_consumed_mwh * grid_emission_factor + additional_leakage
}

#' Net emission reductions under ACM0010
#'
#' Computes net emission reductions as baseline minus project and leakage
#' emissions.
#'
#' @param baseline Baseline emissions in tonnes CO2e.
#' @param project Project emissions in tonnes CO2e.
#' @param leakage Leakage emissions in tonnes CO2e.
#' @return Numeric vector of net emission reductions in tonnes CO2e.
#' @examples
#' calculate_net_emission_reductions_acm0010(100, 5, 3)
#' @export
calculate_net_emission_reductions_acm0010 <- function(baseline, project, leakage) {
  if (!is.numeric(baseline) || !is.numeric(project) || !is.numeric(leakage)) {
    rlang::abort("All inputs must be numeric.")
  }

  baseline - project - leakage
}
