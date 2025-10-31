#' Equation 1: Methane volume recovered
#'
#' Computes the total volume of methane recovered from the drainage or
#' ventilation system during a monitoring period. The function multiplies the
#' average volumetric flow by the cumulative operating hours and supports vector
#' inputs for tidy period-level summarisation.
#'
#' @param flow_rate_m3_per_h Numeric vector of average methane flow rates in
#'   cubic metres per hour.
#' @param operating_hours Numeric vector of operating hours corresponding to the
#'   same observations.
#'
#' @return Numeric vector with methane volumes in cubic metres.
#'
#' @examples
#' calculate_methane_volume_acm0008(500, 720)
#' calculate_methane_volume_acm0008(c(450, 520), c(700, 680))
#' @export
calculate_methane_volume_acm0008 <- function(flow_rate_m3_per_h,
                                             operating_hours) {
  if (!is.numeric(flow_rate_m3_per_h) || any(flow_rate_m3_per_h < 0)) {
    rlang::abort("`flow_rate_m3_per_h` must be a non-negative numeric vector.")
  }
  if (!is.numeric(operating_hours) || any(operating_hours < 0)) {
    rlang::abort("`operating_hours` must be a non-negative numeric vector.")
  }
  if (length(flow_rate_m3_per_h) != length(operating_hours)) {
    rlang::abort("`flow_rate_m3_per_h` and `operating_hours` must have the same length.")
  }

  flow_rate_m3_per_h * operating_hours
}

#' Equation 2: Baseline emissions from methane destruction
#'
#' Implements the ACM0008 baseline equation that assumes all recovered methane
#' would otherwise be released to the atmosphere. Emissions are expressed in
#' tonnes of CO2 equivalent using the density of methane at standard conditions
#' and the global warming potential.
#'
#' @param recovered_volume_m3 Numeric vector of methane volumes in cubic metres.
#' @param methane_fraction Numeric vector specifying the methane share of the gas
#'   stream (between 0 and 1).
#' @param methane_density_t_per_m3 Density of methane expressed in tonnes per
#'   cubic metre. Defaults to 0.000716 t/m^3 (0.716 kg/m^3).
#' @param gwp_ch4 Global warming potential of methane relative to CO2. Defaults
#'   to 28, aligned with the CDM 5th Assessment Report values.
#'
#' @return Numeric vector of baseline emissions in tonnes of CO2 equivalent.
#'
#' @examples
#' vol <- calculate_methane_volume_acm0008(500, 720)
#' calculate_baseline_emissions_acm0008(vol, 0.35)
#' @export
calculate_baseline_emissions_acm0008 <- function(recovered_volume_m3,
                                                 methane_fraction,
                                                 methane_density_t_per_m3 = 0.000716,
                                                 gwp_ch4 = 28) {
  if (!is.numeric(recovered_volume_m3) || any(recovered_volume_m3 < 0)) {
    rlang::abort("`recovered_volume_m3` must be a non-negative numeric vector.")
  }
  if (!is.numeric(methane_fraction) || any(methane_fraction < 0 | methane_fraction > 1)) {
    rlang::abort("`methane_fraction` must be numeric with values between 0 and 1.")
  }
  if (length(recovered_volume_m3) != length(methane_fraction)) {
    rlang::abort("`recovered_volume_m3` and `methane_fraction` must have matching lengths.")
  }

  recovered_volume_m3 * methane_fraction * methane_density_t_per_m3 * gwp_ch4
}

#' Equation 3: Project emissions from residual methane
#'
#' Estimates project emissions that remain after methane is oxidised or utilised
#' within the project boundary. The function multiplies the unoxidised methane
#' volume by density and the methane global warming potential.
#'
#' @param recovered_volume_m3 Numeric vector of methane volumes recovered.
#' @param methane_fraction Numeric vector of methane content (between 0 and 1).
#' @param oxidation_efficiency Numeric vector describing the share of methane
#'   destroyed or utilised (between 0 and 1).
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#' @param gwp_ch4 Global warming potential for methane.
#'
#' @return Numeric vector of project emissions in tonnes of CO2 equivalent.
#'
#' @examples
#' vol <- calculate_methane_volume_acm0008(600, 700)
#' calculate_project_emissions_acm0008(vol, 0.4, 0.95)
#' @export
calculate_project_emissions_acm0008 <- function(recovered_volume_m3,
                                                methane_fraction,
                                                oxidation_efficiency,
                                                methane_density_t_per_m3 = 0.000716,
                                                gwp_ch4 = 28) {
  if (!is.numeric(oxidation_efficiency) || any(oxidation_efficiency < 0 | oxidation_efficiency > 1)) {
    rlang::abort("`oxidation_efficiency` must be numeric with values between 0 and 1.")
  }
  if (length(oxidation_efficiency) != length(recovered_volume_m3)) {
    rlang::abort("`oxidation_efficiency` must match the length of `recovered_volume_m3`.")
  }

  unoxidised_fraction <- 1 - oxidation_efficiency
  recovered_volume_m3 * methane_fraction * unoxidised_fraction *
    methane_density_t_per_m3 * gwp_ch4
}

#' Equation 4: Leakage emissions from auxiliary electricity
#'
#' Calculates leakage associated with auxiliary electricity consumption, such as
#' vacuum pumps and oxidation blowers. The helper accepts electricity
#' consumption in megawatt-hours and multiplies by the grid emission factor.
#'
#' @param electricity_use_mwh Numeric vector of electricity consumption values.
#' @param grid_emission_factor_t_per_mwh Numeric vector of grid emission factors
#'   in tonnes of CO2 per megawatt-hour.
#'
#' @return Numeric vector of leakage emissions in tonnes of CO2 equivalent.
#'
#' @examples
#' calculate_leakage_emissions_acm0008(c(50, 55), c(0.7, 0.68))
#' @export
calculate_leakage_emissions_acm0008 <- function(electricity_use_mwh,
                                                grid_emission_factor_t_per_mwh) {
  if (!is.numeric(electricity_use_mwh) || any(electricity_use_mwh < 0)) {
    rlang::abort("`electricity_use_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(grid_emission_factor_t_per_mwh) ||
      any(grid_emission_factor_t_per_mwh < 0)) {
    rlang::abort("`grid_emission_factor_t_per_mwh` must be a non-negative numeric vector.")
  }
  if (length(electricity_use_mwh) != length(grid_emission_factor_t_per_mwh)) {
    rlang::abort("`electricity_use_mwh` and `grid_emission_factor_t_per_mwh` must have matching lengths.")
  }

  electricity_use_mwh * grid_emission_factor_t_per_mwh
}

#' Equation 5: Net emission reductions
#'
#' Computes the net emission reductions by subtracting project and leakage
#' emissions from the baseline scenario.
#'
#' @param baseline_emissions_tco2e Numeric vector of baseline emissions.
#' @param project_emissions_tco2e Numeric vector of project emissions.
#' @param leakage_emissions_tco2e Numeric vector of leakage emissions.
#'
#' @return Numeric vector of net emission reductions in tonnes of CO2
#'   equivalent.
#'
#' @examples
#' calculate_net_emission_reductions_acm0008(1000, 120, 40)
#' @export
calculate_net_emission_reductions_acm0008 <- function(baseline_emissions_tco2e,
                                                      project_emissions_tco2e,
                                                      leakage_emissions_tco2e) {
  if (!is.numeric(baseline_emissions_tco2e) || any(baseline_emissions_tco2e < 0)) {
    rlang::abort("`baseline_emissions_tco2e` must be non-negative numeric values.")
  }
  if (!is.numeric(project_emissions_tco2e) || any(project_emissions_tco2e < 0)) {
    rlang::abort("`project_emissions_tco2e` must be non-negative numeric values.")
  }
  if (!is.numeric(leakage_emissions_tco2e) || any(leakage_emissions_tco2e < 0)) {
    rlang::abort("`leakage_emissions_tco2e` must be non-negative numeric values.")
  }

  baseline_emissions_tco2e - project_emissions_tco2e - leakage_emissions_tco2e
}
