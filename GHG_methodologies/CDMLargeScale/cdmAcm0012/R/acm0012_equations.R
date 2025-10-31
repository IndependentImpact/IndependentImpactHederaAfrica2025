#' Calculate baseline electricity emissions for ACM0012
#'
#' Implements Equation 1 of ACM0012 by multiplying electricity exported from
#' the waste energy recovery system by the baseline grid emission factor.
#'
#' @param electricity_export_mwh Numeric vector of electricity exported to the
#'   grid in megawatt-hours.
#' @param baseline_grid_ef_t_per_mwh Baseline grid emission factor in tonnes
#'   CO2e per megawatt-hour.
#' @return Numeric vector of baseline electricity emissions in tonnes CO2e.
#' @examples
#' calculate_baseline_electricity_emissions_acm0012(100, 0.8)
#' @export
calculate_baseline_electricity_emissions_acm0012 <- function(
    electricity_export_mwh,
    baseline_grid_ef_t_per_mwh) {
  validate_non_negative_numeric(electricity_export_mwh, "electricity_export_mwh")
  validate_positive_numeric(baseline_grid_ef_t_per_mwh, "baseline_grid_ef_t_per_mwh")
  recycle_inputs(electricity_export_mwh, baseline_grid_ef_t_per_mwh)

  electricity_export_mwh * baseline_grid_ef_t_per_mwh
}

#' Calculate baseline thermal energy emissions for ACM0012
#'
#' Implements Equation 2 of ACM0012 by multiplying useful thermal energy
#' delivered to process users by the baseline emission factor for the displaced
#' fuel.
#'
#' @param thermal_export_gj Numeric vector of thermal energy exported in
#'   gigajoules.
#' @param baseline_thermal_ef_t_per_gj Baseline thermal emission factor in
#'   tonnes CO2e per gigajoule.
#' @return Numeric vector of baseline thermal energy emissions in tonnes CO2e.
#' @examples
#' calculate_baseline_thermal_emissions_acm0012(200, 0.056)
#' @export
calculate_baseline_thermal_emissions_acm0012 <- function(
    thermal_export_gj,
    baseline_thermal_ef_t_per_gj) {
  validate_non_negative_numeric(thermal_export_gj, "thermal_export_gj")
  validate_positive_numeric(baseline_thermal_ef_t_per_gj, "baseline_thermal_ef_t_per_gj")
  recycle_inputs(thermal_export_gj, baseline_thermal_ef_t_per_gj)

  thermal_export_gj * baseline_thermal_ef_t_per_gj
}

#' Calculate total baseline emissions for ACM0012
#'
#' Aggregates baseline electricity and thermal energy emissions while allowing
#' for additional fossil fuel savings attributable to flare gas recovery.
#'
#' @param baseline_electricity_emissions Baseline electricity emissions computed
#'   with [calculate_baseline_electricity_emissions_acm0012()].
#' @param baseline_thermal_emissions Baseline thermal emissions computed with
#'   [calculate_baseline_thermal_emissions_acm0012()].
#' @param flare_gas_displacement_emissions Optional additional baseline
#'   emissions due to fossil fuel displacement from flare gas utilisation.
#' @return Numeric vector of total baseline emissions in tonnes CO2e.
#' @examples
#' calculate_baseline_emissions_acm0012(80, 40, 5)
#' @export
calculate_baseline_emissions_acm0012 <- function(
    baseline_electricity_emissions,
    baseline_thermal_emissions,
    flare_gas_displacement_emissions = 0) {
  validate_non_negative_numeric(baseline_electricity_emissions, "baseline_electricity_emissions")
  validate_non_negative_numeric(baseline_thermal_emissions, "baseline_thermal_emissions")
  validate_non_negative_numeric(flare_gas_displacement_emissions, "flare_gas_displacement_emissions")
  recycle_inputs(baseline_electricity_emissions, baseline_thermal_emissions, flare_gas_displacement_emissions)

  baseline_electricity_emissions + baseline_thermal_emissions + flare_gas_displacement_emissions
}

#' Calculate project electricity emissions for ACM0012
#'
#' Computes project electricity emissions due to imported electricity required
#' for auxiliary equipment such as pumps and fans.
#'
#' @param electricity_import_mwh Numeric vector of electricity imported in
#'   megawatt-hours.
#' @param project_grid_ef_t_per_mwh Project electricity emission factor in
#'   tonnes CO2e per megawatt-hour.
#' @return Numeric vector of project electricity emissions in tonnes CO2e.
#' @examples
#' calculate_project_electricity_emissions_acm0012(10, 0.75)
#' @export
calculate_project_electricity_emissions_acm0012 <- function(
    electricity_import_mwh,
    project_grid_ef_t_per_mwh) {
  validate_non_negative_numeric(electricity_import_mwh, "electricity_import_mwh")
  validate_positive_numeric(project_grid_ef_t_per_mwh, "project_grid_ef_t_per_mwh")
  recycle_inputs(electricity_import_mwh, project_grid_ef_t_per_mwh)

  electricity_import_mwh * project_grid_ef_t_per_mwh
}

#' Calculate auxiliary fuel emissions for ACM0012
#'
#' Accounts for fossil fuel consumption used to support waste energy recovery
#' systems (e.g., supplemental firing).
#'
#' @param auxiliary_fuel_tj Auxiliary fossil fuel consumption in terajoules.
#' @param auxiliary_ef_t_per_tj Emission factor for the auxiliary fuel in tonnes
#'   CO2e per terajoule.
#' @return Numeric vector of auxiliary fuel emissions in tonnes CO2e.
#' @examples
#' calculate_auxiliary_fuel_emissions_acm0012(0.5, 56)
#' @export
calculate_auxiliary_fuel_emissions_acm0012 <- function(
    auxiliary_fuel_tj,
    auxiliary_ef_t_per_tj) {
  validate_non_negative_numeric(auxiliary_fuel_tj, "auxiliary_fuel_tj")
  validate_positive_numeric(auxiliary_ef_t_per_tj, "auxiliary_ef_t_per_tj")
  recycle_inputs(auxiliary_fuel_tj, auxiliary_ef_t_per_tj)

  auxiliary_fuel_tj * auxiliary_ef_t_per_tj
}

#' Calculate methane leakage emissions for ACM0012
#'
#' Applies the global warming potential of methane to any measured leakage from
#' the waste energy recovery system.
#'
#' @param methane_leakage_nm3 Methane leakage in normal cubic metres.
#' @param methane_density_t_per_nm3 Density of methane in tonnes per normal
#'   cubic metre. Defaults to 0.000716 t/nm3.
#' @param gwp_ch4 Global warming potential of methane. Defaults to 28.
#' @return Numeric vector of methane leakage emissions in tonnes CO2e.
#' @examples
#' calculate_methane_leakage_emissions_acm0012(100)
#' @export
calculate_methane_leakage_emissions_acm0012 <- function(
    methane_leakage_nm3,
    methane_density_t_per_nm3 = 0.000716,
    gwp_ch4 = 28) {
  validate_non_negative_numeric(methane_leakage_nm3, "methane_leakage_nm3")
  validate_positive_numeric(methane_density_t_per_nm3, "methane_density_t_per_nm3")
  validate_positive_numeric(gwp_ch4, "gwp_ch4")
  recycle_inputs(methane_leakage_nm3, methane_density_t_per_nm3, gwp_ch4)

  methane_leakage_nm3 * methane_density_t_per_nm3 * gwp_ch4
}

#' Calculate total project emissions for ACM0012
#'
#' Sums project electricity, auxiliary fuel, and methane leakage emissions to
#' produce total project emissions.
#'
#' @param project_electricity_emissions Numeric vector from
#'   [calculate_project_electricity_emissions_acm0012()].
#' @param auxiliary_fuel_emissions Numeric vector from
#'   [calculate_auxiliary_fuel_emissions_acm0012()].
#' @param methane_leakage_emissions Numeric vector from
#'   [calculate_methane_leakage_emissions_acm0012()].
#' @return Numeric vector of total project emissions in tonnes CO2e.
#' @examples
#' calculate_project_emissions_acm0012(5, 3, 2)
#' @export
calculate_project_emissions_acm0012 <- function(
    project_electricity_emissions,
    auxiliary_fuel_emissions,
    methane_leakage_emissions = 0) {
  validate_non_negative_numeric(project_electricity_emissions, "project_electricity_emissions")
  validate_non_negative_numeric(auxiliary_fuel_emissions, "auxiliary_fuel_emissions")
  validate_non_negative_numeric(methane_leakage_emissions, "methane_leakage_emissions")
  recycle_inputs(project_electricity_emissions, auxiliary_fuel_emissions, methane_leakage_emissions)

  project_electricity_emissions + auxiliary_fuel_emissions + methane_leakage_emissions
}

#' Calculate leakage emissions for ACM0012
#'
#' Accounts for leakage associated with upstream fuel supply displacement and
#' downstream effects of exported energy.
#'
#' @param leakage_energy_mwh Energy subject to leakage effects in megawatt-hours.
#' @param leakage_ef_t_per_mwh Leakage emission factor in tonnes CO2e per
#'   megawatt-hour.
#' @return Numeric vector of leakage emissions in tonnes CO2e.
#' @examples
#' calculate_leakage_emissions_acm0012(20, 0.1)
#' @export
calculate_leakage_emissions_acm0012 <- function(
    leakage_energy_mwh,
    leakage_ef_t_per_mwh) {
  validate_non_negative_numeric(leakage_energy_mwh, "leakage_energy_mwh")
  validate_non_negative_numeric(leakage_ef_t_per_mwh, "leakage_ef_t_per_mwh")
  recycle_inputs(leakage_energy_mwh, leakage_ef_t_per_mwh)

  leakage_energy_mwh * leakage_ef_t_per_mwh
}

#' Calculate emission reductions for ACM0012
#'
#' Computes net emission reductions by subtracting project and leakage emissions
#' from baseline emissions.
#'
#' @param baseline_emissions Baseline emissions in tonnes CO2e.
#' @param project_emissions Project emissions in tonnes CO2e.
#' @param leakage_emissions Leakage emissions in tonnes CO2e.
#' @return Numeric vector of emission reductions in tonnes CO2e.
#' @examples
#' calculate_emission_reductions_acm0012(100, 20, 5)
#' @export
calculate_emission_reductions_acm0012 <- function(
    baseline_emissions,
    project_emissions,
    leakage_emissions = 0) {
  validate_non_negative_numeric(baseline_emissions, "baseline_emissions")
  validate_non_negative_numeric(project_emissions, "project_emissions")
  validate_non_negative_numeric(leakage_emissions, "leakage_emissions")
  recycle_inputs(baseline_emissions, project_emissions, leakage_emissions)

  baseline_emissions - project_emissions - leakage_emissions
}

# Internal validation helpers -------------------------------------------------

validate_non_negative_numeric <- function(x, name) {
  if (!is.numeric(x) || any(is.na(x)) || any(x < 0)) {
    rlang::abort(glue::glue("`{name}` must be a non-negative numeric vector."))
  }
  invisible(x)
}

validate_positive_numeric <- function(x, name) {
  if (!is.numeric(x) || any(is.na(x)) || any(x <= 0)) {
    rlang::abort(glue::glue("`{name}` must contain positive numeric values."))
  }
  invisible(x)
}

recycle_inputs <- function(...) {
  args <- list(...)
  lengths <- purrr::map_int(args, length)
  max_length <- max(lengths)
  if (max_length == 1) {
    return(invisible(NULL))
  }
  valid <- purrr::map_lgl(lengths, ~ .x %in% c(1, max_length))
  if (!all(valid)) {
    rlang::abort("Inputs must either be length 1 or the same length as the longest vector.")
  }
  invisible(NULL)
}
