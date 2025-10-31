#' Aggregate monitoring periods for ACM0012
#'
#' Summarises waste energy recovery monitoring data into period-level totals
#' including baseline, project, leakage, and net emission reductions.
#'
#' @param monitoring_data Data frame or tibble containing monitoring
#'   observations with columns:
#'   - `period`: Monitoring period identifier.
#'   - `electricity_export_mwh`: Electricity exported in MWh.
#'   - `baseline_grid_ef_t_per_mwh`: Baseline grid emission factor in tCO2e/MWh.
#'   - `thermal_export_gj`: Useful thermal energy exported in GJ.
#'   - `baseline_thermal_ef_t_per_gj`: Baseline thermal emission factor in tCO2e/GJ.
#'   - `electricity_import_mwh`: Electricity imported for project operations.
#'   - `project_grid_ef_t_per_mwh`: Emission factor for imported electricity.
#'   - `auxiliary_fuel_tj`: Auxiliary fossil fuel consumption in TJ.
#'   - `auxiliary_ef_t_per_tj`: Emission factor for auxiliary fuel in tCO2e/TJ.
#'   - `methane_leakage_nm3`: Methane leakage in normal cubic metres.
#'   - `leakage_energy_mwh`: Energy subject to leakage in MWh.
#' Optional columns `flare_gas_displacement_emissions`,
#' `methane_density_t_per_nm3`, `gwp_ch4`, and `leakage_ef_t_per_mwh` override
#' default parameters.
#' @return Tibble with period-level totals and emissions outcomes.
#' @examples
#' data <- simulate_acm0012_dataset(3, seed = 123)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(monitoring_data) {
  required <- c(
    "period",
    "electricity_export_mwh",
    "baseline_grid_ef_t_per_mwh",
    "thermal_export_gj",
    "baseline_thermal_ef_t_per_gj",
    "electricity_import_mwh",
    "project_grid_ef_t_per_mwh",
    "auxiliary_fuel_tj",
    "auxiliary_ef_t_per_tj",
    "methane_leakage_nm3",
    "leakage_energy_mwh"
  )

  missing <- setdiff(required, names(monitoring_data))
  if (length(missing) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }

  defaults <- list(
    flare_gas_displacement_emissions = 0,
    methane_density_t_per_nm3 = 0.000716,
    gwp_ch4 = 28,
    leakage_ef_t_per_mwh = 0
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(monitoring_data)) {
      monitoring_data[[nm]] <- defaults[[nm]]
    }
  }

  monitoring_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      baseline_electricity_emissions = calculate_baseline_electricity_emissions_acm0012(
        electricity_export_mwh,
        baseline_grid_ef_t_per_mwh
      ),
      baseline_thermal_emissions = calculate_baseline_thermal_emissions_acm0012(
        thermal_export_gj,
        baseline_thermal_ef_t_per_gj
      ),
      baseline_emissions = calculate_baseline_emissions_acm0012(
        baseline_electricity_emissions,
        baseline_thermal_emissions,
        flare_gas_displacement_emissions
      ),
      project_electricity_emissions = calculate_project_electricity_emissions_acm0012(
        electricity_import_mwh,
        project_grid_ef_t_per_mwh
      ),
      auxiliary_fuel_emissions = calculate_auxiliary_fuel_emissions_acm0012(
        auxiliary_fuel_tj,
        auxiliary_ef_t_per_tj
      ),
      methane_leakage_emissions = calculate_methane_leakage_emissions_acm0012(
        methane_leakage_nm3,
        methane_density_t_per_nm3,
        gwp_ch4
      ),
      project_emissions = calculate_project_emissions_acm0012(
        project_electricity_emissions,
        auxiliary_fuel_emissions,
        methane_leakage_emissions
      ),
      leakage_emissions = calculate_leakage_emissions_acm0012(
        leakage_energy_mwh,
        leakage_ef_t_per_mwh
      ),
      emission_reductions = calculate_emission_reductions_acm0012(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      )
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      electricity_export_mwh = sum(electricity_export_mwh, na.rm = TRUE),
      baseline_grid_ef_t_per_mwh = mean(baseline_grid_ef_t_per_mwh, na.rm = TRUE),
      thermal_export_gj = sum(thermal_export_gj, na.rm = TRUE),
      baseline_thermal_ef_t_per_gj = mean(baseline_thermal_ef_t_per_gj, na.rm = TRUE),
      flare_gas_displacement_emissions = sum(flare_gas_displacement_emissions, na.rm = TRUE),
      electricity_import_mwh = sum(electricity_import_mwh, na.rm = TRUE),
      project_grid_ef_t_per_mwh = mean(project_grid_ef_t_per_mwh, na.rm = TRUE),
      auxiliary_fuel_tj = sum(auxiliary_fuel_tj, na.rm = TRUE),
      auxiliary_ef_t_per_tj = mean(auxiliary_ef_t_per_tj, na.rm = TRUE),
      methane_leakage_nm3 = sum(methane_leakage_nm3, na.rm = TRUE),
      methane_density_t_per_nm3 = mean(methane_density_t_per_nm3, na.rm = TRUE),
      gwp_ch4 = mean(gwp_ch4, na.rm = TRUE),
      leakage_energy_mwh = sum(leakage_energy_mwh, na.rm = TRUE),
      leakage_ef_t_per_mwh = mean(leakage_ef_t_per_mwh, na.rm = TRUE),
      baseline_electricity_emissions = sum(baseline_electricity_emissions, na.rm = TRUE),
      baseline_thermal_emissions = sum(baseline_thermal_emissions, na.rm = TRUE),
      baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      project_electricity_emissions = sum(project_electricity_emissions, na.rm = TRUE),
      auxiliary_fuel_emissions = sum(auxiliary_fuel_emissions, na.rm = TRUE),
      methane_leakage_emissions = sum(methane_leakage_emissions, na.rm = TRUE),
      project_emissions = sum(project_emissions, na.rm = TRUE),
      leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Estimate total emission reductions under ACM0012
#'
#' Aggregates monitoring data to compute facility-level totals for baseline,
#' project, leakage, and net emission reductions.
#'
#' @inheritParams aggregate_monitoring_periods
#' @return Tibble with total emissions accounting for the monitoring dataset.
#' @examples
#' data <- simulate_acm0012_dataset(4, seed = 99)
#' estimate_emission_reductions_acm0012(data)
#' @export
estimate_emission_reductions_acm0012 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_electricity_export_mwh = sum(electricity_export_mwh, na.rm = TRUE),
      total_thermal_export_gj = sum(thermal_export_gj, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
