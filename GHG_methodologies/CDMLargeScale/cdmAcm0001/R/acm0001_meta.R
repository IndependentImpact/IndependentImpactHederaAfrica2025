#' Aggregate monitoring periods for ACM0001
#'
#' Summarises landfill gas monitoring data into period-level totals that follow
#' the ACM0001 calculation sequence for baseline, project, leakage, and emission
#' reductions.
#'
#' @param monitoring_data Data frame or tibble containing monitoring
#'   observations with at least the following columns: `period`,
#'   `methane_generation_m3`, `baseline_capture_efficiency`,
#'   `methane_captured_m3`, `destruction_efficiency`, `auxiliary_fuel_tj`,
#'   `auxiliary_ef_t_per_tj`, `electricity_import_mwh`, `import_ef_t_per_mwh`,
#'   and `leakage_fraction`. Optional columns `methane_density_t_per_m3` and
#'   `gwp_ch4` override default parameter values.
#' @return Tibble with period-level totals and emissions outcomes.
#' @examples
#' data <- simulate_acm0001_dataset(3, seed = 42)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(monitoring_data) {
  required <- c(
    "period",
    "methane_generation_m3",
    "baseline_capture_efficiency",
    "methane_captured_m3",
    "destruction_efficiency",
    "auxiliary_fuel_tj",
    "auxiliary_ef_t_per_tj",
    "electricity_import_mwh",
    "import_ef_t_per_mwh",
    "leakage_fraction"
  )

  missing <- setdiff(required, names(monitoring_data))
  if (length(missing) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }

  defaults <- list(
    methane_density_t_per_m3 = 0.000716,
    gwp_ch4 = 28,
    oxidation_fraction = 0
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(monitoring_data)) {
      monitoring_data[[nm]] <- defaults[[nm]]
    }
  }

  monitoring_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      baseline_emissions = calculate_baseline_emissions_acm0001(
        methane_generation_m3,
        baseline_capture_efficiency,
        oxidation_fraction,
        methane_density_t_per_m3,
        gwp_ch4
      ),
      project_emissions = calculate_project_emissions_acm0001(
        methane_captured_m3,
        destruction_efficiency,
        methane_density_t_per_m3,
        gwp_ch4,
        auxiliary_fuel_tj,
        auxiliary_ef_t_per_tj,
        electricity_import_mwh,
        import_ef_t_per_mwh
      ),
      leakage_emissions = calculate_leakage_emissions_acm0001(
        leakage_fraction,
        methane_captured_m3,
        methane_density_t_per_m3,
        gwp_ch4
      ),
      methane_destroyed_t = calculate_methane_destroyed_acm0001(
        methane_captured_m3,
        destruction_efficiency,
        methane_density_t_per_m3
      ),
      methane_destroyed_co2e = calculate_methane_destruction_co2e_acm0001(
        methane_captured_m3,
        destruction_efficiency,
        methane_density_t_per_m3,
        gwp_ch4
      ),
      emission_reductions = calculate_emission_reductions_acm0001(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      )
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      methane_generation_m3 = sum(methane_generation_m3, na.rm = TRUE),
      baseline_capture_efficiency = mean(baseline_capture_efficiency, na.rm = TRUE),
      methane_captured_m3 = sum(methane_captured_m3, na.rm = TRUE),
      destruction_efficiency = mean(destruction_efficiency, na.rm = TRUE),
      auxiliary_fuel_tj = sum(auxiliary_fuel_tj, na.rm = TRUE),
      auxiliary_ef_t_per_tj = mean(auxiliary_ef_t_per_tj, na.rm = TRUE),
      electricity_import_mwh = sum(electricity_import_mwh, na.rm = TRUE),
      import_ef_t_per_mwh = mean(import_ef_t_per_mwh, na.rm = TRUE),
      leakage_fraction = mean(leakage_fraction, na.rm = TRUE),
      oxidation_fraction = mean(oxidation_fraction, na.rm = TRUE),
      methane_density_t_per_m3 = mean(methane_density_t_per_m3, na.rm = TRUE),
      gwp_ch4 = mean(gwp_ch4, na.rm = TRUE),
      baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      project_emissions = sum(project_emissions, na.rm = TRUE),
      leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      methane_destroyed_t = sum(methane_destroyed_t, na.rm = TRUE),
      methane_destroyed_co2e = sum(methane_destroyed_co2e, na.rm = TRUE),
      emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Estimate total emission reductions under ACM0001
#'
#' Aggregates monitoring data to compute total baseline, project, leakage, and
#' emission reductions suitable for project documentation.
#'
#' @inheritParams aggregate_monitoring_periods
#' @return Tibble with total emissions accounting for the monitoring dataset.
#' @examples
#' data <- simulate_acm0001_dataset(4, seed = 99)
#' estimate_emission_reductions_acm0001(data)
#' @export
estimate_emission_reductions_acm0001 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_methane_generation_m3 = sum(methane_generation_m3, na.rm = TRUE),
      total_methane_captured_m3 = sum(methane_captured_m3, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_methane_destroyed_t = sum(methane_destroyed_t, na.rm = TRUE),
      total_methane_destroyed_co2e = sum(methane_destroyed_co2e, na.rm = TRUE),
      total_emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
