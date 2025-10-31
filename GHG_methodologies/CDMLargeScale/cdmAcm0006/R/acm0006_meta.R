#' Aggregate monitoring periods for ACM0006
#'
#' Summarises biomass combined heat and power monitoring data into period-level
#' totals following the ACM0006 calculation sequence.
#'
#' @param monitoring_data Data frame or tibble containing monitoring
#'   observations with at least the following columns: `period`,
#'   `heat_output_tj`, `electricity_output_mwh`, `baseline_heat_ef`,
#'   `baseline_electricity_ef`, `auxiliary_fossil_tj`, `auxiliary_fossil_ef`,
#'   `electricity_import_mwh`, `import_emission_factor`,
#'   `biomass_transport_tkm`, `transport_emission_factor`, and
#'   `leakage_fraction`.
#' @return Tibble with period-level totals and emissions outcomes.
#' @examples
#' data <- simulate_acm0006_dataset(3, seed = 21)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(monitoring_data) {
  required <- c(
    "period",
    "heat_output_tj",
    "electricity_output_mwh",
    "baseline_heat_ef",
    "baseline_electricity_ef",
    "auxiliary_fossil_tj",
    "auxiliary_fossil_ef",
    "electricity_import_mwh",
    "import_emission_factor",
    "biomass_transport_tkm",
    "transport_emission_factor",
    "leakage_fraction"
  )

  missing <- setdiff(required, names(monitoring_data))
  if (length(missing) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }

  monitoring_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      baseline_emissions = calculate_baseline_emissions_acm0006(
        heat_output_tj,
        baseline_heat_ef,
        electricity_output_mwh,
        baseline_electricity_ef
      ),
      project_emissions = calculate_project_emissions_acm0006(
        auxiliary_fossil_tj,
        auxiliary_fossil_ef,
        electricity_import_mwh,
        import_emission_factor,
        biomass_transport_tkm,
        transport_emission_factor
      ),
      leakage_emissions = calculate_leakage_emissions_acm0006(
        leakage_fraction,
        baseline_emissions
      ),
      emission_reductions = calculate_emission_reductions_acm0006(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      )
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      heat_output_tj = sum(heat_output_tj, na.rm = TRUE),
      electricity_output_mwh = sum(electricity_output_mwh, na.rm = TRUE),
      baseline_heat_ef = mean(baseline_heat_ef, na.rm = TRUE),
      baseline_electricity_ef = mean(baseline_electricity_ef, na.rm = TRUE),
      auxiliary_fossil_tj = sum(auxiliary_fossil_tj, na.rm = TRUE),
      auxiliary_fossil_ef = mean(auxiliary_fossil_ef, na.rm = TRUE),
      electricity_import_mwh = sum(electricity_import_mwh, na.rm = TRUE),
      import_emission_factor = mean(import_emission_factor, na.rm = TRUE),
      biomass_transport_tkm = sum(biomass_transport_tkm, na.rm = TRUE),
      transport_emission_factor = mean(transport_emission_factor, na.rm = TRUE),
      leakage_fraction = mean(leakage_fraction, na.rm = TRUE),
      baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      project_emissions = sum(project_emissions, na.rm = TRUE),
      leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Estimate total emission reductions under ACM0006
#'
#' Aggregates monitoring data to compute total baseline, project, leakage, and
#' emission reductions suitable for project documentation.
#'
#' @inheritParams aggregate_monitoring_periods
#' @return Tibble with total emissions accounting for the monitoring dataset.
#' @examples
#' data <- simulate_acm0006_dataset(4, seed = 99)
#' estimate_emission_reductions_acm0006(data)
#' @export
estimate_emission_reductions_acm0006 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_heat_output_tj = sum(heat_output_tj, na.rm = TRUE),
      total_electricity_output_mwh = sum(electricity_output_mwh, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
