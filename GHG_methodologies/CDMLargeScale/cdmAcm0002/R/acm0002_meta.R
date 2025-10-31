#' Aggregate monitoring periods for ACM0002
#'
#' Converts raw monitoring observations into period-level summaries with
#' baseline, project, and emission reduction calculations.
#'
#' @param monitoring_data Data frame or tibble containing monitoring
#'   observations with at least the following columns:
#'   `period`, `gross_generation_mwh`, `auxiliary_consumption_mwh`,
#'   `combined_margin_ef`, `fossil_fuel_tj`, `fossil_emission_factor`,
#'   `electricity_import_mwh`, `import_emission_factor`, and
#'   `leakage_emissions`.
#' @return Tibble summarising each monitoring period with calculated emissions
#'   and reductions.
#' @examples
#' data <- simulate_acm0002_dataset(3, seed = 123)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(monitoring_data) {
  required <- c(
    "period",
    "gross_generation_mwh",
    "auxiliary_consumption_mwh",
    "combined_margin_ef",
    "fossil_fuel_tj",
    "fossil_emission_factor",
    "electricity_import_mwh",
    "import_emission_factor",
    "leakage_emissions"
  )

  missing <- setdiff(required, names(monitoring_data))
  if (length(missing) > 0) {
    rlang::abort(
      paste("Missing required columns:", paste(missing, collapse = ", "))
    )
  }

  monitoring_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      net_electricity_mwh = calculate_net_electricity_generation(
        gross_generation_mwh,
        auxiliary_consumption_mwh
      ),
      baseline_emissions = calculate_baseline_emissions(
        net_electricity_mwh,
        combined_margin_ef
      ),
      project_emissions = calculate_project_emissions(
        fossil_fuel_tj,
        fossil_emission_factor,
        electricity_import_mwh,
        import_emission_factor
      ),
      emission_reductions = calculate_emission_reductions(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      )
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      gross_generation_mwh = sum(gross_generation_mwh, na.rm = TRUE),
      auxiliary_consumption_mwh = sum(auxiliary_consumption_mwh, na.rm = TRUE),
      net_electricity_mwh = sum(net_electricity_mwh, na.rm = TRUE),
      combined_margin_ef = mean(combined_margin_ef, na.rm = TRUE),
      fossil_fuel_tj = sum(fossil_fuel_tj, na.rm = TRUE),
      fossil_emission_factor = mean(fossil_emission_factor, na.rm = TRUE),
      electricity_import_mwh = sum(electricity_import_mwh, na.rm = TRUE),
      import_emission_factor = mean(import_emission_factor, na.rm = TRUE),
      leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      project_emissions = sum(project_emissions, na.rm = TRUE),
      emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Estimate total emission reductions under ACM0002
#'
#' Applies ACM0002 calculation steps to monitoring data and returns portfolio
#' totals useful for project design documents.
#'
#' @inheritParams aggregate_monitoring_periods
#' @return Tibble with total generation, emissions, and emission reductions.
#' @examples
#' data <- simulate_acm0002_dataset(6, seed = 2024)
#' estimate_emission_reductions_acm0002(data)
#' @export
estimate_emission_reductions_acm0002 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_gross_generation_mwh = sum(gross_generation_mwh, na.rm = TRUE),
      total_auxiliary_consumption_mwh = sum(auxiliary_consumption_mwh, na.rm = TRUE),
      total_net_electricity_mwh = sum(net_electricity_mwh, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
