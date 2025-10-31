#' Aggregate monitoring periods for ACM0018
#'
#' Summarises biomass power plant monitoring data into period-level totals
#' following the ACM0018 calculation sequence.
#'
#' @param monitoring_data Data frame or tibble containing monitoring
#'   observations with at least the following columns: `period`,
#'   `electricity_output_mwh`, `baseline_emission_factor`,
#'   `auxiliary_fossil_tj`, `auxiliary_fossil_ef`,
#'   `onsite_generation_mwh`, `onsite_emission_factor`,
#'   `biomass_transport_tkm`, `transport_emission_factor`, and
#'   `leakage_fraction`.
#'
#' @return Tibble with period-level totals and emissions outcomes.
#' @examples
#' data <- simulate_acm0018_dataset(3, seed = 21)
#' aggregate_monitoring_periods_acm0018(data)
#' @export
aggregate_monitoring_periods_acm0018 <- function(monitoring_data) {
  required <- c(
    "period",
    "electricity_output_mwh",
    "baseline_emission_factor",
    "auxiliary_fossil_tj",
    "auxiliary_fossil_ef",
    "onsite_generation_mwh",
    "onsite_emission_factor",
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
      baseline_emissions = calculate_baseline_emissions_acm0018(
        electricity_output_mwh,
        baseline_emission_factor
      ),
      project_emissions = calculate_project_emissions_acm0018(
        auxiliary_fossil_tj,
        auxiliary_fossil_ef,
        onsite_generation_mwh,
        onsite_emission_factor,
        biomass_transport_tkm,
        transport_emission_factor
      ),
      leakage_emissions = calculate_leakage_emissions_acm0018(
        leakage_fraction,
        baseline_emissions
      ),
      emission_reductions = calculate_emission_reductions_acm0018(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      )
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      electricity_output_mwh = sum(electricity_output_mwh, na.rm = TRUE),
      baseline_emission_factor = mean(baseline_emission_factor, na.rm = TRUE),
      auxiliary_fossil_tj = sum(auxiliary_fossil_tj, na.rm = TRUE),
      auxiliary_fossil_ef = mean(auxiliary_fossil_ef, na.rm = TRUE),
      onsite_generation_mwh = sum(onsite_generation_mwh, na.rm = TRUE),
      onsite_emission_factor = mean(onsite_emission_factor, na.rm = TRUE),
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

#' Estimate total emission reductions under ACM0018
#'
#' Aggregates monitoring data to compute total baseline, project, leakage, and
#' emission reductions suitable for project documentation.
#'
#' @inheritParams aggregate_monitoring_periods_acm0018
#'
#' @return Tibble with total emissions accounting for the monitoring dataset.
#' @examples
#' data <- simulate_acm0018_dataset(4, seed = 99)
#' estimate_emission_reductions_acm0018(data)
#' @export
estimate_emission_reductions_acm0018 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods_acm0018(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_electricity_output_mwh = sum(electricity_output_mwh, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_emission_reductions = sum(emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
