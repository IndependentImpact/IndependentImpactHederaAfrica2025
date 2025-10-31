#' Aggregate monitoring periods for ACM0008
#'
#' Summarises coal mine methane monitoring data into period-level emission
#' accounting consistent with ACM0008. The function calculates baseline, project
#' and leakage emissions for each record before aggregating to the reporting
#' period supplied in the input dataset.
#'
#' @param monitoring_data Data frame or tibble containing at least the columns
#'   `period`, `flow_rate_m3_per_h`, `operating_hours`, `methane_fraction`,
#'   `oxidation_efficiency`, `electricity_use_mwh`, and
#'   `grid_emission_factor_t_per_mwh`.
#'
#' @return Tibble with period-level totals including recovered volume,
#'   emissions, and net reductions.
#'
#' @examples
#' data <- simulate_acm0008_dataset(3, seed = 42)
#' aggregate_monitoring_periods_acm0008(data)
#' @export
aggregate_monitoring_periods_acm0008 <- function(monitoring_data) {
  required <- c(
    "period",
    "flow_rate_m3_per_h",
    "operating_hours",
    "methane_fraction",
    "oxidation_efficiency",
    "electricity_use_mwh",
    "grid_emission_factor_t_per_mwh"
  )

  missing <- setdiff(required, names(monitoring_data))
  if (length(missing) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }

  monitoring_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      recovered_volume_m3 = calculate_methane_volume_acm0008(
        flow_rate_m3_per_h,
        operating_hours
      ),
      baseline_emissions = calculate_baseline_emissions_acm0008(
        recovered_volume_m3,
        methane_fraction
      ),
      project_emissions = calculate_project_emissions_acm0008(
        recovered_volume_m3,
        methane_fraction,
        oxidation_efficiency
      ),
      leakage_emissions = calculate_leakage_emissions_acm0008(
        electricity_use_mwh,
        grid_emission_factor_t_per_mwh
      ),
      net_emission_reductions = calculate_net_emission_reductions_acm0008(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      )
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      flow_rate_m3_per_h = mean(flow_rate_m3_per_h, na.rm = TRUE),
      operating_hours = sum(operating_hours, na.rm = TRUE),
      methane_fraction = mean(methane_fraction, na.rm = TRUE),
      oxidation_efficiency = mean(oxidation_efficiency, na.rm = TRUE),
      electricity_use_mwh = sum(electricity_use_mwh, na.rm = TRUE),
      grid_emission_factor_t_per_mwh = mean(grid_emission_factor_t_per_mwh, na.rm = TRUE),
      recovered_volume_m3 = sum(recovered_volume_m3, na.rm = TRUE),
      baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      project_emissions = sum(project_emissions, na.rm = TRUE),
      leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      net_emission_reductions = sum(net_emission_reductions, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Estimate total emission reductions under ACM0008
#'
#' Aggregates monitoring data to compute total baseline, project, leakage, and
#' net emission reductions suitable for project documentation and reporting.
#'
#' @inheritParams aggregate_monitoring_periods_acm0008
#' @return Tibble with one row summarising total emissions accounting results.
#'
#' @examples
#' data <- simulate_acm0008_dataset(4, seed = 99)
#' estimate_emission_reductions_acm0008(data)
#' @export
estimate_emission_reductions_acm0008 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods_acm0008(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_recovered_volume_m3 = sum(recovered_volume_m3, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_net_emission_reductions = sum(net_emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
