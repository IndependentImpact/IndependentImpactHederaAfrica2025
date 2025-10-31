#' Aggregate ACM0013 monitoring periods
#'
#' Summarises monitoring data to the reporting-period level, computing total
#' baseline, project, leakage, and net emission reductions.
#'
#' @param data A tibble containing at least the columns `period`,
#'   `baseline_emissions_tco2e`, `project_emissions_tco2e`, and
#'   `leakage_emissions_tco2e`.
#' @param period_col Column name (as string) describing the monitoring period.
#'   Defaults to `"period"`.
#' @return A tibble with one row per period and aggregated emission components.
#' @examples
#' dataset <- simulate_acm0013_dataset(periods = 2, seed = 123)
#' summary <- estimate_emission_reductions_acm0013(dataset)
#' aggregate_monitoring_periods_acm0013(summary$period_results)
#' @export
aggregate_monitoring_periods_acm0013 <- function(data, period_col = "period") {
  if (!period_col %in% names(data)) {
    rlang::abort(sprintf("Column `%s` not found in `data`.", period_col))
  }

  period_sym <- rlang::sym(period_col)

  data |>
    dplyr::group_by(!!period_sym) |>
    dplyr::summarise(
      dplyr::across(
        c(
          baseline_emissions_tco2e,
          project_emissions_tco2e,
          leakage_emissions_tco2e,
          net_emission_reductions_tco2e
        ),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

#' Estimate ACM0013 emission reductions
#'
#' Applies the ACM0013 workflow to monitoring data by computing baseline,
#' project, and leakage emissions before returning period-level detail and total
#' emission reductions.
#'
#' @param data Tibble of monitoring observations from an ACM0013 project.
#'   Expected columns include: `period`, `electricity_sent_out_mwh`,
#'   `baseline_emission_factor_tco2_per_mwh`, `fuel_consumed`,
#'   `fuel_emission_factor_tco2_per_unit`, `auxiliary_electricity_mwh`,
#'   `auxiliary_emission_factor_tco2_per_mwh`, `other_project_emissions_tco2e`,
#'   `upstream_fuel_emissions_tco2e`, `displaced_unit_emissions_tco2e`, and
#'   `other_leakage_tco2e`.
#' @return A list with two components: `period_results` (tibble of period-level
#'   emissions) and `total_emission_reductions` (tibble summarising the total net
#'   reductions).
#' @examples
#' monitoring <- simulate_acm0013_dataset(periods = 6, seed = 99)
#' estimate_emission_reductions_acm0013(monitoring)
#' @export
estimate_emission_reductions_acm0013 <- function(data) {
  required_cols <- c(
    "period", "electricity_sent_out_mwh", "baseline_emission_factor_tco2_per_mwh",
    "fuel_consumed", "fuel_emission_factor_tco2_per_unit",
    "auxiliary_electricity_mwh", "auxiliary_emission_factor_tco2_per_mwh",
    "other_project_emissions_tco2e", "upstream_fuel_emissions_tco2e",
    "displaced_unit_emissions_tco2e", "other_leakage_tco2e"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(sprintf(
      "Missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  period_results <- data |>
    dplyr::mutate(
      baseline_emissions_tco2e = calculate_baseline_emissions_acm0013(
        electricity_sent_out_mwh, baseline_emission_factor_tco2_per_mwh
      ),
      project_emissions_tco2e = calculate_project_emissions_acm0013(
        fuel_consumed, fuel_emission_factor_tco2_per_unit,
        auxiliary_electricity_mwh = auxiliary_electricity_mwh,
        auxiliary_emission_factor_tco2_per_mwh = auxiliary_emission_factor_tco2_per_mwh,
        other_project_emissions_tco2e = other_project_emissions_tco2e
      ),
      leakage_emissions_tco2e = calculate_leakage_emissions_acm0013(
        upstream_fuel_emissions_tco2e,
        displaced_unit_emissions_tco2e,
        other_leakage_tco2e
      ),
      net_emission_reductions_tco2e = calculate_net_emission_reductions_acm0013(
        baseline_emissions_tco2e,
        project_emissions_tco2e,
        leakage_emissions_tco2e
      )
    )

  totals <- tibble::tibble(
    baseline_emissions_tco2e = sum(period_results$baseline_emissions_tco2e, na.rm = TRUE),
    project_emissions_tco2e = sum(period_results$project_emissions_tco2e, na.rm = TRUE),
    leakage_emissions_tco2e = sum(period_results$leakage_emissions_tco2e, na.rm = TRUE),
    net_emission_reductions_tco2e = sum(period_results$net_emission_reductions_tco2e, na.rm = TRUE)
  )

  list(
    period_results = period_results,
    total_emission_reductions = totals
  )
}
