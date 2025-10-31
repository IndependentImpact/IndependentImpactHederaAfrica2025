#' Aggregate ACM0010 monitoring periods
#'
#' Summarises monitoring data to the reporting-period level, computing total
#' baseline, project, leakage, and net emission reductions.
#'
#' @param data A tibble containing at least the columns `period`, `baseline_emissions_tco2e`,
#'   `project_emissions_tco2e`, and `leakage_emissions_tco2e`.
#' @param period_col Column name (as string) describing the monitoring period.
#'   Defaults to `"period"`.
#' @return A tibble with one row per period and aggregated emission components.
#' @examples
#' dataset <- simulate_acm0010_dataset(periods = 2, seed = 123)
#' summary <- estimate_emission_reductions_acm0010(dataset)
#' aggregate_monitoring_periods_acm0010(summary$period_results)
#' @export
aggregate_monitoring_periods_acm0010 <- function(data, period_col = "period") {
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

#' Estimate ACM0010 emission reductions
#'
#' Applies the ACM0010 workflow to monitoring data by computing baseline,
#' project, and leakage emissions before returning period-level detail and total
#' emission reductions.
#'
#' @param data Tibble of monitoring observations from an ACM0010 project.
#'   Expected columns include: `period`, `cod_in_mg_l`, `cod_out_mg_l`, `flow_m3`,
#'   `methane_conversion_factor`, `methane_recovered_m3`,
#'   `combustion_efficiency`, `electricity_consumed_mwh`,
#'   `grid_emission_factor`, and `additional_leakage_tco2e`.
#' @param gwp_ch4 Methane global warming potential. Defaults to `28`.
#' @param methane_density Density of methane in tonnes per cubic metre. Defaults
#'   to `0.0007168`.
#' @return A list with two components: `period_results` (tibble of period-level
#'   emissions) and `total_emission_reductions` (tibble summarising the total net
#'   reductions).
#' @examples
#' monitoring <- simulate_acm0010_dataset(periods = 6, seed = 99)
#' estimate_emission_reductions_acm0010(monitoring)
#' @export
estimate_emission_reductions_acm0010 <- function(data,
                                                 gwp_ch4 = 28,
                                                 methane_density = 0.0007168) {
  required_cols <- c(
    "period", "cod_in_mg_l", "cod_out_mg_l", "flow_m3",
    "methane_conversion_factor", "methane_recovered_m3",
    "combustion_efficiency", "electricity_consumed_mwh",
    "grid_emission_factor", "additional_leakage_tco2e"
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
      cod_removed_kg = calculate_cod_removed_acm0010(
        cod_in_mg_l, cod_out_mg_l, flow_m3
      ),
      baseline_emissions_tco2e = calculate_baseline_emissions_acm0010(
        cod_removed_kg, methane_conversion_factor, gwp_ch4 = gwp_ch4
      ),
      project_emissions_tco2e = calculate_project_emissions_acm0010(
        methane_recovered_m3, combustion_efficiency,
        methane_density = methane_density, gwp_ch4 = gwp_ch4
      ),
      leakage_emissions_tco2e = calculate_leakage_emissions_acm0010(
        electricity_consumed_mwh, grid_emission_factor, additional_leakage_tco2e
      ),
      net_emission_reductions_tco2e = calculate_net_emission_reductions_acm0010(
        baseline_emissions_tco2e, project_emissions_tco2e, leakage_emissions_tco2e
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
