#' Aggregate monitoring data for ACM0009
#'
#' Summarises period-level monitoring data for ACM0009 projects, returning one
#' row per reporting period with total baseline, project, leakage, and net
#' emission reductions.
#'
#' @param data Tibble containing at least the columns `period`,
#'   `baseline_emissions_tco2e`, `project_emissions_tco2e`,
#'   `leakage_emissions_tco2e`, and `net_emission_reductions_tco2e`.
#' @param period_col Column name (string) representing the period identifier.
#'   Defaults to `"period"`.
#'
#' @return Tibble grouped by period with aggregated emissions components.
#' @examples
#' dataset <- simulate_acm0009_dataset(periods = 3, seed = 42)
#' results <- estimate_emission_reductions_acm0009(dataset)
#' aggregate_monitoring_periods_acm0009(results$period_results)
#' @export
aggregate_monitoring_periods_acm0009 <- function(data, period_col = "period") {
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

#' Estimate emission reductions for ACM0009
#'
#' Applies the ACM0009 workflow to monitoring data by computing baseline,
#' project, and leakage emissions, returning both period-level detail and total
#' emission reductions.
#'
#' @param data Tibble of monitoring observations with columns: `period`,
#'   `baseline_fuel_quantity`, `baseline_ncv_tj_per_unit`,
#'   `baseline_emission_factor_tco2_per_tj`, `project_fuel_quantity`,
#'   `project_ncv_tj_per_unit`, `project_emission_factor_tco2_per_tj`,
#'   `methane_slip_m3`, and `additional_leakage_tco2e`.
#' @param methane_density Density of methane in tonnes per cubic metre. Defaults
#'   to `0.0007168`.
#' @param gwp_ch4 Global warming potential for methane. Defaults to `28`.
#'
#' @return A list with `period_results` and `total_emission_reductions` tibbles.
#' @examples
#' monitoring <- simulate_acm0009_dataset(periods = 4, seed = 101)
#' estimate_emission_reductions_acm0009(monitoring)
#' @export
estimate_emission_reductions_acm0009 <- function(data,
                                                 methane_density = 0.0007168,
                                                 gwp_ch4 = 28) {
  required_cols <- c(
    "period",
    "baseline_fuel_quantity",
    "baseline_ncv_tj_per_unit",
    "baseline_emission_factor_tco2_per_tj",
    "project_fuel_quantity",
    "project_ncv_tj_per_unit",
    "project_emission_factor_tco2_per_tj",
    "methane_slip_m3",
    "additional_leakage_tco2e"
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
      baseline_energy_tj = calculate_energy_content_acm0009(
        baseline_fuel_quantity,
        baseline_ncv_tj_per_unit
      ),
      project_energy_tj = calculate_energy_content_acm0009(
        project_fuel_quantity,
        project_ncv_tj_per_unit
      ),
      baseline_emissions_tco2e = calculate_baseline_emissions_acm0009(
        baseline_fuel_quantity,
        baseline_ncv_tj_per_unit,
        baseline_emission_factor_tco2_per_tj
      ),
      project_emissions_tco2e = calculate_project_emissions_acm0009(
        project_fuel_quantity,
        project_ncv_tj_per_unit,
        project_emission_factor_tco2_per_tj
      ),
      leakage_emissions_tco2e = calculate_leakage_emissions_acm0009(
        methane_slip_m3,
        methane_density = methane_density,
        gwp_ch4 = gwp_ch4,
        additional_leakage_tco2e = additional_leakage_tco2e
      ),
      net_emission_reductions_tco2e = calculate_net_emission_reductions_acm0009(
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
