#' Run the AMS-II.F emission reduction workflow
#'
#' Orchestrates the numbered equations for AMS-II.F to compute annual emission
#' reductions from agricultural energy efficiency or fuel switching projects.
#' The meta-function wires together
#' [calculate_baseline_agricultural_emissions()],
#' [calculate_project_agricultural_emissions()],
#' [calculate_leakage_emissions_iif()], and
#' [calculate_emission_reductions_iif()] while allowing optional fine-tuning via
#' argument lists.
#'
#' @param baseline_data Tidy baseline dataset describing counterfactual fuel and
#'   electricity use.
#' @param project_data Tidy project dataset describing monitored fuel and
#'   electricity use.
#' @param leakage_data Optional tibble of leakage components aligned with the
#'   grouping structure of the baseline/project data.
#' @param group_cols Character vector of grouping columns common to the supplied
#'   datasets.
#' @param baseline_args Optional list of additional arguments passed to
#'   [calculate_baseline_agricultural_emissions()].
#' @param project_args Optional list of additional arguments passed to
#'   [calculate_project_agricultural_emissions()].
#' @param leakage_args Optional list of additional arguments passed to
#'   [calculate_leakage_emissions_iif()].
#' @param reduction_args Optional list of arguments forwarded to
#'   [calculate_emission_reductions_iif()]. `group_cols` is injected
#'   automatically.
#' @return A tibble containing baseline, project, leakage, and emission reduction
#'   totals by the requested grouping structure.
#' @examples
#' baseline <- tibble::tibble(
#'   facility_id = "rice_mill_1",
#'   baseline_fuel_energy_gj = 960,
#'   baseline_fuel_emission_factor_tco2_per_gj = 0.072,
#'   baseline_electricity_mwh = 180,
#'   baseline_electricity_emission_factor_tco2_per_mwh = 0.61
#' )
#' project <- tibble::tibble(
#'   facility_id = "rice_mill_1",
#'   project_fuel_energy_gj = 520,
#'   project_fuel_emission_factor_tco2_per_gj = 0.03,
#'   project_electricity_mwh = 150,
#'   project_electricity_emission_factor_tco2_per_mwh = 0.61
#' )
#' estimate_emission_reductions_ams_iif(baseline, project, group_cols = "facility_id")
#' @export
estimate_emission_reductions_ams_iif <- function(baseline_data,
                                                 project_data,
                                                 leakage_data = NULL,
                                                 group_cols = NULL,
                                                 baseline_args = list(),
                                                 project_args = list(),
                                                 leakage_args = list(),
                                                 reduction_args = list()) {
  baseline_inputs <- c(list(baseline_data = baseline_data, group_cols = group_cols), baseline_args)
  project_inputs <- c(list(project_data = project_data, group_cols = group_cols), project_args)
  leakage_inputs <- c(list(leakage_data = leakage_data, group_cols = group_cols), leakage_args)

  baseline_emissions <- do.call(calculate_baseline_agricultural_emissions, baseline_inputs)
  project_emissions <- do.call(calculate_project_agricultural_emissions, project_inputs)
  leakage_emissions <- if (!is.null(leakage_data)) {
    do.call(calculate_leakage_emissions_iif, leakage_inputs)
  } else {
    NULL
  }

  reduction_inputs <- c(
    list(
      baseline_emissions = baseline_emissions,
      project_emissions = project_emissions,
      leakage_emissions = leakage_emissions,
      group_cols = group_cols
    ),
    reduction_args
  )

  do.call(calculate_emission_reductions_iif, reduction_inputs)
}

#' Aggregate monitoring periods for AMS-II.F projects
#'
#' Agricultural monitoring data may be captured for crop seasons, irrigation
#' cycles, or monthly production runs. This helper aggregates tidy monitoring
#' records to the reporting period while concatenating the period labels and
#' summing numeric metrics such as energy consumption, service proxies, and
#' production levels.
#'
#' @param monitoring_data Tibble containing monitoring records.
#' @param period_col Column containing the monitoring period label (e.g. season or
#'   month).
#' @param group_cols Character vector of columns describing facility identifiers.
#' @param summarise_cols Optional character vector of numeric columns to sum. If
#'   `NULL`, all numeric columns are aggregated.
#' @param output_col Name of the column storing the concatenated period labels.
#' @return A tibble aggregated by `group_cols` with numeric columns summed across
#'   the monitoring periods.
#' @examples
#' monitoring <- tibble::tibble(
#'   facility_id = rep("rice_mill_1", 3),
#'   season = c("Early", "Mid", "Late"),
#'   project_total_energy_mwh = c(12.5, 11.8, 11.2),
#'   processed_grain_tonnes = c(140, 150, 138)
#' )
#' aggregate_monitoring_periods_iif(monitoring, period_col = season, group_cols = "facility_id")
#' @export
aggregate_monitoring_periods_iif <- function(monitoring_data,
                                             period_col = "monitoring_period",
                                             group_cols = NULL,
                                             summarise_cols = NULL,
                                             output_col = "monitoring_period") {
  period_sym <- rlang::ensym(period_col)
  output_sym <- rlang::ensym(output_col)

  data_tbl <- dplyr::as_tibble(monitoring_data)

  if (!is.null(group_cols) && length(group_cols) > 0) {
    data_tbl <- data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  numeric_cols <- if (is.null(summarise_cols)) {
    names(
      data_tbl |>
        dplyr::ungroup() |>
        dplyr::select(where(is.numeric))
    )
  } else {
    summarise_cols
  }

  if (!is.null(group_cols) && length(group_cols) > 0) {
    numeric_cols <- setdiff(numeric_cols, group_cols)
  }
  numeric_cols <- setdiff(numeric_cols, rlang::as_string(period_sym))
  numeric_cols <- setdiff(numeric_cols, rlang::as_string(output_sym))

  data_tbl |>
    dplyr::summarise(
      !!output_sym := paste(unique(!!period_sym), collapse = ", "),
      dplyr::across(dplyr::all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}
