#' Run the AMS-II.E emission reduction workflow
#'
#' This meta-function orchestrates the numbered equations of AMS-II.E to compute
#' annual emission reductions for building energy efficiency and fuel switching
#' projects. Users provide baseline and project datasets describing thermal and
#' electrical energy use along with optional leakage estimates. The helper
#' delegates to [calculate_baseline_building_emissions()],
#' [calculate_project_building_emissions()], and [calculate_leakage_emissions()]
#' before combining the results with [estimate_emission_reductions()].
#'
#' @param baseline_data Tidy baseline dataset describing counterfactual building
#'   energy use.
#' @param project_data Tidy project dataset describing monitored building energy
#'   use.
#' @param leakage_data Optional tibble of leakage components with the same
#'   grouping structure as the baseline/project data.
#' @param group_cols Character vector of grouping columns common to the supplied
#'   datasets.
#' @param baseline_args Optional list of additional arguments passed to
#'   [calculate_baseline_building_emissions()].
#' @param project_args Optional list of additional arguments passed to
#'   [calculate_project_building_emissions()].
#' @param leakage_args Optional list of additional arguments passed to
#'   [calculate_leakage_emissions()].
#' @param reduction_args Optional list of arguments forwarded to
#'   [estimate_emission_reductions()]. `group_cols` is added automatically.
#' @return A tibble containing baseline, project, leakage, and emission reduction
#'   totals by the requested grouping structure.
#' @examples
#' baseline <- tibble::tibble(
#'   building_id = "Office_A",
#'   baseline_thermal_energy_gj = 420,
#'   baseline_thermal_emission_factor_tco2_per_gj = 0.056,
#'   baseline_electricity_mwh = 120,
#'   baseline_electricity_emission_factor_tco2_per_mwh = 0.62
#' )
#' project <- tibble::tibble(
#'   building_id = "Office_A",
#'   project_thermal_energy_gj = 210,
#'   project_thermal_emission_factor_tco2_per_gj = 0.034,
#'   project_electricity_mwh = 86,
#'   project_electricity_emission_factor_tco2_per_mwh = 0.62
#' )
#' estimate_emission_reductions_ams_iie(baseline, project, group_cols = "building_id")
#' @export
estimate_emission_reductions_ams_iie <- function(baseline_data,
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

  baseline_emissions <- do.call(calculate_baseline_building_emissions, baseline_inputs)
  project_emissions <- do.call(calculate_project_building_emissions, project_inputs)
  leakage_emissions <- if (!is.null(leakage_data)) {
    do.call(calculate_leakage_emissions, leakage_inputs)
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

  do.call(estimate_emission_reductions, reduction_inputs)
}

#' Aggregate monitoring periods for AMS-II.E projects
#'
#' Building monitoring data are often recorded monthly or quarterly. This helper
#' aggregates tidy monitoring records to the reporting period (typically annual)
#' while summing numeric columns such as energy consumption and service proxies.
#'
#' @param monitoring_data Tibble containing monitoring records.
#' @param period_col Column containing the monitoring period label (e.g. month or
#'   quarter).
#' @param group_cols Character vector of columns describing building identifiers.
#' @param summarise_cols Optional character vector of numeric columns to sum. If
#'   `NULL`, all numeric columns are aggregated.
#' @param output_col Name of the column storing the concatenated period labels.
#' @return A tibble aggregated by `group_cols` with numeric columns summed across
#'   the monitoring periods.
#' @examples
#' monitoring <- tibble::tibble(
#'   building_id = rep("Office_A", 3),
#'   month = c("Jan", "Feb", "Mar"),
#'   project_total_energy_mwh = c(10.5, 9.8, 9.2),
#'   service_level_indicator = c(0.32, 0.31, 0.33)
#' )
#' aggregate_monitoring_periods(monitoring, period_col = month, group_cols = "building_id")
#' @export
aggregate_monitoring_periods <- function(monitoring_data,
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
