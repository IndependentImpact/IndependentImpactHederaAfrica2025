#' Run the AMS-II.D emission reduction workflow
#'
#' This meta-function orchestrates all numbered equations in AMS-II.D to compute
#' annual emission reductions. Users supply baseline and project monitoring data
#' together with leakage estimates. The function delegates to
#' [calculate_baseline_fossil_emissions()],
#' [calculate_project_fossil_emissions()], and
#' [calculate_leakage_emissions()] before combining results with
#' [estimate_emission_reductions()].
#'
#' @param baseline_data Tidy data describing counterfactual fuel consumption and
#'   characteristics.
#' @param project_data Tidy data describing monitored project fuel consumption
#'   and characteristics.
#' @param leakage_data Optional tibble of leakage components for the same
#'   grouping structure as the baseline/project data.
#' @param group_cols Character vector of grouping columns common to the three
#'   datasets.
#' @param baseline_args Optional list of additional arguments passed to
#'   [calculate_baseline_fossil_emissions()].
#' @param project_args Optional list of additional arguments passed to
#'   [calculate_project_fossil_emissions()].
#' @param leakage_args Optional list of additional arguments passed to
#'   [calculate_leakage_emissions()].
#' @param reduction_args Optional list of additional arguments passed to
#'   [estimate_emission_reductions()]. `group_cols` and default column names are
#'   supplied automatically.
#' @return A tibble with emission reductions and intermediate emission accounts.
#' @examples
#' baseline <- tibble::tibble(unit = "Kiln",
#'                            baseline_fuel_quantity = 1200,
#'                            baseline_ncv_gj_per_unit = 0.038,
#'                            baseline_emission_factor_tco2_per_gj = 0.094,
#'                            baseline_efficiency = 0.72)
#' project <- tibble::tibble(unit = "Kiln",
#'                           project_fuel_quantity = 950,
#'                           project_ncv_gj_per_unit = 0.038,
#'                           project_emission_factor_tco2_per_gj = 0.094,
#'                           project_efficiency = 0.84)
#' leakage <- tibble::tibble(unit = "Kiln", leakage_emissions_tco2e = 5)
#' estimate_emission_reductions_ams_iid(baseline, project, leakage, group_cols = "unit")
#' @export
estimate_emission_reductions_ams_iid <- function(baseline_data,
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

  baseline_emissions <- do.call(calculate_baseline_fossil_emissions, baseline_inputs)
  project_emissions <- do.call(calculate_project_fossil_emissions, project_inputs)
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

#' Aggregate monitoring periods for AMS-II.D projects
#'
#' Monitoring data in AMS-II.D projects often arrive at sub-annual cadence. This
#' helper aggregates tidy monitoring records to the project reporting periods,
#' typically annual. Aggregation sums all numeric columns, preserving grouping
#' identifiers and ordered monitoring boundaries.
#'
#' @param monitoring_data Tibble containing monitoring records.
#' @param period_col Column containing the monitoring period label (e.g. month or
#'   quarter).
#' @param group_cols Character vector of columns describing equipment or site
#'   groupings.
#' @param summarise_cols Optional character vector of numeric columns to sum. If
#'   `NULL`, all numeric columns are aggregated.
#' @param output_col Name of the column storing the concatenated period labels.
#' @return A tibble aggregated by `group_cols` with numeric columns summed over
#'   the monitoring period.
#' @examples
#' monitoring <- tibble::tibble(
#'   unit = rep("Kiln", 3),
#'   month = c("Jan", "Feb", "Mar"),
#'   project_fuel_quantity = c(80, 75, 70),
#'   electricity_emissions_tco2e = c(0.5, 0.5, 0.4)
#' )
#' aggregate_monitoring_periods(monitoring,
#'   period_col = month,
#'   group_cols = "unit"
#' )
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
