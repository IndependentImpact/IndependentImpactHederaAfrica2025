#' Estimate emission reductions under AMS-I.C
#'
#' Composes the equation-level helpers to compute emission reductions for datasets capturing
#' useful thermal energy delivered and baseline emission factors.
#'
#' @param thermal_data Tibble containing useful thermal energy observations in MWhth.
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWhth.
#' @param project_emission_factor Optional project emission factor in tCO2e/MWhth.
#' @param group_cols Optional character vector specifying grouping columns in `thermal_data`.
#' @return Tibble with baseline thermal output, baseline emissions, project emissions, and emission reductions.
#' @examples
#' thermal <- tibble::tibble(facility_id = c("A", "B"), thermal_energy_mwh = c(800, 620))
#' estimate_emission_reductions_ams_ic(thermal, baseline_emission_factor = 0.25)
#' @export
estimate_emission_reductions_ams_ic <- function(thermal_data,
                                                baseline_emission_factor,
                                                project_emission_factor = 0,
                                                group_cols = NULL) {
  baseline_output <- calculate_baseline_thermal_output(
    thermal_data = thermal_data,
    energy_col = "thermal_energy_mwh",
    group_cols = group_cols
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_output = baseline_output,
    baseline_emission_factor = baseline_emission_factor
  )

  project_emissions <- calculate_project_emissions(
    baseline_output = baseline_output,
    project_emission_factor = project_emission_factor
  )

  calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )
}

#' Aggregate monitoring results across periods
#'
#' Summarises simulated or observed thermal energy data for each monitoring period, returning
#' baseline thermal output, baseline emissions, project emissions, and emission reductions.
#'
#' @param thermal_data Tibble containing monitoring observations, including the columns listed in
#'   `group_cols` and `monitoring_cols` plus thermal energy and emission information.
#' @param monitoring_cols Character vector specifying the columns that define a monitoring period.
#' @param group_cols Character vector specifying entity-level identifiers (e.g. `facility_id`).
#' @param energy_col Name of the column with thermal energy delivered for each observation in MWhth.
#' @param baseline_factor_col Name of the column storing the baseline emission factor.
#' @param project_col Name of the project emissions column (aggregated as the sum across the period).
#' @return Tibble aggregated by entity and monitoring period with columns for baseline thermal output,
#'   baseline emissions, project emissions, and emission reductions.
#' @examples
#' data <- simulate_ams_ic_dataset(n_facilities = 2, n_periods = 3)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(thermal_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "facility_id",
                                         energy_col = "thermal_energy_mwh",
                                         baseline_factor_col = "baseline_emission_factor",
                                         project_col = "project_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(thermal_data)

  energy_sym <- if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(energy_col)
  } else {
    rlang::ensym(energy_col)
  }
  baseline_factor_sym <- if (is.character(baseline_factor_col)) {
    if (length(baseline_factor_col) != 1) {
      stop("`baseline_factor_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(baseline_factor_col)
  } else {
    rlang::ensym(baseline_factor_col)
  }
  project_sym <- if (is.character(project_col)) {
    if (length(project_col) != 1) {
      stop("`project_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(project_col)
  } else {
    rlang::ensym(project_col)
  }

  energy_col <- rlang::as_string(energy_sym)
  baseline_factor_col <- rlang::as_string(baseline_factor_sym)
  project_col <- rlang::as_string(project_sym)

  keys <- unique(c(group_cols, monitoring_cols))
  required_cols <- unique(c(keys, energy_col, baseline_factor_col, project_col))
  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`thermal_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  baseline_output <- calculate_baseline_thermal_output(
    thermal_data = data_tbl,
    energy_col = energy_col,
    group_cols = keys
  )

  factors_and_project <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      !!baseline_factor_sym := dplyr::first(!!baseline_factor_sym),
      !!project_sym := sum(!!project_sym, na.rm = TRUE),
      .groups = "drop"
    )

  baseline_with_factors <- dplyr::left_join(
    baseline_output,
    factors_and_project,
    by = keys
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_output = baseline_with_factors,
    baseline_emission_factor = baseline_with_factors[[baseline_factor_col]],
    output_col = "baseline_emissions_tco2e"
  )

  project_summary <- factors_and_project |>
    dplyr::select(dplyr::all_of(keys), dplyr::all_of(project_col))

  emission_reductions <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions |>
      dplyr::select(dplyr::all_of(keys), baseline_emissions_tco2e),
    project_emissions = project_summary,
    baseline_col = "baseline_emissions_tco2e",
    project_col = project_col,
    output_col = "emission_reductions_tco2e"
  )

  emission_reductions |>
    dplyr::left_join(
      baseline_output,
      by = keys
    ) |>
    dplyr::left_join(
      factors_and_project |>
        dplyr::select(dplyr::all_of(keys), dplyr::all_of(baseline_factor_col)),
      by = keys
    ) |>
    dplyr::relocate(dplyr::all_of(keys)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
}
