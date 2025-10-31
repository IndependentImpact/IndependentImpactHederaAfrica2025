#' Estimate emission reductions under AMS-I.F
#'
#' Composes the equation-level helpers to compute emission reductions for
#' datasets describing electricity supplied to captive and mini-grid consumers
#' along with the associated baseline emission factor.
#'
#' @param supply_data Tibble containing electricity supply observations in MWh.
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWh.
#' @param project_emission_factor Optional project emission factor in tCO2e/MWh.
#' @param group_cols Optional character vector specifying grouping columns in
#'   `supply_data`.
#' @return Tibble with baseline electricity, baseline emissions, project
#'   emissions, and emission reductions.
#' @examples
#' supply <- tibble::tibble(grid_id = c("A", "B"), electricity_mwh = c(900, 750))
#' estimate_emission_reductions_ams_if(supply, baseline_emission_factor = 0.72)
#' @export
estimate_emission_reductions_ams_if <- function(supply_data,
                                               baseline_emission_factor,
                                               project_emission_factor = 0,
                                               group_cols = NULL) {
  baseline_supply <- calculate_baseline_electricity_supply(
    supply_data = supply_data,
    electricity_col = "electricity_mwh",
    group_cols = group_cols
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_supply = baseline_supply,
    baseline_emission_factor = baseline_emission_factor
  )

  project_emissions <- calculate_project_emissions(
    baseline_supply = baseline_supply,
    project_emission_factor = project_emission_factor
  )

  calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )
}

#' Aggregate monitoring results across periods
#'
#' Summarises simulated or observed electricity supply data for each monitoring
#' period, returning baseline supply, emissions, project emissions, and emission
#' reductions. The helper leverages the equation-level functions to maintain
#' consistency with AMS-I.F calculations.
#'
#' @param supply_data Tibble containing monitoring observations, including the
#'   columns listed in `group_cols` and `monitoring_cols` plus electricity supply
#'   and emission information.
#' @param monitoring_cols Character vector specifying the columns that define a
#'   monitoring period.
#' @param group_cols Character vector specifying entity-level identifiers (e.g.
#'   `grid_id`).
#' @param electricity_col Name of the column with electricity supplied for each
#'   observation in MWh.
#' @param baseline_factor_col Name of the column storing the baseline emission
#'   factor.
#' @param project_col Name of the project emissions column (aggregated as the sum
#'   across the period).
#' @return Tibble aggregated by entity and monitoring period with columns for
#'   baseline electricity, baseline emissions, project emissions, and emission
#'   reductions.
#' @examples
#' data <- simulate_ams_if_dataset(n_grids = 2, n_periods = 3)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(supply_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "grid_id",
                                         electricity_col = "electricity_mwh",
                                         baseline_factor_col = "baseline_emission_factor",
                                         project_col = "project_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(supply_data)

  electricity_sym <- if (is.character(electricity_col)) {
    if (length(electricity_col) != 1) {
      stop("`electricity_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(electricity_col)
  } else {
    rlang::ensym(electricity_col)
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

  electricity_col <- rlang::as_string(electricity_sym)
  baseline_factor_col <- rlang::as_string(baseline_factor_sym)
  project_col <- rlang::as_string(project_sym)

  keys <- unique(c(group_cols, monitoring_cols))
  required_cols <- unique(c(keys, electricity_col, baseline_factor_col, project_col))
  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`supply_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  baseline_supply <- calculate_baseline_electricity_supply(
    supply_data = data_tbl,
    electricity_col = electricity_col,
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
    baseline_supply,
    factors_and_project,
    by = keys
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_supply = baseline_with_factors,
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
      baseline_supply,
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
