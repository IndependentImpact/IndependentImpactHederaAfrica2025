
#' Estimate emission reductions under AMS-I.A
#'
#' Composes the equation-level functions to compute emission reductions for a dataset describing
#' user-level electricity generation and the grid emission factor.
#'
#' @param generation_data Tibble containing user-level electricity generation in kWh.
#' @param grid_emission_factor Grid emission factor in tCO2e/kWh.
#' @param project_emission_factor Optional project emission factor in tCO2e/kWh.
#' @param group_cols Optional character vector specifying grouping columns in `generation_data`.
#' @return A tibble with baseline generation, baseline emissions, project emissions, and emission reductions.
#' @examples
#' generation <- tibble::tibble(user_id = c("A", "B"), generation_kwh = c(1200, 1500))
#' estimate_emission_reductions_ams_ia(generation, grid_emission_factor = 0.8)
#' @export
estimate_emission_reductions_ams_ia <- function(generation_data,
                                                grid_emission_factor,
                                                project_emission_factor = 0,
                                                group_cols = NULL) {
  baseline_generation <- calculate_baseline_generation(
    generation_data = generation_data,
    group_cols = group_cols
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_generation = baseline_generation,
    grid_emission_factor = grid_emission_factor
  )

  project_emissions <- calculate_project_emissions(
    baseline_generation = baseline_generation,
    project_emission_factor = project_emission_factor
  )

  calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )
}

#' Aggregate monitoring results across periods
#'
#' Summarises simulated or observed generation data for each monitoring period, returning
#' baseline generation, emissions, and emission reductions. The helper leverages the
#' equation-level functions to maintain consistency with AMS-I.A calculations.
#'
#' @param generation_data Tibble containing monitoring observations, including the columns
#'   listed in `group_cols` and `monitoring_cols` plus generation and emission details.
#' @param monitoring_cols Character vector specifying the columns that define a monitoring period.
#' @param group_cols Character vector specifying entity-level identifiers (e.g. `user_id`).
#' @param generation_col Name of the column with electricity generation for each observation in kWh.
#' @param grid_factor_col Name of the column storing the grid emission factor.
#' @param project_col Name of the project emissions column (aggregated as the sum across the period).
#' @return A tibble aggregated by entity and monitoring period with columns for baseline generation,
#'   baseline emissions, project emissions, and emission reductions.
#' @examples
#' data <- simulate_ams_ia_dataset(n_users = 2, n_periods = 3)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(generation_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "user_id",
                                         generation_col = "generation_kwh",
                                         grid_factor_col = "grid_emission_factor",
                                         project_col = "project_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(generation_data)

  generation_sym <- if (is.character(generation_col)) {
    if (length(generation_col) != 1) {
      stop("`generation_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(generation_col)
  } else {
    rlang::ensym(generation_col)
  }
  grid_factor_sym <- if (is.character(grid_factor_col)) {
    if (length(grid_factor_col) != 1) {
      stop("`grid_factor_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(grid_factor_col)
  } else {
    rlang::ensym(grid_factor_col)
  }
  project_sym <- if (is.character(project_col)) {
    if (length(project_col) != 1) {
      stop("`project_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(project_col)
  } else {
    rlang::ensym(project_col)
  }

  generation_col <- rlang::as_string(generation_sym)
  grid_factor_col <- rlang::as_string(grid_factor_sym)
  project_col <- rlang::as_string(project_sym)

  keys <- unique(c(group_cols, monitoring_cols))
  required_cols <- unique(c(keys, generation_col, grid_factor_col, project_col))
  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`generation_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  baseline_generation <- calculate_baseline_generation(
    generation_data = data_tbl,
    generation_col = generation_col,
    group_cols = keys
  )

  grid_and_project <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      !!grid_factor_sym := dplyr::first(!!grid_factor_sym),
      !!project_sym := sum(!!project_sym, na.rm = TRUE),
      .groups = "drop"
    )

  baseline_with_factors <- dplyr::left_join(
    baseline_generation,
    grid_and_project,
    by = keys
  )

  baseline_emissions <- calculate_baseline_emissions(
    baseline_generation = baseline_with_factors,
    grid_emission_factor = baseline_with_factors[[grid_factor_col]],
    output_col = "baseline_emissions_tco2e"
  )

  project_summary <- grid_and_project |>
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
      baseline_generation,
      by = keys
    ) |>
    dplyr::left_join(
      grid_and_project |>
        dplyr::select(dplyr::all_of(keys), dplyr::all_of(grid_factor_col)),
      by = keys
    ) |>
    dplyr::relocate(dplyr::all_of(keys)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
}
