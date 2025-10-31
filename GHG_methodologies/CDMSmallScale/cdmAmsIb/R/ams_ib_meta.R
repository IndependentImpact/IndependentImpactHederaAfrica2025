#' Estimate emission reductions under AMS-I.B
#'
#' Composes the AMS-I.B equation helpers to estimate emission reductions for a dataset describing
#' fossil fuel consumption displaced by a renewable mechanical energy system.
#'
#' @param fuel_data Tibble containing baseline fuel consumption data.
#' @param consumption_col Name of the column with baseline fuel consumption (e.g. litres or kg).
#' @param ncv_col Name of the column with net calorific value in MJ per unit of fuel.
#' @param group_cols Optional character vector specifying grouping columns (e.g. machine identifiers).
#' @param emission_factor Baseline emission factor in tCO2e/MJ.
#' @param project_energy_col Optional column name containing residual project fossil energy in MJ.
#' @param project_emission_factor Project emission factor in tCO2e/MJ (default 0).
#' @return A tibble containing baseline energy, baseline emissions, project energy, project emissions, and emission reductions.
#' @examples
#' fuel <- tibble::tibble(machine_id = c("pump-1", "pump-1", "mill-3"),
#'                        fuel_consumption = c(200, 180, 150),
#'                        net_calorific_value = c(43, 43, 42))
#' estimate_emission_reductions_ams_ib(fuel, emission_factor = 0.00007)
#' @export
estimate_emission_reductions_ams_ib <- function(fuel_data,
                                                consumption_col = "fuel_consumption",
                                                ncv_col = "net_calorific_value",
                                                group_cols = NULL,
                                                emission_factor,
                                                project_energy_col = NULL,
                                                project_emission_factor = 0) {
  keys <- if (is.null(group_cols) || length(group_cols) == 0) character() else unique(group_cols)

  baseline_energy <- calculate_baseline_energy_content(
    fuel_data = fuel_data,
    consumption_col = consumption_col,
    ncv_col = ncv_col,
    group_cols = keys,
    output_col = "baseline_energy_mj"
  )

  baseline_emissions <- calculate_baseline_emissions(
    energy_data = baseline_energy,
    energy_col = "baseline_energy_mj",
    emission_factor = emission_factor,
    output_col = "baseline_emissions_tco2e"
  )

  if (!is.null(project_energy_col)) {
    project_energy_tbl <- dplyr::as_tibble(fuel_data)
    if (!project_energy_col %in% names(project_energy_tbl)) {
      stop("`project_energy_col` must exist in `fuel_data` when supplied.", call. = FALSE)
    }
    select_cols <- c(keys, project_energy_col)
    project_energy_tbl <- project_energy_tbl |>
      dplyr::select(dplyr::all_of(select_cols)) |>
      dplyr::rename(project_energy_mj = dplyr::all_of(project_energy_col))

    if (length(keys) > 0) {
      project_energy_tbl <- project_energy_tbl |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
        dplyr::summarise(project_energy_mj = sum(project_energy_mj, na.rm = TRUE), .groups = "drop")
    } else {
      project_energy_tbl <- project_energy_tbl |>
        dplyr::summarise(project_energy_mj = sum(project_energy_mj, na.rm = TRUE))
    }
  } else {
    if (length(keys) > 0) {
      project_energy_tbl <- baseline_energy |>
        dplyr::select(dplyr::all_of(keys)) |>
        dplyr::mutate(project_energy_mj = 0)
    } else {
      project_energy_tbl <- tibble::tibble(project_energy_mj = 0)
    }
  }

  project_emissions <- calculate_project_emissions(
    project_energy = project_energy_tbl,
    energy_col = "project_energy_mj",
    project_emission_factor = project_emission_factor,
    output_col = "project_emissions_tco2e"
  )

  if (length(keys) == 0) {
    baseline_emissions$.__all__ <- 1
    project_emissions$.__all__ <- 1
  }

  result <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions,
    baseline_col = "baseline_emissions_tco2e",
    project_col = "project_emissions_tco2e",
    output_col = "emission_reductions_tco2e"
  )

  if (length(keys) == 0) {
    result$.__all__ <- NULL
  }

  if (length(keys) > 0) {
    result <- result |>
      dplyr::relocate(dplyr::all_of(keys), .before = dplyr::everything()) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  }

  result
}

#' Aggregate monitoring results for AMS-I.B
#'
#' Summarises monitoring data across reporting periods, combining baseline fuel consumption and
#' residual project fossil energy to return period-level emission reductions.
#'
#' @param monitoring_data Tibble containing monitoring observations.
#' @param monitoring_cols Character vector specifying the columns that define a monitoring period.
#' @param group_cols Character vector specifying entity-level identifiers (e.g. machine or site IDs).
#' @param consumption_col Name of the column with baseline fuel consumption.
#' @param ncv_col Name of the column with net calorific value in MJ per unit fuel.
#' @param emission_factor_col Name of the column storing the emission factor in tCO2e/MJ.
#' @param project_energy_col Name of the column storing project fossil energy consumption in MJ.
#' @return A tibble aggregated by entity and monitoring period with baseline energy, baseline emissions,
#'   project energy, project emissions, and emission reductions.
#' @examples
#' data <- simulate_ams_ib_dataset(n_machines = 2, n_periods = 3)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(monitoring_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "machine_id",
                                         consumption_col = "fuel_consumption",
                                         ncv_col = "net_calorific_value",
                                         emission_factor_col = "emission_factor",
                                         project_energy_col = "project_energy_mj") {
  data_tbl <- tibble::as_tibble(monitoring_data)

  required_cols <- unique(c(group_cols, monitoring_cols, consumption_col, ncv_col, emission_factor_col, project_energy_col))
  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`monitoring_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  keys <- unique(c(group_cols, monitoring_cols))

  baseline_energy <- calculate_baseline_energy_content(
    fuel_data = data_tbl,
    consumption_col = consumption_col,
    ncv_col = ncv_col,
    group_cols = keys,
    output_col = "baseline_energy_mj"
  )

  emission_factors <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(emission_factor = dplyr::first(.data[[emission_factor_col]]), .groups = "drop")

  baseline_with_factors <- dplyr::left_join(baseline_energy, emission_factors, by = keys)

  baseline_emissions <- calculate_baseline_emissions(
    energy_data = baseline_with_factors,
    energy_col = "baseline_energy_mj",
    emission_factor = baseline_with_factors$emission_factor,
    output_col = "baseline_emissions_tco2e"
  )

  project_energy <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(project_energy_mj = sum(.data[[project_energy_col]], na.rm = TRUE), .groups = "drop")

  project_with_factors <- dplyr::left_join(project_energy, emission_factors, by = keys)

  project_emissions <- calculate_project_emissions(
    project_energy = project_with_factors,
    energy_col = "project_energy_mj",
    project_emission_factor = project_with_factors$emission_factor,
    output_col = "project_emissions_tco2e"
  ) |>
    dplyr::select(-emission_factor)

  emission_reductions <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions,
    baseline_col = "baseline_emissions_tco2e",
    project_col = "project_emissions_tco2e",
    output_col = "emission_reductions_tco2e"
  )

  emission_reductions |>
    dplyr::relocate(dplyr::all_of(keys)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
}
