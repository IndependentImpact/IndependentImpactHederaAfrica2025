#' Calculate baseline energy content (Equation 1)
#'
#' Implements Equation (1) of AMS-I.B by converting baseline fossil fuel
#' consumption into energy delivered to the mechanical equipment.
#'
#' @param fuel_data A tibble containing the baseline fuel consumption observations.
#' @param consumption_col Name of the column storing fuel consumption (e.g. litres, kg).
#' @param ncv_col Name of the column storing net calorific value in MJ per unit of fuel.
#' @param group_cols Optional character vector of grouping columns used to aggregate energy content.
#' @param output_col Name of the resulting column containing baseline energy in MJ.
#' @return A tibble containing grouping columns (if supplied) and a column named `output_col` with
#'   baseline energy in MJ.
#' @examples
#' data <- tibble::tibble(machine_id = c("pump-1", "pump-1", "mill-3"),
#'                        fuel_consumption = c(200, 180, 150),
#'                        net_calorific_value = c(43, 43, 42))
#' calculate_baseline_energy_content(data)
#' @export
calculate_baseline_energy_content <- function(fuel_data,
                                              consumption_col = "fuel_consumption",
                                              ncv_col = "net_calorific_value",
                                              group_cols = NULL,
                                              output_col = "baseline_energy_mj") {
  fuel_tbl <- dplyr::as_tibble(fuel_data)

  consumption_sym <- if (is.character(consumption_col)) {
    if (length(consumption_col) != 1) {
      stop("`consumption_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(consumption_col)
  } else {
    rlang::ensym(consumption_col)
  }
  ncv_sym <- if (is.character(ncv_col)) {
    if (length(ncv_col) != 1) {
      stop("`ncv_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(ncv_col)
  } else {
    rlang::ensym(ncv_col)
  }
  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  required_cols <- c(rlang::as_string(consumption_sym), rlang::as_string(ncv_sym))
  missing_cols <- setdiff(required_cols, names(fuel_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`fuel_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  temp <- fuel_tbl |>
    dplyr::mutate(`__energy` = !!consumption_sym * !!ncv_sym)

  if (is.null(group_cols) || length(group_cols) == 0) {
    temp |>
      dplyr::summarise(!!output_sym := sum(`__energy`, na.rm = TRUE), .groups = "drop")
  } else {
    temp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(!!output_sym := sum(`__energy`, na.rm = TRUE), .groups = "drop")
  }
}

#' Calculate baseline emissions (Equation 2)
#'
#' Implements Equation (2) of AMS-I.B by multiplying the baseline energy content by
#' the appropriate emission factor representing fossil fuel combustion.
#'
#' @param energy_data A tibble, typically the output of [calculate_baseline_energy_content()].
#' @param energy_col Name of the column containing baseline energy in MJ.
#' @param emission_factor Baseline emission factor in tCO2e/MJ. Supply either a single value
#'   applied to all rows or a vector matching the number of rows.
#' @param output_col Name of the resulting column containing baseline emissions in tCO2e.
#' @return A tibble with baseline emissions in tCO2e.
#' @examples
#' baseline_energy <- tibble::tibble(baseline_energy_mj = c(10000, 12000))
#' calculate_baseline_emissions(baseline_energy, emission_factor = 0.00007)
#' @export
calculate_baseline_emissions <- function(energy_data,
                                         energy_col = "baseline_energy_mj",
                                         emission_factor,
                                         output_col = "baseline_emissions_tco2e") {
  energy_tbl <- dplyr::as_tibble(energy_data)
  energy_sym <- if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(energy_col)
  } else {
    rlang::ensym(energy_col)
  }
  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (!rlang::as_string(energy_sym) %in% names(energy_tbl)) {
    stop("`energy_col` must be present in `energy_data`.", call. = FALSE)
  }
  if (!is.numeric(emission_factor)) {
    stop("`emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(energy_tbl)
  if (!length(emission_factor) %in% c(1L, n_rows)) {
    stop("`emission_factor` must be length 1 or match the number of rows in `energy_data`.", call. = FALSE)
  }

  factors <- if (length(emission_factor) == 1L) {
    rep(emission_factor, n_rows)
  } else {
    emission_factor
  }

  energy_tbl |>
    dplyr::mutate(!!output_sym := !!energy_sym * factors)
}

#' Calculate project emissions (Equation 3)
#'
#' Computes project emissions for AMS-I.B where auxiliary fossil fuel use may be
#' present (e.g. back-up diesel engines). The default assumes zero emissions when
#' renewable energy fully drives the mechanical system.
#'
#' @param project_energy A tibble with project energy values in MJ.
#' @param energy_col Name of the column containing project energy in MJ.
#' @param project_emission_factor Project emission factor in tCO2e/MJ (default 0).
#' @param output_col Name of the resulting project emissions column.
#' @return A tibble with project emissions in tCO2e.
#' @examples
#' project_energy <- tibble::tibble(project_energy_mj = c(1000, 1200))
#' calculate_project_emissions(project_energy, energy_col = "project_energy_mj", project_emission_factor = 0)
#' @export
calculate_project_emissions <- function(project_energy,
                                        energy_col = "project_energy_mj",
                                        project_emission_factor = 0,
                                        output_col = "project_emissions_tco2e") {
  project_tbl <- dplyr::as_tibble(project_energy)
  energy_sym <- if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(energy_col)
  } else {
    rlang::ensym(energy_col)
  }
  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (!rlang::as_string(energy_sym) %in% names(project_tbl)) {
    stop("`energy_col` must be present in `project_energy`.", call. = FALSE)
  }
  if (!is.numeric(project_emission_factor)) {
    stop("`project_emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(project_tbl)
  if (!length(project_emission_factor) %in% c(1L, n_rows)) {
    stop("`project_emission_factor` must be length 1 or match the number of rows in `project_energy`.", call. = FALSE)
  }

  factors <- if (length(project_emission_factor) == 1L) {
    rep(project_emission_factor, n_rows)
  } else {
    project_emission_factor
  }

  project_tbl |>
    dplyr::mutate(!!output_sym := !!energy_sym * factors)
}

#' Calculate emission reductions (Equation 4)
#'
#' Implements Equation (4) of AMS-I.B by subtracting project emissions from baseline emissions.
#'
#' @param baseline_emissions Tibble containing baseline emissions, typically the output from
#'   [calculate_baseline_emissions()].
#' @param project_emissions Tibble containing project emissions, typically the output from
#'   [calculate_project_emissions()].
#' @param baseline_col Name of the baseline emission column.
#' @param project_col Name of the project emission column.
#' @param output_col Name of the resulting emission reduction column.
#' @return A tibble containing emission reductions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(system_id = c("pump-1", "pump-2"), baseline_emissions_tco2e = c(120, 150))
#' project <- tibble::tibble(system_id = c("pump-1", "pump-2"), project_emissions_tco2e = c(5, 10))
#' calculate_emission_reductions(baseline, project)
#' @export
calculate_emission_reductions <- function(baseline_emissions,
                                          project_emissions,
                                          baseline_col = "baseline_emissions_tco2e",
                                          project_col = "project_emissions_tco2e",
                                          output_col = "emission_reductions_tco2e") {
  baseline_col <- if (is.character(baseline_col)) {
    if (length(baseline_col) != 1) {
      stop("`baseline_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(baseline_col)
  } else {
    rlang::ensym(baseline_col)
  }
  project_col <- if (is.character(project_col)) {
    if (length(project_col) != 1) {
      stop("`project_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(project_col)
  } else {
    rlang::ensym(project_col)
  }
  output_col <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  baseline_tbl <- dplyr::as_tibble(baseline_emissions)
  project_tbl <- dplyr::as_tibble(project_emissions)

  by_cols <- intersect(names(baseline_tbl), names(project_tbl))
  if (length(by_cols) == 0) {
    stop("`baseline_emissions` and `project_emissions` must share at least one key column.", call. = FALSE)
  }

  merged <- dplyr::full_join(baseline_tbl, project_tbl, by = by_cols)

  if (!rlang::as_string(baseline_col) %in% names(merged)) {
    stop("`baseline_col` must be present after merging inputs.", call. = FALSE)
  }
  if (!rlang::as_string(project_col) %in% names(merged)) {
    stop("`project_col` must be present after merging inputs.", call. = FALSE)
  }

  merged |>
    dplyr::mutate(!!output_col := !!baseline_col - !!project_col)
}
