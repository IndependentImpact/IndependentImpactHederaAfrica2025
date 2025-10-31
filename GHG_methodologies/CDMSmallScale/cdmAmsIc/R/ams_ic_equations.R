#' Calculate baseline thermal energy delivery (Equation 1)
#'
#' Implements Equation (1) of AMS-I.C by summing the useful thermal energy supplied to
#' end users during the baseline scenario.
#'
#' @param thermal_data A tibble with the column specified in `energy_col` and optional grouping columns.
#' @param energy_col Column name containing useful thermal energy in megawatt-hours thermal (MWhth).
#' @param group_cols Optional character vector of grouping columns used to aggregate thermal energy.
#' @return A tibble with grouping columns (if provided) and a column named `baseline_thermal_output_mwh`.
#' @examples
#' thermal <- tibble::tibble(facility_id = c("A", "B"), thermal_energy_mwh = c(800, 620))
#' calculate_baseline_thermal_output(thermal)
#' @export
calculate_baseline_thermal_output <- function(thermal_data,
                                              energy_col = "thermal_energy_mwh",
                                              group_cols = NULL) {
  if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    energy_sym <- rlang::sym(energy_col)
  } else {
    energy_sym <- rlang::ensym(energy_col)
  }

  data_tbl <- dplyr::as_tibble(thermal_data)
  if (!rlang::as_string(energy_sym) %in% names(data_tbl)) {
    stop("`energy_col` must be present in `thermal_data`.", call. = FALSE)
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    data_tbl |>
      dplyr::summarise(baseline_thermal_output_mwh = sum(!!energy_sym, na.rm = TRUE), .groups = "drop")
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(baseline_thermal_output_mwh = sum(!!energy_sym, na.rm = TRUE), .groups = "drop")
  }
}

#' Calculate baseline emissions (Equation 2)
#'
#' Applies Equation (2) of AMS-I.C by multiplying baseline thermal output by the
#' baseline emission factor representing displaced fossil fuel heat production.
#'
#' @param baseline_output Tibble produced by [calculate_baseline_thermal_output()].
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWhth.
#' @param output_col Name of the resulting emission column.
#' @return The input tibble with a column named `output_col` containing baseline emissions in tCO2e.
#' @examples
#' output <- tibble::tibble(baseline_thermal_output_mwh = c(500, 620))
#' calculate_baseline_emissions(output, baseline_emission_factor = 0.25)
#' @seealso [calculate_emission_reductions()]
#' @export
calculate_baseline_emissions <- function(baseline_output,
                                         baseline_emission_factor,
                                         output_col = "baseline_emissions_tco2e") {
  if (!"baseline_thermal_output_mwh" %in% names(baseline_output)) {
    stop("`baseline_output` must contain `baseline_thermal_output_mwh`.", call. = FALSE)
  }
  if (!is.numeric(baseline_emission_factor)) {
    stop("`baseline_emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(baseline_output)
  if (!length(baseline_emission_factor) %in% c(1L, n_rows)) {
    stop("`baseline_emission_factor` must be length 1 or match the number of rows in `baseline_output`.", call. = FALSE)
  }

  output_sym <- rlang::ensym(output_col)
  factors <- if (length(baseline_emission_factor) == 1L) {
    rep(baseline_emission_factor, n_rows)
  } else {
    baseline_emission_factor
  }

  dplyr::as_tibble(baseline_output) |>
    dplyr::mutate(!!output_sym := baseline_thermal_output_mwh * factors)
}

#' Calculate project emissions (Equation 3)
#'
#' Estimates project emissions associated with auxiliary fossil fuel use or backup boilers.
#'
#' @param baseline_output Tibble containing `baseline_thermal_output_mwh`.
#' @param project_emission_factor Optional project emission factor in tCO2e/MWhth (default 0).
#' @param output_col Name of the resulting emission column.
#' @return Tibble with the project emission column.
#' @examples
#' output <- tibble::tibble(baseline_thermal_output_mwh = c(500, 620))
#' calculate_project_emissions(output, project_emission_factor = 0.02)
#' @export
calculate_project_emissions <- function(baseline_output,
                                        project_emission_factor = 0,
                                        output_col = "project_emissions_tco2e") {
  if (!"baseline_thermal_output_mwh" %in% names(baseline_output)) {
    stop("`baseline_output` must contain `baseline_thermal_output_mwh`.", call. = FALSE)
  }
  if (!is.numeric(project_emission_factor) || length(project_emission_factor) != 1) {
    stop("`project_emission_factor` must be a single numeric value.", call. = FALSE)
  }

  output_sym <- rlang::ensym(output_col)
  dplyr::as_tibble(baseline_output) |>
    dplyr::mutate(!!output_sym := baseline_thermal_output_mwh * project_emission_factor)
}

#' Calculate emission reductions (Equation 4)
#'
#' Implements Equation (4) of AMS-I.C by subtracting project emissions from baseline emissions.
#'
#' @param baseline_emissions Tibble containing baseline emissions, typically output from
#'   [calculate_baseline_emissions()].
#' @param project_emissions Tibble containing project emissions, typically output from
#'   [calculate_project_emissions()].
#' @param baseline_col Name of the baseline emission column.
#' @param project_col Name of the project emission column.
#' @param output_col Name of the resulting emission reduction column.
#' @return Tibble containing emission reductions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(facility_id = c("A", "B"), baseline_emissions_tco2e = c(120, 155))
#' project <- tibble::tibble(facility_id = c("A", "B"), project_emissions_tco2e = c(5, 6))
#' calculate_emission_reductions(baseline, project)
#' @export
calculate_emission_reductions <- function(baseline_emissions,
                                          project_emissions,
                                          baseline_col = "baseline_emissions_tco2e",
                                          project_col = "project_emissions_tco2e",
                                          output_col = "emission_reductions_tco2e") {
  baseline_sym <- if (is.character(baseline_col)) {
    if (length(baseline_col) != 1) {
      stop("`baseline_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(baseline_col)
  } else {
    rlang::ensym(baseline_col)
  }

  project_sym <- if (is.character(project_col)) {
    if (length(project_col) != 1) {
      stop("`project_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(project_col)
  } else {
    rlang::ensym(project_col)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  baseline_tbl <- dplyr::as_tibble(baseline_emissions)
  project_tbl <- dplyr::as_tibble(project_emissions)

  baseline_col_name <- rlang::as_string(baseline_sym)
  project_col_name <- rlang::as_string(project_sym)
  join_cols <- intersect(names(baseline_tbl), names(project_tbl))
  join_cols <- setdiff(join_cols, c(baseline_col_name, project_col_name))

  if (length(join_cols) == 0) {
    stop(
      "`baseline_emissions` and `project_emissions` must share at least one identifier column.",
      call. = FALSE
    )
  }

  dplyr::left_join(
    baseline_tbl,
    project_tbl,
    by = join_cols
  ) |>
    dplyr::mutate(!!output_sym := !!baseline_sym - !!project_sym)
}
