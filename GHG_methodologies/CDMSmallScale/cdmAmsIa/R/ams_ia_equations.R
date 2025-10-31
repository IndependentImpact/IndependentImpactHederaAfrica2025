
#' Calculate baseline electricity generation (Equation 1)
#'
#' Implements Equation (1) of AMS-I.A by summing electricity production delivered to the user.
#'
#' @param generation_data A tibble with at least the columns specified in `generation_col`
#'   and optional grouping columns identifying systems or users.
#' @param generation_col Name of the column containing electricity generation in kWh.
#' @param group_cols Optional character vector of grouping columns used to aggregate generation.
#' @return A tibble containing grouping columns (if supplied) and a column named
#'   `baseline_generation_kwh` with summed electricity generation in kWh.
#' @examples
#' generation <- tibble::tibble(user_id = c("A", "B"), generation_kwh = c(1200, 1500))
#' calculate_baseline_generation(generation)
#' @seealso [calculate_baseline_emissions()]
#' @export
calculate_baseline_generation <- function(generation_data,
                                          generation_col = "generation_kwh",
                                          group_cols = NULL) {
  if (is.character(generation_col)) {
    if (length(generation_col) != 1) {
      stop("`generation_col` must be a single column name.", call. = FALSE)
    }
    generation_col <- rlang::sym(generation_col)
  } else {
    generation_col <- rlang::ensym(generation_col)
  }
  generation_data <- dplyr::as_tibble(generation_data)
  if (!rlang::as_string(generation_col) %in% names(generation_data)) {
    stop("`generation_col` must be present in `generation_data`.", call. = FALSE)
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    generation_data |>
      dplyr::summarise(baseline_generation_kwh = sum(!!generation_col, na.rm = TRUE), .groups = "drop")
  } else {
    generation_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(baseline_generation_kwh = sum(!!generation_col, na.rm = TRUE), .groups = "drop")
  }
}

#' Calculate baseline emissions (Equation 2)
#'
#' Implements Equation (2) of AMS-I.A, multiplying baseline electricity generation by
#' the applicable grid emission factor.
#'
#' @param baseline_generation A tibble produced by [calculate_baseline_generation()].
#' @param grid_emission_factor Grid emission factor in tCO2e/kWh. Provide either a single value
#'   applied to all rows or a vector matching the number of rows in `baseline_generation`.
#' @param output_col Name of the resulting emission column.
#' @return The input tibble with a new column named `output_col` containing baseline emissions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(baseline_generation_kwh = c(2000, 3000))
#' calculate_baseline_emissions(baseline, grid_emission_factor = 0.8)
#' @seealso [calculate_emission_reductions()]
#' @export
calculate_baseline_emissions <- function(baseline_generation,
                                         grid_emission_factor,
                                         output_col = "baseline_emissions_tco2e") {
  if (!"baseline_generation_kwh" %in% names(baseline_generation)) {
    stop("`baseline_generation` must contain `baseline_generation_kwh`.", call. = FALSE)
  }
  if (!is.numeric(grid_emission_factor)) {
    stop("`grid_emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(baseline_generation)
  if (!length(grid_emission_factor) %in% c(1L, n_rows)) {
    stop("`grid_emission_factor` must be length 1 or match the number of rows in `baseline_generation`.", call. = FALSE)
  }

  output_col <- rlang::ensym(output_col)
  factors <- if (length(grid_emission_factor) == 1L) {
    rep(grid_emission_factor, n_rows)
  } else {
    grid_emission_factor
  }

  dplyr::as_tibble(baseline_generation) |>
    dplyr::mutate(!!output_col := baseline_generation_kwh * factors)
}

#' Calculate project emissions (Equation 3)
#'
#' AMS-I.A assumes negligible project emissions for renewable energy systems. The function retains
#' flexibility by allowing a non-zero factor when auxiliary fossil fuel use is reported.
#'
#' @param baseline_generation Tibble with at least `baseline_generation_kwh` column.
#' @param project_emission_factor Optional project emission factor in tCO2e/kWh (default 0).
#' @param output_col Name of the resulting emission column.
#' @return A tibble with a column named `output_col` containing project emissions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(baseline_generation_kwh = c(2000, 3000))
#' calculate_project_emissions(baseline)
#' @export
calculate_project_emissions <- function(baseline_generation,
                                        project_emission_factor = 0,
                                        output_col = "project_emissions_tco2e") {
  if (!"baseline_generation_kwh" %in% names(baseline_generation)) {
    stop("`baseline_generation` must contain `baseline_generation_kwh`.", call. = FALSE)
  }
  if (!is.numeric(project_emission_factor) || length(project_emission_factor) != 1) {
    stop("`project_emission_factor` must be a single numeric value.", call. = FALSE)
  }

  output_col <- rlang::ensym(output_col)
  dplyr::as_tibble(baseline_generation) |>
    dplyr::mutate(!!output_col := baseline_generation_kwh * project_emission_factor)
}

#' Calculate emission reductions (Equation 4)
#'
#' Implements Equation (4) of AMS-I.A by subtracting project emissions from baseline emissions.
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
#' baseline <- tibble::tibble(system_id = c("A", "B"), baseline_emissions_tco2e = c(100, 120))
#' project <- tibble::tibble(system_id = c("A", "B"), project_emissions_tco2e = c(0, 0))
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

  by_cols <- intersect(names(baseline_emissions), names(project_emissions))
  if (length(by_cols) == 0) {
    stop("`baseline_emissions` and `project_emissions` must share at least one key column.", call. = FALSE)
  }

  merged <- dplyr::full_join(dplyr::as_tibble(baseline_emissions),
                             dplyr::as_tibble(project_emissions),
                             by = by_cols)

  if (!rlang::as_string(baseline_col) %in% names(merged)) {
    stop("`baseline_col` must be present after merging inputs.", call. = FALSE)
  }
  if (!rlang::as_string(project_col) %in% names(merged)) {
    stop("`project_col` must be present after merging inputs.", call. = FALSE)
  }

  merged |>
    dplyr::mutate(!!output_col := !!baseline_col - !!project_col)
}
