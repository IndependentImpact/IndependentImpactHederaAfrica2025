#' Calculate baseline electricity supplied to the mini-grid (Equation 1)
#'
#' Implements Equation (1) of AMS-I.D by summing the electricity delivered to
#' the captive mini-grid or isolated distribution network during the baseline
#' scenario.
#'
#' @param supply_data A tibble with the column specified in `electricity_col`
#'   and optional grouping columns.
#' @param electricity_col Column name containing electricity supplied in
#'   megawatt-hours (MWh).
#' @param group_cols Optional character vector of grouping columns used to
#'   aggregate electricity supply.
#' @return A tibble with grouping columns (if provided) and a column named
#'   `baseline_electricity_mwh`.
#' @examples
#' supply <- tibble::tibble(grid_id = c("A", "B"), electricity_mwh = c(900, 750))
#' calculate_baseline_electricity_supply(supply)
#' @export
calculate_baseline_electricity_supply <- function(supply_data,
                                                  electricity_col = "electricity_mwh",
                                                  group_cols = NULL) {
  electricity_sym <- if (is.character(electricity_col)) {
    if (length(electricity_col) != 1) {
      stop("`electricity_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(electricity_col)
  } else {
    rlang::ensym(electricity_col)
  }

  data_tbl <- dplyr::as_tibble(supply_data)
  if (!rlang::as_string(electricity_sym) %in% names(data_tbl)) {
    stop("`electricity_col` must be present in `supply_data`.", call. = FALSE)
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    data_tbl |>
      dplyr::summarise(baseline_electricity_mwh = sum(!!electricity_sym, na.rm = TRUE), .groups = "drop")
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(baseline_electricity_mwh = sum(!!electricity_sym, na.rm = TRUE), .groups = "drop")
  }
}

#' Calculate baseline emissions (Equation 2)
#'
#' Applies Equation (2) of AMS-I.D by multiplying baseline electricity supply by
#' the baseline emission factor representing displaced fossil-based electricity
#' generation.
#'
#' @param baseline_supply Tibble produced by [calculate_baseline_electricity_supply()].
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWh.
#' @param output_col Name of the resulting emission column.
#' @return The input tibble with a column named `output_col` containing baseline
#'   emissions in tCO2e.
#' @examples
#' supply <- tibble::tibble(baseline_electricity_mwh = c(400, 520))
#' calculate_baseline_emissions(supply, baseline_emission_factor = 0.72)
#' @seealso [calculate_emission_reductions()]
#' @export
calculate_baseline_emissions <- function(baseline_supply,
                                         baseline_emission_factor,
                                         output_col = "baseline_emissions_tco2e") {
  if (!"baseline_electricity_mwh" %in% names(baseline_supply)) {
    stop("`baseline_supply` must contain `baseline_electricity_mwh`.", call. = FALSE)
  }
  if (!is.numeric(baseline_emission_factor)) {
    stop("`baseline_emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(baseline_supply)
  if (!length(baseline_emission_factor) %in% c(1L, n_rows)) {
    stop("`baseline_emission_factor` must be length 1 or match the number of rows in `baseline_supply`.", call. = FALSE)
  }

  output_sym <- rlang::ensym(output_col)
  factors <- if (length(baseline_emission_factor) == 1L) {
    rep(baseline_emission_factor, n_rows)
  } else {
    baseline_emission_factor
  }

  dplyr::as_tibble(baseline_supply) |>
    dplyr::mutate(!!output_sym := baseline_electricity_mwh * factors)
}

#' Calculate project emissions (Equation 3)
#'
#' Estimates project emissions associated with auxiliary fossil fuel use or
#' backup generators in the renewable mini-grid.
#'
#' @param baseline_supply Tibble containing `baseline_electricity_mwh`.
#' @param project_emission_factor Optional project emission factor in tCO2e/MWh (default 0).
#' @param output_col Name of the resulting emission column.
#' @return Tibble with the project emission column.
#' @examples
#' supply <- tibble::tibble(baseline_electricity_mwh = c(400, 520))
#' calculate_project_emissions(supply, project_emission_factor = 0.05)
#' @export
calculate_project_emissions <- function(baseline_supply,
                                        project_emission_factor = 0,
                                        output_col = "project_emissions_tco2e") {
  if (!"baseline_electricity_mwh" %in% names(baseline_supply)) {
    stop("`baseline_supply` must contain `baseline_electricity_mwh`.", call. = FALSE)
  }
  if (!is.numeric(project_emission_factor) || length(project_emission_factor) != 1) {
    stop("`project_emission_factor` must be a single numeric value.", call. = FALSE)
  }

  output_sym <- rlang::ensym(output_col)
  dplyr::as_tibble(baseline_supply) |>
    dplyr::mutate(!!output_sym := baseline_electricity_mwh * project_emission_factor)
}

#' Calculate emission reductions (Equation 4)
#'
#' Implements Equation (4) of AMS-I.D by subtracting project emissions from
#' baseline emissions.
#'
#' @param baseline_emissions Tibble containing baseline emissions, typically
#'   output from [calculate_baseline_emissions()].
#' @param project_emissions Tibble containing project emissions, typically
#'   output from [calculate_project_emissions()].
#' @param baseline_col Name of the baseline emission column.
#' @param project_col Name of the project emission column.
#' @param output_col Name of the resulting emission reduction column.
#' @return Tibble containing emission reductions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(grid_id = c("A", "B"), baseline_emissions_tco2e = c(280, 360))
#' project <- tibble::tibble(grid_id = c("A", "B"), project_emissions_tco2e = c(10, 12))
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
