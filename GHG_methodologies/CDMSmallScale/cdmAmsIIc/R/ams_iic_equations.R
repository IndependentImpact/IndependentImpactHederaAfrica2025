#' Calculate baseline energy consumption (Equation 1)
#'
#' Implements Equation (1) of AMS-II.C by aggregating the baseline energy
#' consumption for the equipment or process being retrofitted. The function
#' accepts tidy input data and returns grouped totals expressed in megawatt-hours
#' (MWh).
#'
#' @param baseline_data A tibble containing baseline energy consumption values.
#' @param energy_col Column name holding baseline energy use in MWh.
#' @param group_cols Optional character vector of grouping columns to preserve in
#'   the output.
#' @param output_col Name of the resulting column storing baseline energy in MWh.
#' @return A tibble with grouping columns (if provided) and a column named
#'   `output_col`.
#' @examples
#' baseline <- tibble::tibble(site = c("A", "A", "B"),
#'                            appliance = c("Lighting", "HVAC", "Lighting"),
#'                            baseline_energy_mwh = c(12, 34, 28))
#' calculate_baseline_energy_consumption(baseline, group_cols = "site")
#' @export
calculate_baseline_energy_consumption <- function(baseline_data,
                                                  energy_col = "baseline_energy_mwh",
                                                  group_cols = NULL,
                                                  output_col = "baseline_energy_mwh") {
  if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    energy_sym <- rlang::sym(energy_col)
  } else {
    energy_sym <- rlang::ensym(energy_col)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  data_tbl <- dplyr::as_tibble(baseline_data)
  if (!rlang::as_string(energy_sym) %in% names(data_tbl)) {
    stop("`energy_col` must be present in `baseline_data`.", call. = FALSE)
  }

  summarise_energy <- function(tbl) {
    tbl |>
      dplyr::summarise(!!output_sym := sum(!!energy_sym, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_energy(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_energy()
  }
}

#' Calculate project energy consumption (Equation 2)
#'
#' Implements Equation (2) of AMS-II.C by aggregating the monitored project
#' energy consumption for the efficient technology. Inputs mirror
#' [calculate_baseline_energy_consumption()] to ease piping workflows.
#'
#' @param project_data A tibble containing project energy use in MWh.
#' @param energy_col Column name holding project energy values.
#' @param group_cols Optional grouping columns to retain in the output.
#' @param output_col Name of the resulting column storing project energy in MWh.
#' @return A tibble with grouped project energy totals.
#' @examples
#' project <- tibble::tibble(site = c("A", "A", "B"),
#'                           project_energy_mwh = c(8, 16, 18))
#' calculate_project_energy_consumption(project, group_cols = "site")
#' @export
calculate_project_energy_consumption <- function(project_data,
                                                 energy_col = "project_energy_mwh",
                                                 group_cols = NULL,
                                                 output_col = "project_energy_mwh") {
  if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    energy_sym <- rlang::sym(energy_col)
  } else {
    energy_sym <- rlang::ensym(energy_col)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  data_tbl <- dplyr::as_tibble(project_data)
  if (!rlang::as_string(energy_sym) %in% names(data_tbl)) {
    stop("`energy_col` must be present in `project_data`.", call. = FALSE)
  }

  summarise_energy <- function(tbl) {
    tbl |>
      dplyr::summarise(!!output_sym := sum(!!energy_sym, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_energy(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_energy()
  }
}

#' Calculate energy savings (Equation 3)
#'
#' Implements Equation (3) of AMS-II.C by subtracting project energy consumption
#' from baseline consumption. The function joins baseline and project data on the
#' specified grouping columns.
#'
#' @param baseline_energy Tibble created by
#'   [calculate_baseline_energy_consumption()].
#' @param project_energy Tibble created by
#'   [calculate_project_energy_consumption()].
#' @param baseline_col Column name storing baseline energy in `baseline_energy`.
#' @param project_col Column name storing project energy in `project_energy`.
#' @param output_col Name of the resulting column storing energy savings in MWh.
#' @return A tibble with the union of grouping columns and an energy savings
#'   column.
#' @examples
#' baseline <- tibble::tibble(site = c("A", "B"), baseline_energy_mwh = c(46, 28))
#' project <- tibble::tibble(site = c("A", "B"), project_energy_mwh = c(24, 18))
#' calculate_energy_savings(baseline, project)
#' @export
calculate_energy_savings <- function(baseline_energy,
                                     project_energy,
                                     baseline_col = "baseline_energy_mwh",
                                     project_col = "project_energy_mwh",
                                     output_col = "energy_savings_mwh") {
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

  baseline_tbl <- dplyr::as_tibble(baseline_energy)
  project_tbl <- dplyr::as_tibble(project_energy)

  if (!rlang::as_string(baseline_sym) %in% names(baseline_tbl)) {
    stop("`baseline_energy` must contain the column specified by `baseline_col`.", call. = FALSE)
  }
  if (!rlang::as_string(project_sym) %in% names(project_tbl)) {
    stop("`project_energy` must contain the column specified by `project_col`.", call. = FALSE)
  }

  common_cols <- intersect(names(baseline_tbl), names(project_tbl))
  common_cols <- setdiff(common_cols, c(rlang::as_string(baseline_sym), rlang::as_string(project_sym)))

  if (length(common_cols) == 0) {
    baseline_tbl$..join_id <- seq_len(nrow(baseline_tbl))
    project_tbl$..join_id <- seq_len(nrow(project_tbl))
    joined <- dplyr::full_join(baseline_tbl, project_tbl, by = "..join_id") |>
      dplyr::select(-"..join_id")
  } else {
    joined <- dplyr::full_join(baseline_tbl, project_tbl, by = common_cols)
  }

  joined |>
    dplyr::mutate(
      !!output_sym := (!!baseline_sym) - (!!project_sym)
    )
}

#' Calculate emission reductions (Equation 4)
#'
#' Implements Equation (4) of AMS-II.C by multiplying energy savings by the
#' relevant emission factor for the displaced energy carrier.
#'
#' @param energy_savings Tibble produced by [calculate_energy_savings()].
#' @param savings_col Column containing energy savings in MWh.
#' @param emission_factor Emission factor in tonnes CO2e per MWh.
#' @param output_col Name of the resulting emission reduction column.
#' @return A tibble with emission reductions in tCO2e.
#' @examples
#' savings <- tibble::tibble(site = c("A", "B"), energy_savings_mwh = c(22, 10))
#' calculate_emission_reductions(savings, emission_factor = 0.65)
#' @export
calculate_emission_reductions <- function(energy_savings,
                                          savings_col = "energy_savings_mwh",
                                          emission_factor,
                                          output_col = "emission_reductions_tco2e") {
  savings_sym <- if (is.character(savings_col)) {
    if (length(savings_col) != 1) {
      stop("`savings_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(savings_col)
  } else {
    rlang::ensym(savings_col)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (!is.numeric(emission_factor) || length(emission_factor) == 0) {
    stop("`emission_factor` must be numeric.", call. = FALSE)
  }

  savings_tbl <- dplyr::as_tibble(energy_savings)
  if (!rlang::as_string(savings_sym) %in% names(savings_tbl)) {
    stop("`energy_savings` must contain the column specified by `savings_col`.", call. = FALSE)
  }

  n_rows <- nrow(savings_tbl)
  if (!length(emission_factor) %in% c(1L, n_rows)) {
    stop("`emission_factor` must be length 1 or match the number of rows in `energy_savings`.", call. = FALSE)
  }

  factors <- if (length(emission_factor) == 1L) {
    rep(emission_factor, n_rows)
  } else {
    emission_factor
  }

  savings_tbl |>
    dplyr::mutate(!!output_sym := (!!savings_sym) * factors)
}
