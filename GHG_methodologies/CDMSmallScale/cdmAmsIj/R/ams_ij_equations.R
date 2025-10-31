#' Calculate useful thermal energy delivery (Equation 1)
#'
#' Implements Equation (1) of AMS-I.J by summing the useful thermal energy
#' delivered by the solar water heating system during the monitoring period.
#'
#' @param thermal_data A tibble with the column specified in `energy_col` and
#'   optional grouping columns.
#' @param energy_col Column name containing useful thermal energy in megawatt-hours
#'   thermal (MWhth).
#' @param group_cols Optional character vector of grouping columns used to
#'   aggregate useful thermal energy.
#' @return A tibble with grouping columns (if provided) and a column named
#'   `useful_thermal_output_mwh`.
#' @examples
#' thermal <- tibble::tibble(site_id = c("A", "B"), useful_heat_mwh = c(420, 380))
#' calculate_useful_thermal_output(thermal)
#' @export
calculate_useful_thermal_output <- function(thermal_data,
                                            energy_col = "useful_heat_mwh",
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
      dplyr::summarise(useful_thermal_output_mwh = sum(!!energy_sym, na.rm = TRUE), .groups = "drop")
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(useful_thermal_output_mwh = sum(!!energy_sym, na.rm = TRUE), .groups = "drop")
  }
}

#' Calculate baseline emissions (Equation 2)
#'
#' Applies Equation (2) of AMS-I.J by multiplying useful thermal output by the
#' baseline emission factor representing displaced fossil fuel or electric water
#' heating.
#'
#' @param useful_output Tibble produced by [calculate_useful_thermal_output()].
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWhth.
#' @param output_col Name of the resulting emission column.
#' @return The input tibble with a column named `output_col` containing baseline
#'   emissions in tCO2e.
#' @examples
#' output <- tibble::tibble(useful_thermal_output_mwh = c(500, 620))
#' calculate_baseline_emissions(output, baseline_emission_factor = 0.25)
#' @seealso [calculate_emission_reductions()]
#' @export
calculate_baseline_emissions <- function(useful_output,
                                         baseline_emission_factor,
                                         output_col = "baseline_emissions_tco2e") {
  if (!"useful_thermal_output_mwh" %in% names(useful_output)) {
    stop("`useful_output` must contain `useful_thermal_output_mwh`.", call. = FALSE)
  }
  if (!is.numeric(baseline_emission_factor)) {
    stop("`baseline_emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(useful_output)
  if (!length(baseline_emission_factor) %in% c(1L, n_rows)) {
    stop("`baseline_emission_factor` must be length 1 or match the number of rows in `useful_output`.", call. = FALSE)
  }

  output_sym <- rlang::ensym(output_col)
  factors <- if (length(baseline_emission_factor) == 1L) {
    rep(baseline_emission_factor, n_rows)
  } else {
    baseline_emission_factor
  }

  dplyr::as_tibble(useful_output) |>
    dplyr::mutate(!!output_sym := useful_thermal_output_mwh * factors)
}

#' Calculate project emissions from auxiliary energy (Equation 3)
#'
#' Estimates project emissions associated with auxiliary fossil fuel or electric
#' backup heating that supplements the solar water heating system.
#'
#' @param auxiliary_data Tibble containing auxiliary energy values in MWhth.
#' @param energy_col Column name containing auxiliary energy in MWhth.
#' @param emission_factor Project emission factor in tCO2e/MWhth.
#' @param group_cols Optional character vector of grouping columns used to
#'   aggregate auxiliary energy.
#' @param output_col Name of the resulting emission column.
#' @return Tibble with auxiliary emissions aggregated by the specified grouping
#'   columns.
#' @examples
#' auxiliary <- tibble::tibble(site_id = c("A", "B"), auxiliary_energy_mwh = c(50, 80))
#' calculate_project_emissions_auxiliary(auxiliary, emission_factor = 0.18)
#' @export
calculate_project_emissions_auxiliary <- function(auxiliary_data,
                                                  energy_col = "auxiliary_energy_mwh",
                                                  emission_factor = 0,
                                                  group_cols = NULL,
                                                  output_col = "project_emissions_tco2e") {
  if (!is.numeric(emission_factor) || length(emission_factor) != 1) {
    stop("`emission_factor` must be a single numeric value.", call. = FALSE)
  }
  if (emission_factor < 0) {
    stop("`emission_factor` must be non-negative.", call. = FALSE)
  }

  if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    energy_sym <- rlang::sym(energy_col)
  } else {
    energy_sym <- rlang::ensym(energy_col)
  }

  data_tbl <- dplyr::as_tibble(auxiliary_data)
  if (!rlang::as_string(energy_sym) %in% names(data_tbl)) {
    stop("`energy_col` must be present in `auxiliary_data`.", call. = FALSE)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    data_tbl |>
      dplyr::summarise(!!output_sym := sum(!!energy_sym, na.rm = TRUE) * emission_factor, .groups = "drop")
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(!!output_sym := sum(!!energy_sym, na.rm = TRUE) * emission_factor, .groups = "drop")
  }
}

#' Calculate emission reductions (Equation 4)
#'
#' Implements Equation (4) of AMS-I.J by subtracting project emissions from
#' baseline emissions.
#'
#' @param baseline_emissions Tibble containing baseline emissions, typically
#'   output from [calculate_baseline_emissions()].
#' @param project_emissions Tibble containing project emissions, typically output
#'   from [calculate_project_emissions_auxiliary()].
#' @param baseline_col Name of the baseline emission column.
#' @param project_col Name of the project emission column.
#' @param output_col Name of the resulting emission reduction column.
#' @return Tibble containing emission reductions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(site_id = c("A", "B"), baseline_emissions_tco2e = c(95, 110))
#' project <- tibble::tibble(site_id = c("A", "B"), project_emissions_tco2e = c(5, 8))
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
