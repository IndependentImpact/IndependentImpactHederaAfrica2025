#' Calculate baseline fossil fuel emissions (Equation 1)
#'
#' Equation (1) of AMS-II.D expresses baseline emissions from fossil fuel use in
#' the absence of the project activity. Emissions are derived from the quantity
#' of fuel that would have been consumed without the efficiency improvement,
#' multiplied by the fuel's net calorific value (NCV), the carbon dioxide
#' emission factor, and adjusted by the efficiency of the baseline equipment.
#' The function accepts tidy input data and returns grouped totals in tonnes of
#' CO2e.
#'
#' @param baseline_data A tibble describing counterfactual fuel consumption.
#' @param fuel_col Column storing the baseline fuel quantity (in mass or volume
#'   units consistent with `ncv_col`).
#' @param ncv_col Column storing the net calorific value (GJ per unit).
#' @param emission_factor_col Column storing the CO2 emission factor (tCO2e per
#'   GJ).
#' @param efficiency_col Column storing the thermal efficiency of the baseline
#'   equipment (fraction between 0 and 1).
#' @param group_cols Optional character vector of grouping variables that should
#'   be retained in the output.
#' @param output_col Name of the resulting emission column. Defaults to
#'   `"baseline_emissions_tco2e"`.
#' @return A tibble with the requested grouping columns and the baseline
#'   emission totals.
#' @examples
#' baseline <- tibble::tibble(unit = c("Kiln", "Dryer"),
#'                            baseline_fuel = c(1200, 900),
#'                            ncv = c(0.038, 0.041),
#'                            ef = c(0.094, 0.094),
#'                            efficiency = c(0.72, 0.68))
#' calculate_baseline_fossil_emissions(baseline,
#'   fuel_col = baseline_fuel,
#'   ncv_col = ncv,
#'   emission_factor_col = ef,
#'   efficiency_col = efficiency,
#'   group_cols = "unit"
#' )
#' @export
calculate_baseline_fossil_emissions <- function(baseline_data,
                                                fuel_col = "baseline_fuel_quantity",
                                                ncv_col = "baseline_ncv_gj_per_unit",
                                                emission_factor_col = "baseline_emission_factor_tco2_per_gj",
                                                efficiency_col = "baseline_efficiency",
                                                group_cols = NULL,
                                                output_col = "baseline_emissions_tco2e") {
  fuel_sym <- rlang::ensym(fuel_col)
  ncv_sym <- rlang::ensym(ncv_col)
  ef_sym <- rlang::ensym(emission_factor_col)
  eff_sym <- rlang::ensym(efficiency_col)
  output_sym <- rlang::ensym(output_col)

  data_tbl <- dplyr::as_tibble(baseline_data)
  required_cols <- c(rlang::as_string(fuel_sym), rlang::as_string(ncv_sym),
                     rlang::as_string(ef_sym), rlang::as_string(eff_sym))
  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in `baseline_data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  calculate_term <- function(tbl) {
    if (any((tbl[[rlang::as_string(eff_sym)]]) <= 0, na.rm = TRUE)) {
      stop("Efficiency values must be positive.", call. = FALSE)
    }
    tbl |>
      dplyr::mutate(
        .emissions = (!!fuel_sym) * (!!ncv_sym) * (!!ef_sym) / (!!eff_sym)
      ) |>
      dplyr::summarise(!!output_sym := sum(.emissions, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    calculate_term(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      calculate_term()
  }
}

#' Calculate project fossil fuel emissions (Equation 2)
#'
#' Equation (2) of AMS-II.D quantifies project emissions from the modernised
#' thermal system. It mirrors the baseline formulation but uses monitored fuel
#' consumption under project conditions. An optional column for indirect
#' electricity emissions can be supplied and will be added to the thermal
#' combustion term.
#'
#' @param project_data A tibble describing monitored project fuel consumption.
#' @param fuel_col Column with project fuel consumption quantities.
#' @param ncv_col Column with project fuel net calorific values (GJ per unit).
#' @param emission_factor_col Column with project emission factors (tCO2e per GJ).
#' @param efficiency_col Column storing project equipment efficiency (fraction
#'   between 0 and 1).
#' @param electricity_col Optional column storing indirect electricity emissions
#'   in tCO2e. If `NULL`, the term is omitted.
#' @param group_cols Optional character vector of grouping variables to retain.
#' @param output_col Name of the resulting emission column. Defaults to
#'   `"project_emissions_tco2e"`.
#' @return A tibble with grouped project emission totals.
#' @examples
#' project <- tibble::tibble(unit = c("Kiln", "Dryer"),
#'                           project_fuel = c(950, 710),
#'                           ncv = c(0.038, 0.041),
#'                           ef = c(0.094, 0.094),
#'                           efficiency = c(0.84, 0.8))
#' calculate_project_fossil_emissions(project,
#'   fuel_col = project_fuel,
#'   ncv_col = ncv,
#'   emission_factor_col = ef,
#'   efficiency_col = efficiency,
#'   group_cols = "unit"
#' )
#' @export
calculate_project_fossil_emissions <- function(project_data,
                                               fuel_col = "project_fuel_quantity",
                                               ncv_col = "project_ncv_gj_per_unit",
                                               emission_factor_col = "project_emission_factor_tco2_per_gj",
                                               efficiency_col = "project_efficiency",
                                               electricity_col = NULL,
                                               group_cols = NULL,
                                               output_col = "project_emissions_tco2e") {
  fuel_sym <- rlang::ensym(fuel_col)
  ncv_sym <- rlang::ensym(ncv_col)
  ef_sym <- rlang::ensym(emission_factor_col)
  eff_sym <- rlang::ensym(efficiency_col)
  output_sym <- rlang::ensym(output_col)
  electricity_sym <- if (!is.null(electricity_col)) rlang::ensym(electricity_col) else NULL

  data_tbl <- dplyr::as_tibble(project_data)
  required_cols <- c(rlang::as_string(fuel_sym), rlang::as_string(ncv_sym),
                     rlang::as_string(ef_sym), rlang::as_string(eff_sym))
  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in `project_data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  calculate_term <- function(tbl) {
    if (any((tbl[[rlang::as_string(eff_sym)]]) <= 0, na.rm = TRUE)) {
      stop("Efficiency values must be positive.", call. = FALSE)
    }
    tbl |>
      dplyr::mutate(
        .emissions = (!!fuel_sym) * (!!ncv_sym) * (!!ef_sym) / (!!eff_sym),
        .electricity = if (!is.null(electricity_sym)) (!!electricity_sym) else 0
      ) |>
      dplyr::summarise(
        !!output_sym := sum(.emissions + .electricity, na.rm = TRUE),
        .groups = "drop"
      )
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    calculate_term(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      calculate_term()
  }
}

#' Calculate leakage emissions (Equation 3)
#'
#' Equation (3) of AMS-II.D accounts for leakage associated with displaced fuels
#' or upstream energy market effects. The function accepts a tibble with any
#' number of leakage components expressed in tCO2e and sums them by the requested
#' grouping columns.
#'
#' @param leakage_data A tibble containing leakage components in tCO2e.
#' @param leakage_col Column storing leakage emission quantities.
#' @param group_cols Optional character vector of grouping variables to retain.
#' @param output_col Name of the resulting column storing leakage totals.
#' @return A tibble with leakage totals.
#' @examples
#' leakage <- tibble::tibble(unit = c("Kiln", "Dryer"),
#'                           leakage_tco2e = c(12.4, 4.3))
#' calculate_leakage_emissions(leakage, group_cols = "unit")
#' @export
calculate_leakage_emissions <- function(leakage_data,
                                        leakage_col = "leakage_emissions_tco2e",
                                        group_cols = NULL,
                                        output_col = "leakage_emissions_tco2e") {
  leakage_sym <- rlang::ensym(leakage_col)
  output_sym <- rlang::ensym(output_col)

  data_tbl <- dplyr::as_tibble(leakage_data)
  if (!rlang::as_string(leakage_sym) %in% names(data_tbl)) {
    stop("`leakage_col` must be present in `leakage_data`.", call. = FALSE)
  }

  summarise_leakage <- function(tbl) {
    tbl |>
      dplyr::summarise(!!output_sym := sum(!!leakage_sym, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_leakage(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_leakage()
  }
}

#' Estimate emission reductions (Equation 4)
#'
#' Equation (4) combines baseline emissions, project emissions, and leakage to
#' produce annual emission reductions for AMS-II.D project activities.
#'
#' @param baseline_emissions Tibble produced by
#'   [calculate_baseline_fossil_emissions()].
#' @param project_emissions Tibble produced by
#'   [calculate_project_fossil_emissions()].
#' @param leakage_emissions Tibble produced by [calculate_leakage_emissions()].
#' @param baseline_col Column storing baseline emissions in the baseline tibble.
#' @param project_col Column storing project emissions in the project tibble.
#' @param leakage_col Column storing leakage emissions in the leakage tibble.
#' @param group_cols Character vector of columns used for joining the three
#'   tables. When `NULL`, a single-row tibble is assumed.
#' @param output_col Name of the resulting emission reduction column.
#' @return A tibble with the union of grouping columns and an emission reduction
#'   column.
#' @examples
#' baseline <- tibble::tibble(unit = "Kiln", baseline_emissions_tco2e = 120)
#' project <- tibble::tibble(unit = "Kiln", project_emissions_tco2e = 72)
#' leakage <- tibble::tibble(unit = "Kiln", leakage_emissions_tco2e = 5)
#' estimate_emission_reductions(baseline, project, leakage, group_cols = "unit")
#' @export
estimate_emission_reductions <- function(baseline_emissions,
                                         project_emissions,
                                         leakage_emissions = NULL,
                                         baseline_col = "baseline_emissions_tco2e",
                                         project_col = "project_emissions_tco2e",
                                         leakage_col = "leakage_emissions_tco2e",
                                         group_cols = NULL,
                                         output_col = "emission_reductions_tco2e") {
  baseline_sym <- rlang::ensym(baseline_col)
  project_sym <- rlang::ensym(project_col)
  leakage_sym <- rlang::ensym(leakage_col)
  output_sym <- rlang::ensym(output_col)

  baseline_tbl <- dplyr::as_tibble(baseline_emissions)
  project_tbl <- dplyr::as_tibble(project_emissions)
  leakage_tbl <- if (!is.null(leakage_emissions)) dplyr::as_tibble(leakage_emissions) else NULL

  joiner <- function(x, y) {
    if (is.null(group_cols)) {
      dplyr::bind_cols(x, y)
    } else {
      dplyr::full_join(x, y, by = group_cols)
    }
  }

  combined <- joiner(baseline_tbl, project_tbl)
  if (!is.null(leakage_tbl)) {
    combined <- joiner(combined, leakage_tbl)
  }

  has_leakage <- !is.null(leakage_emissions) &&
    rlang::as_string(leakage_sym) %in% names(combined)

  result <- combined |>
    dplyr::mutate(
      !!output_sym := (!!baseline_sym) - (!!project_sym) -
        if (has_leakage) dplyr::coalesce(!!leakage_sym, 0) else 0
    )

  if (is.null(group_cols) || length(group_cols) == 0) {
    result |>
      dplyr::select(!!output_sym, dplyr::everything())
  } else {
    result |>
      dplyr::select(dplyr::all_of(group_cols), !!output_sym, dplyr::everything()) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)))
  }
}
