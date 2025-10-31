#' Calculate baseline building emissions (Equation 1)
#'
#' Equation (1) of AMS-II.E consolidates baseline greenhouse gas emissions from
#' thermal fuels and electricity consumption in buildings. This function accepts
#' tidy baseline data and multiplies each energy stream by its emission factor,
#' returning grouped totals in tonnes of CO2e.
#'
#' @param baseline_data A tibble describing counterfactual building energy use.
#' @param thermal_energy_col Column storing baseline thermal energy demand (GJ).
#'   Set to `NULL` when thermal fuels are not part of the baseline scenario.
#' @param thermal_emission_factor_col Column storing thermal emission factors
#'   (tCO2e per GJ). Required when `thermal_energy_col` is supplied.
#' @param electricity_col Column storing baseline electricity consumption (MWh).
#'   Set to `NULL` to omit electricity emissions.
#' @param electricity_emission_factor_col Column storing electricity emission
#'   factors (tCO2e per MWh). Required when `electricity_col` is supplied.
#' @param group_cols Optional character vector of grouping variables to retain in
#'   the output.
#' @param output_col Name of the resulting emission column. Defaults to
#'   `"baseline_emissions_tco2e"`.
#' @return A tibble with grouped baseline emissions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(
#'   building_id = c("Office_A", "Office_B"),
#'   baseline_thermal_energy_gj = c(420, 380),
#'   baseline_thermal_emission_factor_tco2_per_gj = c(0.056, 0.052),
#'   baseline_electricity_mwh = c(120, 95),
#'   baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.62)
#' )
#' calculate_baseline_building_emissions(baseline, group_cols = "building_id")
#' @export
calculate_baseline_building_emissions <- function(baseline_data,
                                                  thermal_energy_col = "baseline_thermal_energy_gj",
                                                  thermal_emission_factor_col = "baseline_thermal_emission_factor_tco2_per_gj",
                                                  electricity_col = "baseline_electricity_mwh",
                                                  electricity_emission_factor_col = "baseline_electricity_emission_factor_tco2_per_mwh",
                                                  group_cols = NULL,
                                                  output_col = "baseline_emissions_tco2e") {
  data_tbl <- dplyr::as_tibble(baseline_data)

  thermal_syms <- if (!is.null(thermal_energy_col) && !is.null(thermal_emission_factor_col)) {
    energy_sym <- rlang::ensym(thermal_energy_col)
    factor_sym <- rlang::ensym(thermal_emission_factor_col)
    missing_cols <- setdiff(c(rlang::as_string(energy_sym), rlang::as_string(factor_sym)), names(data_tbl))
    if (length(missing_cols) > 0) {
      stop("Specified thermal columns must exist in the data frame: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    list(energy = energy_sym, factor = factor_sym)
  } else {
    NULL
  }

  electricity_syms <- if (!is.null(electricity_col) && !is.null(electricity_emission_factor_col)) {
    energy_sym <- rlang::ensym(electricity_col)
    factor_sym <- rlang::ensym(electricity_emission_factor_col)
    missing_cols <- setdiff(c(rlang::as_string(energy_sym), rlang::as_string(factor_sym)), names(data_tbl))
    if (length(missing_cols) > 0) {
      stop("Specified electricity columns must exist in the data frame: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    list(energy = energy_sym, factor = factor_sym)
  } else {
    NULL
  }

  summarise_group <- function(tbl) {
    output_sym <- rlang::ensym(output_col)

    tbl |>
      dplyr::mutate(
        .thermal = if (!is.null(thermal_syms)) {
          dplyr::coalesce(!!thermal_syms$energy, 0) * dplyr::coalesce(!!thermal_syms$factor, 0)
        } else {
          0
        },
        .electricity = if (!is.null(electricity_syms)) {
          dplyr::coalesce(!!electricity_syms$energy, 0) * dplyr::coalesce(!!electricity_syms$factor, 0)
        } else {
          0
        }
      ) |>
      dplyr::summarise(!!output_sym := sum(.thermal + .electricity, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_group()
  }
}

#' Calculate project building emissions (Equation 2)
#'
#' Equation (2) of AMS-II.E quantifies project emissions from residual thermal
#' fuel use and electricity demand after energy efficiency or fuel switching
#' measures are implemented. Inputs mirror
#' [calculate_baseline_building_emissions()] to streamline workflows.
#'
#' @param project_data A tibble describing monitored project energy use.
#' @param thermal_energy_col Column storing project thermal energy demand (GJ).
#'   Set to `NULL` to omit thermal emissions.
#' @param thermal_emission_factor_col Column storing project thermal emission
#'   factors (tCO2e per GJ).
#' @param electricity_col Column storing project electricity consumption (MWh).
#' @param electricity_emission_factor_col Column storing project electricity
#'   emission factors (tCO2e per MWh).
#' @param group_cols Optional character vector of grouping variables to retain.
#' @param output_col Name of the resulting emission column. Defaults to
#'   `"project_emissions_tco2e"`.
#' @return A tibble with grouped project emissions in tCO2e.
#' @examples
#' project <- tibble::tibble(
#'   building_id = c("Office_A", "Office_B"),
#'   project_thermal_energy_gj = c(210, 195),
#'   project_thermal_emission_factor_tco2_per_gj = c(0.034, 0.031),
#'   project_electricity_mwh = c(86, 74),
#'   project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.62)
#' )
#' calculate_project_building_emissions(project, group_cols = "building_id")
#' @export
calculate_project_building_emissions <- function(project_data,
                                                 thermal_energy_col = "project_thermal_energy_gj",
                                                 thermal_emission_factor_col = "project_thermal_emission_factor_tco2_per_gj",
                                                 electricity_col = "project_electricity_mwh",
                                                 electricity_emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
                                                 group_cols = NULL,
                                                 output_col = "project_emissions_tco2e") {
  data_tbl <- dplyr::as_tibble(project_data)

  thermal_syms <- if (!is.null(thermal_energy_col) && !is.null(thermal_emission_factor_col)) {
    energy_sym <- rlang::ensym(thermal_energy_col)
    factor_sym <- rlang::ensym(thermal_emission_factor_col)
    missing_cols <- setdiff(c(rlang::as_string(energy_sym), rlang::as_string(factor_sym)), names(data_tbl))
    if (length(missing_cols) > 0) {
      stop("Specified thermal columns must exist in the data frame: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    list(energy = energy_sym, factor = factor_sym)
  } else {
    NULL
  }

  electricity_syms <- if (!is.null(electricity_col) && !is.null(electricity_emission_factor_col)) {
    energy_sym <- rlang::ensym(electricity_col)
    factor_sym <- rlang::ensym(electricity_emission_factor_col)
    missing_cols <- setdiff(c(rlang::as_string(energy_sym), rlang::as_string(factor_sym)), names(data_tbl))
    if (length(missing_cols) > 0) {
      stop("Specified electricity columns must exist in the data frame: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    list(energy = energy_sym, factor = factor_sym)
  } else {
    NULL
  }

  summarise_group <- function(tbl) {
    output_sym <- rlang::ensym(output_col)

    tbl |>
      dplyr::mutate(
        .thermal = if (!is.null(thermal_syms)) {
          dplyr::coalesce(!!thermal_syms$energy, 0) * dplyr::coalesce(!!thermal_syms$factor, 0)
        } else {
          0
        },
        .electricity = if (!is.null(electricity_syms)) {
          dplyr::coalesce(!!electricity_syms$energy, 0) * dplyr::coalesce(!!electricity_syms$factor, 0)
        } else {
          0
        }
      ) |>
      dplyr::summarise(!!output_sym := sum(.thermal + .electricity, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_group()
  }
}

#' Calculate leakage emissions for AMS-II.E (Equation 3)
#'
#' Equation (3) of AMS-II.E accounts for leakage linked to upstream fuel supply,
#' equipment disposal, or other market effects. The helper sums leakage
#' components grouped by the requested identifiers.
#'
#' @param leakage_data A tibble containing leakage components expressed in tCO2e.
#' @param leakage_col Column storing leakage emission quantities.
#' @param group_cols Optional character vector of grouping variables to retain.
#' @param output_col Name of the resulting leakage column.
#' @return A tibble with leakage totals.
#' @examples
#' leakage <- tibble::tibble(building_id = c("Office_A", "Office_B"),
#'                           leakage_emissions_tco2e = c(2.1, 1.5))
#' calculate_leakage_emissions(leakage, group_cols = "building_id")
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
      dplyr::summarise(!!output_sym := sum(dplyr::coalesce(!!leakage_sym, 0), na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_leakage(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_leakage()
  }
}

#' Estimate AMS-II.E emission reductions (Equation 4)
#'
#' Equation (4) combines baseline emissions, project emissions, and leakage to
#' derive annual emission reductions for AMS-II.E building activities.
#'
#' @param baseline_emissions Tibble produced by
#'   [calculate_baseline_building_emissions()].
#' @param project_emissions Tibble produced by
#'   [calculate_project_building_emissions()].
#' @param leakage_emissions Optional tibble produced by
#'   [calculate_leakage_emissions()].
#' @param baseline_col Column storing baseline emissions in the baseline tibble.
#' @param project_col Column storing project emissions in the project tibble.
#' @param leakage_col Column storing leakage emissions in the leakage tibble.
#' @param group_cols Character vector of columns used for joining the tables.
#'   When `NULL`, a single-row tibble is assumed.
#' @param output_col Name of the resulting emission reduction column.
#' @return A tibble with the union of grouping columns and an emission reduction
#'   column.
#' @examples
#' baseline <- tibble::tibble(building_id = "Office_A", baseline_emissions_tco2e = 120)
#' project <- tibble::tibble(building_id = "Office_A", project_emissions_tco2e = 72)
#' leakage <- tibble::tibble(building_id = "Office_A", leakage_emissions_tco2e = 2.5)
#' estimate_emission_reductions(baseline, project, leakage, group_cols = "building_id")
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
    if (is.null(group_cols) || length(group_cols) == 0) {
      dplyr::bind_cols(x, y)
    } else {
      dplyr::full_join(x, y, by = group_cols)
    }
  }

  combined <- joiner(baseline_tbl, project_tbl)
  if (!is.null(leakage_tbl)) {
    combined <- joiner(combined, leakage_tbl)
  }

  has_leakage <- !is.null(leakage_emissions) && rlang::as_string(leakage_sym) %in% names(combined)

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
