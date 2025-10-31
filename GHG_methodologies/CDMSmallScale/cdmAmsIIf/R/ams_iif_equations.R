#' Calculate baseline agricultural emissions (Equation 1)
#'
#' Implements Equation (1) of AMS-II.F by converting baseline fossil fuel and
#' electricity consumption for an agricultural processing facility into
#' greenhouse gas emissions. The helper multiplies each energy stream by the
#' corresponding emission factor and aggregates the results using optional
#' grouping columns.
#'
#' @param baseline_data Tidy tibble describing baseline energy use for the
#'   facility or process line.
#' @param fuel_energy_col Column containing baseline fossil fuel energy in
#'   gigajoules (GJ). Set to `NULL` when fossil fuels are absent.
#' @param fuel_emission_factor_col Column storing baseline fossil fuel emission
#'   factors in tCO2e per GJ. Required when `fuel_energy_col` is provided.
#' @param electricity_col Column storing baseline electricity consumption in
#'   megawatt-hours (MWh). Set to `NULL` when electricity is not displaced.
#' @param electricity_emission_factor_col Column storing baseline electricity
#'   emission factors in tCO2e per MWh. Required when `electricity_col` is
#'   provided.
#' @param group_cols Optional character vector of grouping columns to retain in
#'   the output.
#' @param output_col Name of the column that will contain baseline emissions in
#'   tonnes of CO2e. Defaults to `"baseline_emissions_tco2e"`.
#' @return A tibble containing baseline emissions aggregated by the requested
#'   grouping structure.
#' @examples
#' baseline <- tibble::tibble(
#'   facility_id = c("rice_mill_1", "rice_mill_2"),
#'   baseline_fuel_energy_gj = c(980, 1120),
#'   baseline_fuel_emission_factor_tco2_per_gj = c(0.074, 0.071),
#'   baseline_electricity_mwh = c(180, 205),
#'   baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.58)
#' )
#' calculate_baseline_agricultural_emissions(baseline, group_cols = "facility_id")
#' @export
calculate_baseline_agricultural_emissions <- function(baseline_data,
                                                      fuel_energy_col = "baseline_fuel_energy_gj",
                                                      fuel_emission_factor_col = "baseline_fuel_emission_factor_tco2_per_gj",
                                                      electricity_col = "baseline_electricity_mwh",
                                                      electricity_emission_factor_col = "baseline_electricity_emission_factor_tco2_per_mwh",
                                                      group_cols = NULL,
                                                      output_col = "baseline_emissions_tco2e") {
  data_tbl <- dplyr::as_tibble(baseline_data)

  fuel_syms <- if (!is.null(fuel_energy_col) && !is.null(fuel_emission_factor_col)) {
    energy_sym <- rlang::ensym(fuel_energy_col)
    factor_sym <- rlang::ensym(fuel_emission_factor_col)
    missing_cols <- setdiff(c(rlang::as_string(energy_sym), rlang::as_string(factor_sym)), names(data_tbl))
    if (length(missing_cols) > 0) {
      stop("Fuel energy and emission factor columns must exist in `baseline_data`.", call. = FALSE)
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
      stop("Electricity columns must exist in `baseline_data` when provided.", call. = FALSE)
    }
    list(energy = energy_sym, factor = factor_sym)
  } else {
    NULL
  }

  summarise_group <- function(tbl) {
    output_sym <- if (is.character(output_col)) {
      if (length(output_col) != 1) {
        stop("`output_col` must be a single column name.", call. = FALSE)
      }
      rlang::sym(output_col)
    } else {
      rlang::ensym(output_col)
    }

    tbl |>
      dplyr::mutate(
        .fuel_emissions = if (!is.null(fuel_syms)) {
          dplyr::coalesce(!!fuel_syms$energy, 0) * dplyr::coalesce(!!fuel_syms$factor, 0)
        } else {
          0
        },
        .electricity_emissions = if (!is.null(electricity_syms)) {
          dplyr::coalesce(!!electricity_syms$energy, 0) * dplyr::coalesce(!!electricity_syms$factor, 0)
        } else {
          0
        }
      ) |>
      dplyr::summarise(!!output_sym := sum(.fuel_emissions + .electricity_emissions, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_group()
  }
}

#' Calculate project agricultural emissions (Equation 2)
#'
#' Implements Equation (2) of AMS-II.F by converting monitored project fuel and
#' electricity consumption into emissions. Inputs mirror
#' [calculate_baseline_agricultural_emissions()] to streamline workflows.
#'
#' @param project_data Tidy tibble describing project energy use for the
#'   agricultural facility.
#' @param fuel_energy_col Column containing project fuel energy in GJ.
#' @param fuel_emission_factor_col Column storing project fuel emission factors
#'   in tCO2e per GJ.
#' @param electricity_col Column containing project electricity consumption in
#'   MWh.
#' @param electricity_emission_factor_col Column storing project electricity
#'   emission factors in tCO2e per MWh.
#' @param group_cols Optional grouping columns for the output.
#' @param output_col Name of the column that will contain project emissions in
#'   tCO2e. Defaults to `"project_emissions_tco2e"`.
#' @return A tibble containing project emissions aggregated by the requested
#'   grouping structure.
#' @examples
#' project <- tibble::tibble(
#'   facility_id = c("rice_mill_1", "rice_mill_2"),
#'   project_fuel_energy_gj = c(520, 640),
#'   project_fuel_emission_factor_tco2_per_gj = c(0.028, 0.031),
#'   project_electricity_mwh = c(145, 163),
#'   project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.58)
#' )
#' calculate_project_agricultural_emissions(project, group_cols = "facility_id")
#' @export
calculate_project_agricultural_emissions <- function(project_data,
                                                     fuel_energy_col = "project_fuel_energy_gj",
                                                     fuel_emission_factor_col = "project_fuel_emission_factor_tco2_per_gj",
                                                     electricity_col = "project_electricity_mwh",
                                                     electricity_emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
                                                     group_cols = NULL,
                                                     output_col = "project_emissions_tco2e") {
  calculate_baseline_agricultural_emissions(
    baseline_data = project_data,
    fuel_energy_col = fuel_energy_col,
    fuel_emission_factor_col = fuel_emission_factor_col,
    electricity_col = electricity_col,
    electricity_emission_factor_col = electricity_emission_factor_col,
    group_cols = group_cols,
    output_col = output_col
  )
}

#' Calculate leakage emissions for AMS-II.F (Equation 3)
#'
#' Sums leakage emissions associated with fossil fuel transport, displaced
#' biomass usage, or other indirect effects as outlined in AMS-II.F. When the
#' supplied tibble already reflects aggregated leakage for each grouping
#' structure, the helper simply renames the column to ensure compatibility with
#' [calculate_emission_reductions_iif()].
#'
#' @param leakage_data Optional tibble describing leakage emissions.
#' @param leakage_col Column containing leakage values in tCO2e.
#' @param group_cols Optional grouping columns matching the baseline/project
#'   datasets.
#' @param output_col Name of the column that will contain leakage totals.
#' @return A tibble with leakage emissions aggregated by the requested grouping
#'   structure. Returns `NULL` when `leakage_data` is `NULL`.
#' @examples
#' leakage <- tibble::tibble(facility_id = "rice_mill_1", leakage_emissions_tco2e = 4.2)
#' calculate_leakage_emissions_iif(leakage, group_cols = "facility_id")
#' @export
calculate_leakage_emissions_iif <- function(leakage_data,
                                            leakage_col = "leakage_emissions_tco2e",
                                            group_cols = NULL,
                                            output_col = "leakage_emissions_tco2e") {
  if (is.null(leakage_data)) {
    return(NULL)
  }

  data_tbl <- dplyr::as_tibble(leakage_data)
  leakage_sym <- rlang::ensym(leakage_col)
  output_sym <- rlang::ensym(output_col)

  if (!rlang::as_string(leakage_sym) %in% names(data_tbl)) {
    stop("`leakage_col` must exist in `leakage_data`.", call. = FALSE)
  }

  summarise_group <- function(tbl) {
    tbl |>
      dplyr::summarise(!!output_sym := sum(!!leakage_sym, na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_group()
  }
}

#' Calculate emission reductions for AMS-II.F (Equation 4)
#'
#' Implements Equation (4) of AMS-II.F by subtracting project and leakage
#' emissions from baseline emissions to estimate net emission reductions.
#'
#' @param baseline_emissions Tibble containing baseline emissions, typically the
#'   output of [calculate_baseline_agricultural_emissions()].
#' @param project_emissions Tibble containing project emissions, typically the
#'   output of [calculate_project_agricultural_emissions()].
#' @param leakage_emissions Optional tibble containing leakage emissions from
#'   [calculate_leakage_emissions_iif()].
#' @param baseline_col Column storing baseline emissions (tCO2e).
#' @param project_col Column storing project emissions (tCO2e).
#' @param leakage_col Column storing leakage emissions (tCO2e).
#' @param output_col Column storing emission reductions (tCO2e).
#' @param group_cols Optional character vector of grouping columns shared across
#'   inputs. When `NULL`, no join is attempted.
#' @return A tibble with emission reductions aggregated by the requested
#'   grouping structure.
#' @examples
#' baseline <- tibble::tibble(facility_id = "rice_mill_1", baseline_emissions_tco2e = 420)
#' project <- tibble::tibble(facility_id = "rice_mill_1", project_emissions_tco2e = 180)
#' leakage <- tibble::tibble(facility_id = "rice_mill_1", leakage_emissions_tco2e = 6)
#' calculate_emission_reductions_iif(baseline, project, leakage, group_cols = "facility_id")
#' @export
calculate_emission_reductions_iif <- function(baseline_emissions,
                                              project_emissions,
                                              leakage_emissions = NULL,
                                              baseline_col = "baseline_emissions_tco2e",
                                              project_col = "project_emissions_tco2e",
                                              leakage_col = "leakage_emissions_tco2e",
                                              output_col = "emission_reductions_tco2e",
                                              group_cols = NULL) {
  baseline_tbl <- dplyr::as_tibble(baseline_emissions)
  project_tbl <- dplyr::as_tibble(project_emissions)

  baseline_sym <- rlang::ensym(baseline_col)
  project_sym <- rlang::ensym(project_col)
  output_sym <- rlang::ensym(output_col)

  for (col in c(rlang::as_string(baseline_sym))) {
    if (!col %in% names(baseline_tbl)) {
      stop("Baseline emissions tibble must contain the specified column.", call. = FALSE)
    }
  }
  for (col in c(rlang::as_string(project_sym))) {
    if (!col %in% names(project_tbl)) {
      stop("Project emissions tibble must contain the specified column.", call. = FALSE)
    }
  }

  join_by <- if (is.null(group_cols) || length(group_cols) == 0) {
    character(0)
  } else {
    group_cols
  }

  joined <- if (length(join_by) == 0) {
    baseline_tbl |>
      dplyr::mutate(row_id = dplyr::row_number()) |>
      dplyr::left_join(project_tbl |>
        dplyr::mutate(row_id = dplyr::row_number()), by = "row_id", suffix = c("_baseline", "_project"))
  } else {
    dplyr::left_join(baseline_tbl, project_tbl, by = join_by, suffix = c("_baseline", "_project"))
  }

  leakage_tbl <- if (!is.null(leakage_emissions)) {
    leakage_tbl <- dplyr::as_tibble(leakage_emissions)
    leakage_sym <- rlang::ensym(leakage_col)
    if (!rlang::as_string(leakage_sym) %in% names(leakage_tbl)) {
      stop("Leakage tibble must contain the specified leakage column.", call. = FALSE)
    }

    if (length(join_by) == 0) {
      leakage_tbl |>
        dplyr::mutate(row_id = dplyr::row_number())
    } else {
      leakage_tbl
    }
  } else {
    NULL
  }

  if (!is.null(leakage_tbl)) {
    joined <- if (length(join_by) == 0) {
      dplyr::left_join(joined, leakage_tbl, by = "row_id")
    } else {
      dplyr::left_join(joined, leakage_tbl, by = join_by)
    }
  }

  baseline_eval <- if (length(join_by) == 0) {
    rlang::sym(paste0(rlang::as_string(baseline_sym), "_baseline"))
  } else {
    baseline_sym
  }
  project_eval <- if (length(join_by) == 0) {
    rlang::sym(paste0(rlang::as_string(project_sym), "_project"))
  } else {
    project_sym
  }

  leakage_eval <- if (!is.null(leakage_tbl)) {
    rlang::ensym(leakage_col)
  } else {
    NULL
  }

  result <- joined |>
    dplyr::mutate(
      !!output_sym := dplyr::coalesce(!!baseline_eval, 0) - dplyr::coalesce(!!project_eval, 0) -
        if (!is.null(leakage_eval)) dplyr::coalesce(!!leakage_eval, 0) else 0
    )

  if (length(join_by) == 0) {
    dplyr::select(result, !!output_sym)
  } else {
    dplyr::select(result, dplyr::all_of(join_by), !!output_sym)
  }

}
