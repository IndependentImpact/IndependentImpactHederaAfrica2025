#' Calculate baseline methane emissions from unmanaged manure systems
#'
#' Equation 1 of AMS-III.D multiplies volatile solids generation, methane
#' producing potential, and a baseline methane conversion factor to obtain
#' baseline methane emissions expressed in tCO2e. The helper assumes tidy input
#' data and returns grouped summaries suitable for further aggregation.
#'
#' @param data Tibble containing baseline monitoring data.
#' @param volatile_solids_col Column storing volatile solids generated per day
#'   (kg VS/day).
#' @param methane_potential_col Column storing methane producing potential
#'   (m3 CH4/kg VS).
#' @param methane_conversion_factor_col Column storing the baseline methane
#'   conversion factor (fraction).
#' @param days_col Column storing the number of days represented by each record.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column with baseline emissions in tCO2e.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#'   Defaults to 0.00067 t CH4 per m3.
#' @param gwp_ch4 Global warming potential of methane. Defaults to 28.
#' @return Tibble containing grouping columns (if supplied) and baseline
#'   emissions.
#' @examples
#' library(tibble)
#' baseline <- tibble(
#'   farm_id = c("A", "B"),
#'   volatile_solids_kg_per_day = c(45, 30),
#'   methane_potential_m3_per_kg_vs = 0.25,
#'   baseline_mcf_fraction = c(0.8, 0.75),
#'   days_in_period = 365
#' )
#' calculate_baseline_methane_emissions_iiid(baseline)
#' @export
calculate_baseline_methane_emissions_iiid <- function(data,
                                                     volatile_solids_col = "volatile_solids_kg_per_day",
                                                     methane_potential_col = "methane_potential_m3_per_kg_vs",
                                                     methane_conversion_factor_col = "baseline_mcf_fraction",
                                                     days_col = "days_in_period",
                                                     group_cols = NULL,
                                                     output_col = "baseline_emissions_tco2e",
                                                     methane_density_t_per_m3 = 0.00067,
                                                     gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  volatile_solids_col <- if (rlang::is_string(volatile_solids_col)) volatile_solids_col else rlang::as_name(rlang::enquo(volatile_solids_col))
  methane_potential_col <- if (rlang::is_string(methane_potential_col)) methane_potential_col else rlang::as_name(rlang::enquo(methane_potential_col))
  methane_conversion_factor_col <- if (rlang::is_string(methane_conversion_factor_col)) methane_conversion_factor_col else rlang::as_name(rlang::enquo(methane_conversion_factor_col))
  days_col <- if (rlang::is_string(days_col)) days_col else rlang::as_name(rlang::enquo(days_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .baseline_component = .data[[volatile_solids_col]] *
        .data[[methane_potential_col]] *
        .data[[methane_conversion_factor_col]] *
        .data[[days_col]] *
        methane_density_t_per_m3 *
        gwp_ch4
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.baseline_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.baseline_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate project methane emissions after recovery and destruction
#'
#' Equation 2 of AMS-III.D estimates project emissions as the methane generated
#' under project operation minus methane recovered and destroyed. The helper uses
#' project methane conversion factors, capture efficiencies, and destruction
#' efficiencies supplied in tidy columns.
#'
#' @inheritParams calculate_baseline_methane_emissions_iiid
#' @param project_methane_conversion_factor_col Column storing the project MCF
#'   (fraction).
#' @param capture_efficiency_col Column storing the fraction of methane captured
#'   by the recovery system.
#' @param destruction_efficiency_col Column storing the fraction of captured
#'   methane destroyed through combustion or flaring.
#' @param output_col Name of the output column with project emissions in tCO2e.
#' @return Tibble containing grouped project emissions.
#' @examples
#' library(tibble)
#' project <- tibble(
#'   farm_id = c("A", "B"),
#'   volatile_solids_kg_per_day = c(45, 30),
#'   methane_potential_m3_per_kg_vs = 0.25,
#'   project_mcf_fraction = c(0.65, 0.6),
#'   capture_efficiency_fraction = c(0.85, 0.8),
#'   destruction_efficiency_fraction = c(0.98, 0.95),
#'   days_in_period = 365
#' )
#' calculate_project_methane_emissions_iiid(project)
#' @export
calculate_project_methane_emissions_iiid <- function(data,
                                                    volatile_solids_col = "volatile_solids_kg_per_day",
                                                    methane_potential_col = "methane_potential_m3_per_kg_vs",
                                                    project_methane_conversion_factor_col = "project_mcf_fraction",
                                                    capture_efficiency_col = "capture_efficiency_fraction",
                                                    destruction_efficiency_col = "destruction_efficiency_fraction",
                                                    days_col = "days_in_period",
                                                    group_cols = NULL,
                                                    output_col = "project_emissions_tco2e",
                                                    methane_density_t_per_m3 = 0.00067,
                                                    gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  volatile_solids_col <- if (rlang::is_string(volatile_solids_col)) volatile_solids_col else rlang::as_name(rlang::enquo(volatile_solids_col))
  methane_potential_col <- if (rlang::is_string(methane_potential_col)) methane_potential_col else rlang::as_name(rlang::enquo(methane_potential_col))
  project_methane_conversion_factor_col <- if (rlang::is_string(project_methane_conversion_factor_col)) project_methane_conversion_factor_col else rlang::as_name(rlang::enquo(project_methane_conversion_factor_col))
  capture_efficiency_col <- if (rlang::is_string(capture_efficiency_col)) capture_efficiency_col else rlang::as_name(rlang::enquo(capture_efficiency_col))
  destruction_efficiency_col <- if (rlang::is_string(destruction_efficiency_col)) destruction_efficiency_col else rlang::as_name(rlang::enquo(destruction_efficiency_col))
  days_col <- if (rlang::is_string(days_col)) days_col else rlang::as_name(rlang::enquo(days_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .project_generated = .data[[volatile_solids_col]] *
        .data[[methane_potential_col]] *
        .data[[project_methane_conversion_factor_col]] *
        .data[[days_col]] *
        methane_density_t_per_m3 *
        gwp_ch4,
      .captured = .project_generated * .data[[capture_efficiency_col]],
      .destroyed = .captured * .data[[destruction_efficiency_col]],
      .project_component = .project_generated - .destroyed
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.project_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.project_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Convert recovered methane volumes into destroyed emissions
#'
#' Equation 3 converts monitored methane recovery volumes to tCO2e destroyed
#' emissions using methane density, global warming potential, and optional
#' destruction efficiency factors.
#'
#' @param data Tibble containing methane recovery monitoring data.
#' @param methane_recovered_col Column storing methane recovered in m3.
#' @param destruction_efficiency_col Column storing the fraction of recovered
#'   methane destroyed. Defaults to `"destruction_efficiency_fraction"`.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column for recovered methane expressed in
#'   tCO2e.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#' @param gwp_ch4 Global warming potential of methane.
#' @return Tibble containing grouped totals of destroyed methane in tCO2e.
#' @examples
#' recovery <- tibble::tibble(
#'   digester_id = c("D1", "D2"),
#'   methane_recovered_m3 = c(15000, 12000),
#'   destruction_efficiency_fraction = c(0.99, 0.98)
#' )
#' calculate_recovered_methane_iiid(recovery, group_cols = "digester_id")
#' @export
calculate_recovered_methane_iiid <- function(data,
                                             methane_recovered_col = "methane_recovered_m3",
                                             destruction_efficiency_col = "destruction_efficiency_fraction",
                                             group_cols = NULL,
                                             output_col = "recovered_methane_tco2e",
                                             methane_density_t_per_m3 = 0.00067,
                                             gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  methane_recovered_col <- if (rlang::is_string(methane_recovered_col)) methane_recovered_col else rlang::as_name(rlang::enquo(methane_recovered_col))
  destruction_efficiency_col <- if (rlang::is_string(destruction_efficiency_col)) destruction_efficiency_col else rlang::as_name(rlang::enquo(destruction_efficiency_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  if (!destruction_efficiency_col %in% names(data_tbl)) {
    data_tbl[[destruction_efficiency_col]] <- 1
  }

  emissions <- data_tbl |>
    dplyr::mutate(
      .recovered_component = .data[[methane_recovered_col]] *
        methane_density_t_per_m3 *
        gwp_ch4 *
        dplyr::if_else(
          is.na(.data[[destruction_efficiency_col]]),
          1,
          .data[[destruction_efficiency_col]]
        )
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.recovered_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.recovered_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Aggregate leakage emissions for AMS-III.D
#'
#' The methodology requires accounting for leakage resulting from displaced
#' manure management practices. Provide leakage totals in tCO2e per monitoring
#' group using this helper.
#'
#' @param data Tibble containing leakage data.
#' @param leakage_col Column storing leakage emissions in tCO2e.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output leakage column.
#' @return Tibble containing grouped leakage emissions.
#' @examples
#' leakage <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   leakage_emissions_tco2e = c(5, 3)
#' )
#' calculate_leakage_emissions_iiid(leakage)
#' @export
calculate_leakage_emissions_iiid <- function(data,
                                             leakage_col = "leakage_emissions_tco2e",
                                             group_cols = NULL,
                                             output_col = "leakage_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  leakage_col <- if (rlang::is_string(leakage_col)) leakage_col else rlang::as_name(rlang::enquo(leakage_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  if (!leakage_col %in% names(data_tbl)) {
    data_tbl[[leakage_col]] <- 0
  }

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(data_tbl[[leakage_col]], na.rm = TRUE))
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data[[leakage_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate net emission reductions for AMS-III.D
#'
#' Equation 4 computes emission reductions as baseline emissions minus project
#' emissions, plus the methane destroyed through recovery, and minus leakage. The
#' helper expects pre-aggregated tibbles produced by the other equation helpers
#' and joins them on the specified grouping columns.
#'
#' @param baseline Tibble returned by `calculate_baseline_methane_emissions_iiid()`.
#' @param project Tibble returned by `calculate_project_methane_emissions_iiid()`.
#' @param recovered Tibble returned by `calculate_recovered_methane_iiid()`.
#' @param leakage Tibble returned by `calculate_leakage_emissions_iiid()`.
#' @param group_cols Character vector specifying grouping columns present in all
#'   inputs.
#' @param output_col Name of the emission reduction output column.
#' @return Tibble containing emission reductions per group.
#' @examples
#' baseline <- tibble::tibble(farm_id = "A", baseline_emissions_tco2e = 100)
#' project <- tibble::tibble(farm_id = "A", project_emissions_tco2e = 20)
#' recovered <- tibble::tibble(farm_id = "A", recovered_methane_tco2e = 60)
#' leakage <- tibble::tibble(farm_id = "A", leakage_emissions_tco2e = 5)
#' calculate_emission_reductions_iiid(
#'   baseline,
#'   project,
#'   recovered,
#'   leakage,
#'   group_cols = "farm_id"
#' )
#' @export
calculate_emission_reductions_iiid <- function(baseline,
                                               project,
                                               recovered,
                                               leakage,
                                               group_cols = NULL,
                                               output_col = "emission_reductions_tco2e") {
  baseline_tbl <- tibble::as_tibble(baseline)
  project_tbl <- tibble::as_tibble(project)
  recovered_tbl <- tibble::as_tibble(recovered)
  leakage_tbl <- tibble::as_tibble(leakage)

  baseline_value_col <- setdiff(names(baseline_tbl), group_cols)
  project_value_col <- setdiff(names(project_tbl), group_cols)
  recovered_value_col <- setdiff(names(recovered_tbl), group_cols)
  leakage_value_col <- setdiff(names(leakage_tbl), group_cols)

  if (length(project_value_col) == 0) {
    project_value_col <- "project_emissions_tco2e"
    project_tbl[[project_value_col]] <- 0
  }

  if (length(recovered_value_col) == 0) {
    recovered_value_col <- "recovered_methane_tco2e"
    recovered_tbl[[recovered_value_col]] <- 0
  }

  if (length(leakage_value_col) == 0) {
    leakage_value_col <- "leakage_emissions_tco2e"
    leakage_tbl[[leakage_value_col]] <- 0
  }

  reduction_tbl <- baseline_tbl |>
    dplyr::left_join(project_tbl, by = group_cols) |>
    dplyr::left_join(recovered_tbl, by = group_cols) |>
    dplyr::left_join(leakage_tbl, by = group_cols) |>
    dplyr::mutate(
      !!output_col := .data[[baseline_value_col]] -
        dplyr::coalesce(.data[[project_value_col]], 0) +
        dplyr::coalesce(.data[[recovered_value_col]], 0) -
        dplyr::coalesce(.data[[leakage_value_col]], 0)
    )

  if (is.null(group_cols)) {
    reduction_tbl |>
      dplyr::select(dplyr::all_of(output_col))
  } else {
    reduction_tbl |>
      dplyr::select(dplyr::all_of(group_cols), dplyr::all_of(output_col))
  }
}
