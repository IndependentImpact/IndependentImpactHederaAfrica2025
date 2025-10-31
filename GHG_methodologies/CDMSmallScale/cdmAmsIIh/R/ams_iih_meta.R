#' Run the AMS-II.H emission reduction workflow
#'
#' Chains the AMS-II.H equation helpers to convert decentralized baseline
#' monitoring data and centralized project monitoring data into emission
#' reductions. Users provide baseline and project datasets describing fuel use,
#' emission factors, and (optionally) useful thermal output together with
#' auxiliary electricity requirements and leakage estimates. The helper returns a
#' tidy tibble containing baseline emissions, project emissions broken into
#' central and auxiliary components, leakage, specific energy diagnostics, and
#' net emission reductions.
#'
#' @param baseline_data Tibble containing baseline monitoring observations.
#' @param project_data Tibble containing project monitoring observations.
#' @param leakage_data Optional tibble containing leakage emissions in tCO2e.
#' @param group_cols Optional character vector of grouping columns shared across
#'   the inputs.
#' @param baseline_fuel_consumption_col Column storing baseline fuel use (GJ).
#' @param baseline_emission_factor_col Column storing baseline emission factors
#'   (tCO2e/GJ).
#' @param baseline_useful_output_col Optional column storing useful thermal
#'   output in the baseline (GJ). Set to `NULL` when not available.
#' @param project_fuel_consumption_col Column storing project fuel use (GJ).
#' @param project_emission_factor_col Column storing project emission factors
#'   (tCO2e/GJ).
#' @param project_useful_output_col Optional column storing useful project
#'   output (GJ). Set to `NULL` when not available.
#' @param project_aux_electricity_col Optional column storing auxiliary
#'   electricity use (MWh). Set to `NULL` when auxiliary electricity is absent.
#' @param project_electricity_emission_factor_col Column storing emission factors
#'   for auxiliary electricity (tCO2e/MWh). Required when
#'   `project_aux_electricity_col` is supplied.
#' @param leakage_col Column storing leakage emissions (tCO2e) when
#'   `leakage_data` is supplied.
#' @return Tibble containing baseline emissions, project central and auxiliary
#'   emissions, leakage, specific energy diagnostics (when available), and net
#'   emission reductions.
#' @examples
#' baseline <- tibble::tibble(
#'   facility = c("Line_A", "Line_B"),
#'   baseline_fuel_use_gj = c(4200, 3150),
#'   baseline_emission_factor_tco2_per_gj = 0.071,
#'   baseline_useful_output_gj = c(3600, 2700)
#' )
#' project <- tibble::tibble(
#'   facility = c("Line_A", "Line_B"),
#'   project_fuel_use_gj = c(3000, 2250),
#'   project_emission_factor_tco2_per_gj = 0.068,
#'   project_useful_output_gj = c(2880, 2160),
#'   project_auxiliary_electricity_mwh = c(220, 165),
#'   project_electricity_emission_factor_tco2_per_mwh = 0.62
#' )
#' estimate_emission_reductions_ams_iih(
#'   baseline,
#'   project,
#'   group_cols = "facility"
#' )
#' @export
estimate_emission_reductions_ams_iih <- function(baseline_data,
                                                 project_data,
                                                 leakage_data = NULL,
                                                 group_cols = NULL,
                                                 baseline_fuel_consumption_col = "baseline_fuel_use_gj",
                                                 baseline_emission_factor_col = "baseline_emission_factor_tco2_per_gj",
                                                 baseline_useful_output_col = "baseline_useful_output_gj",
                                                 project_fuel_consumption_col = "project_fuel_use_gj",
                                                 project_emission_factor_col = "project_emission_factor_tco2_per_gj",
                                                 project_useful_output_col = "project_useful_output_gj",
                                                 project_aux_electricity_col = "project_auxiliary_electricity_mwh",
                                                 project_electricity_emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
                                                 leakage_col = "leakage_emissions_tco2e") {
  baseline_fuel_consumption_col <- rlang::as_name(rlang::enexpr(baseline_fuel_consumption_col))
  baseline_emission_factor_col <- rlang::as_name(rlang::enexpr(baseline_emission_factor_col))
  if (!is.null(baseline_useful_output_col)) {
    baseline_useful_output_col <- rlang::as_name(rlang::enexpr(baseline_useful_output_col))
  }
  project_fuel_consumption_col <- rlang::as_name(rlang::enexpr(project_fuel_consumption_col))
  project_emission_factor_col <- rlang::as_name(rlang::enexpr(project_emission_factor_col))
  if (!is.null(project_useful_output_col)) {
    project_useful_output_col <- rlang::as_name(rlang::enexpr(project_useful_output_col))
  }
  if (!is.null(project_aux_electricity_col)) {
    project_aux_electricity_col <- rlang::as_name(rlang::enexpr(project_aux_electricity_col))
  }
  if (!is.null(project_electricity_emission_factor_col)) {
    project_electricity_emission_factor_col <- rlang::as_name(rlang::enexpr(project_electricity_emission_factor_col))
  }
  leakage_col <- rlang::as_name(rlang::enexpr(leakage_col))

  baseline_emissions <- do.call(
    calculate_baseline_decentralized_emissions_iih,
    list(
      baseline_data = baseline_data,
      fuel_consumption_col = baseline_fuel_consumption_col,
      emission_factor_col = baseline_emission_factor_col,
      useful_output_col = baseline_useful_output_col,
      group_cols = group_cols
    )
  )

  project_central <- do.call(
    calculate_project_central_emissions_iih,
    list(
      project_data = project_data,
      fuel_consumption_col = project_fuel_consumption_col,
      emission_factor_col = project_emission_factor_col,
      useful_output_col = project_useful_output_col,
      group_cols = group_cols
    )
  )

  project_auxiliary <- do.call(
    calculate_project_auxiliary_electricity_iih,
    list(
      project_data = project_data,
      electricity_consumption_col = project_aux_electricity_col,
      emission_factor_col = project_electricity_emission_factor_col,
      group_cols = group_cols
    )
  )

  join_cols <- if (is.null(group_cols) || length(group_cols) == 0) character() else group_cols

  project_totals <- if (length(join_cols) == 0) {
    dplyr::bind_cols(project_central, project_auxiliary)
  } else {
    project_central |>
      dplyr::left_join(project_auxiliary, by = join_cols)
  }

  project_totals <- project_totals |>
    dplyr::mutate(
      project_emissions_tco2e = dplyr::coalesce(project_central_emissions_tco2e, 0) +
        dplyr::coalesce(project_auxiliary_emissions_tco2e, 0)
    )

  leakage_totals <- if (is.null(leakage_data)) {
    if (length(join_cols) == 0) {
      tibble::tibble(leakage_emissions_tco2e = 0)
    } else {
      baseline_emissions |>
        dplyr::select(dplyr::all_of(join_cols)) |>
        dplyr::distinct() |>
        dplyr::mutate(leakage_emissions_tco2e = 0)
    }
  } else {
    tibble::as_tibble(leakage_data) |>
      dplyr::rename(leakage_emissions_tco2e = !!rlang::ensym(leakage_col))
  }

  if (length(join_cols) == 0) {
    combined <- dplyr::bind_cols(baseline_emissions, project_totals, leakage_totals)
  } else {
    combined <- baseline_emissions |>
      dplyr::left_join(project_totals, by = join_cols) |>
      dplyr::left_join(leakage_totals, by = join_cols)
  }

  reductions <- calculate_emission_reductions_iih(
    baseline_emissions = combined |>
      dplyr::select(dplyr::all_of(c(join_cols, "baseline_emissions_tco2e"))),
    project_emissions = combined |>
      dplyr::select(dplyr::all_of(c(join_cols, "project_emissions_tco2e"))),
    leakage_emissions = combined |>
      dplyr::select(dplyr::all_of(c(join_cols, "leakage_emissions_tco2e"))),
    group_cols = group_cols
  )

  reduction_cols <- c(join_cols, "emission_reductions_tco2e")
  if (length(join_cols) == 0) {
    dplyr::bind_cols(combined, reductions |>
      dplyr::select(dplyr::all_of(reduction_cols)))
  } else {
    combined |>
      dplyr::left_join(
        reductions |>
          dplyr::select(dplyr::all_of(reduction_cols)),
        by = join_cols
      )
  }
}
