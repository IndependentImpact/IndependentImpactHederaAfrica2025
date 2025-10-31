#' Run the AMS-II.Q emission reduction workflow
#'
#' Chains the AMS-II.Q equation helpers to convert baseline commercial building
#' operation and post-retrofit monitoring data into emission reductions. Users
#' supply baseline and project datasets describing energy use, emission factors,
#' optional service output (e.g. conditioned floor area served), onsite energy
#' supply, and leakage. The helper returns a tidy tibble containing baseline
#' emissions, project emissions split between grid consumption and onsite energy,
#' leakage, energy intensity diagnostics, and net emission reductions.
#'
#' @param baseline_data Tibble containing baseline monitoring observations.
#' @param project_data Tibble containing project monitoring observations.
#' @param leakage_data Optional tibble containing leakage emissions in tCO2e.
#' @param group_cols Optional character vector of grouping columns shared across
#'   the inputs.
#' @param baseline_energy_consumption_col Column storing baseline energy use.
#' @param baseline_emission_factor_col Column storing baseline emission factors
#'   (tCO2e per unit of energy).
#' @param baseline_service_output_col Optional column storing baseline service
#'   output (e.g. chilled water or conditioned floor area). Set to `NULL` when
#'   unavailable.
#' @param project_energy_consumption_col Column storing project grid energy use.
#' @param project_emission_factor_col Column storing project emission factors
#'   (tCO2e per unit of energy).
#' @param project_service_output_col Optional column storing project service
#'   output. Set to `NULL` when not monitored.
#' @param project_onsite_energy_col Optional column storing onsite energy use.
#'   Set to `NULL` when the project does not operate onsite supply assets.
#' @param project_onsite_emission_factor_col Column storing emission factors for
#'   onsite energy. Required when `project_onsite_energy_col` is supplied.
#' @param leakage_col Column storing leakage emissions (tCO2e) when
#'   `leakage_data` is supplied.
#' @return Tibble containing baseline emissions, project grid and onsite
#'   emissions, leakage, energy intensity diagnostics, and emission reductions.
#' @examples
#' baseline <- tibble::tibble(
#'   building_id = c("A", "B"),
#'   baseline_energy_use_mwh = c(800, 560),
#'   baseline_emission_factor_tco2_per_mwh = 0.62,
#'   baseline_service_output_mwh = c(720, 510)
#' )
#' project <- tibble::tibble(
#'   building_id = c("A", "B"),
#'   project_energy_use_mwh = c(520, 380),
#'   project_emission_factor_tco2_per_mwh = 0.58,
#'   project_service_output_mwh = c(720, 510),
#'   project_onsite_energy_gj = c(120, 40),
#'   project_onsite_emission_factor_tco2_per_gj = 0.05
#' )
#' estimate_emission_reductions_ams_iiq(
#'   baseline,
#'   project,
#'   group_cols = "building_id"
#' )
#' @export
estimate_emission_reductions_ams_iiq <- function(baseline_data,
                                                project_data,
                                                leakage_data = NULL,
                                                group_cols = NULL,
                                                baseline_energy_consumption_col = "baseline_energy_use_mwh",
                                                baseline_emission_factor_col = "baseline_emission_factor_tco2_per_mwh",
                                                baseline_service_output_col = "baseline_service_output_mwh",
                                                project_energy_consumption_col = "project_energy_use_mwh",
                                                project_emission_factor_col = "project_emission_factor_tco2_per_mwh",
                                                project_service_output_col = "project_service_output_mwh",
                                                project_onsite_energy_col = "project_onsite_energy_gj",
                                                project_onsite_emission_factor_col = "project_onsite_emission_factor_tco2_per_gj",
                                                leakage_col = "leakage_emissions_tco2e") {
  baseline_energy_consumption_col <- if (rlang::is_string(baseline_energy_consumption_col)) baseline_energy_consumption_col else rlang::as_name(rlang::enquo(baseline_energy_consumption_col))
  baseline_emission_factor_col <- if (rlang::is_string(baseline_emission_factor_col)) baseline_emission_factor_col else rlang::as_name(rlang::enquo(baseline_emission_factor_col))
  if (!is.null(baseline_service_output_col)) {
    baseline_service_output_col <- if (is.null(baseline_service_output_col)) NULL else if (rlang::is_string(baseline_service_output_col)) baseline_service_output_col else rlang::as_name(rlang::enquo(baseline_service_output_col))
  }
  project_energy_consumption_col <- if (rlang::is_string(project_energy_consumption_col)) project_energy_consumption_col else rlang::as_name(rlang::enquo(project_energy_consumption_col))
  project_emission_factor_col <- if (rlang::is_string(project_emission_factor_col)) project_emission_factor_col else rlang::as_name(rlang::enquo(project_emission_factor_col))
  if (!is.null(project_service_output_col)) {
    project_service_output_col <- if (is.null(project_service_output_col)) NULL else if (rlang::is_string(project_service_output_col)) project_service_output_col else rlang::as_name(rlang::enquo(project_service_output_col))
  }
  if (!is.null(project_onsite_energy_col)) {
    project_onsite_energy_col <- if (is.null(project_onsite_energy_col)) NULL else if (rlang::is_string(project_onsite_energy_col)) project_onsite_energy_col else rlang::as_name(rlang::enquo(project_onsite_energy_col))
  }
  if (!is.null(project_onsite_emission_factor_col)) {
    project_onsite_emission_factor_col <- if (is.null(project_onsite_emission_factor_col)) NULL else if (rlang::is_string(project_onsite_emission_factor_col)) project_onsite_emission_factor_col else rlang::as_name(rlang::enquo(project_onsite_emission_factor_col))
  }
  leakage_col <- if (rlang::is_string(leakage_col)) leakage_col else rlang::as_name(rlang::enquo(leakage_col))

  baseline_emissions <- calculate_baseline_building_emissions_iiq(
    baseline_data = baseline_data,
    energy_consumption_col = baseline_energy_consumption_col,
    emission_factor_col = baseline_emission_factor_col,
    service_output_col = baseline_service_output_col,
    group_cols = group_cols
  )

  project_grid <- calculate_project_building_emissions_iiq(
    project_data = project_data,
    energy_consumption_col = project_energy_consumption_col,
    emission_factor_col = project_emission_factor_col,
    service_output_col = project_service_output_col,
    group_cols = group_cols,
    output_col = "project_grid_emissions_tco2e",
    intensity_col = "project_energy_intensity"
  )

  project_onsite <- calculate_project_onsite_energy_emissions_iiq(
    project_data = project_data,
    onsite_energy_col = project_onsite_energy_col,
    emission_factor_col = project_onsite_emission_factor_col,
    group_cols = group_cols,
    output_col = "project_onsite_emissions_tco2e"
  )

  join_cols <- if (is.null(group_cols) || length(group_cols) == 0) character() else group_cols

  project_totals <- if (length(join_cols) == 0) {
    dplyr::bind_cols(project_grid, project_onsite)
  } else {
    project_grid |>
      dplyr::left_join(project_onsite, by = join_cols)
  }

  project_totals <- project_totals |>
    dplyr::mutate(
      project_emissions_tco2e = dplyr::coalesce(project_grid_emissions_tco2e, 0) +
        dplyr::coalesce(project_onsite_emissions_tco2e, 0)
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
    leakage_tbl <- tibble::as_tibble(leakage_data)
    names(leakage_tbl)[names(leakage_tbl) == leakage_col] <- "leakage_emissions_tco2e"
    leakage_tbl
  }

  combined <- if (length(join_cols) == 0) {
    dplyr::bind_cols(baseline_emissions, project_totals, leakage_totals)
  } else {
    baseline_emissions |>
      dplyr::left_join(project_totals, by = join_cols) |>
      dplyr::left_join(leakage_totals, by = join_cols)
  }

  reductions <- calculate_emission_reductions_iiq(
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
    dplyr::bind_cols(
      combined,
      reductions |>
        dplyr::select(dplyr::all_of(reduction_cols))
    )
  } else {
    combined |>
      dplyr::left_join(
        reductions |>
          dplyr::select(dplyr::all_of(reduction_cols)),
        by = join_cols
      )
  }
}
