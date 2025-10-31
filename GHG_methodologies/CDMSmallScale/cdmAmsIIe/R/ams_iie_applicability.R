#' Check AMS-II.E energy efficiency criterion
#'
#' AMS-II.E requires demonstrable reductions in energy intensity for the targeted
#' buildings. This helper compares baseline and project total energy use relative
#' to a service output proxy (e.g. conditioned floor area, occupancy, or energy
#' service delivered).
#'
#' @param baseline_data Tibble containing baseline energy and service indicators.
#' @param project_data Tibble containing project energy and service indicators.
#' @param energy_col Baseline column storing total energy consumption (MWh).
#' @param project_energy_col Project column storing total energy consumption (MWh).
#' @param service_col Column storing the service proxy (e.g. floor area). Must be
#'   present in both datasets.
#' @param tolerance Minimum fractional reduction in energy intensity required to
#'   satisfy the criterion. Defaults to 10%.
#' @return A logical scalar indicating whether the intensity reduction meets the
#'   tolerance threshold.
#' @examples
#' baseline <- tibble::tibble(total_energy_mwh = c(220, 195), service_proxy = c(4.2, 3.9))
#' project <- tibble::tibble(total_energy_mwh = c(150, 138), service_proxy = c(4.2, 3.9))
#' check_applicability_energy_efficiency(baseline, project,
#'   energy_col = "total_energy_mwh",
#'   project_energy_col = "total_energy_mwh",
#'   service_col = "service_proxy"
#' )
#' @export
check_applicability_energy_efficiency <- function(baseline_data,
                                                  project_data,
                                                  energy_col = "baseline_total_energy_mwh",
                                                  project_energy_col = "project_total_energy_mwh",
                                                  service_col = "service_level_indicator",
                                                  tolerance = 0.1) {
  baseline_tbl <- dplyr::as_tibble(baseline_data)
  project_tbl <- dplyr::as_tibble(project_data)

  if (!service_col %in% names(baseline_tbl) || !service_col %in% names(project_tbl)) {
    stop("`service_col` must be present in both baseline and project data.", call. = FALSE)
  }

  if (!energy_col %in% names(baseline_tbl)) {
    stop("`energy_col` must exist in `baseline_data`.", call. = FALSE)
  }
  if (!project_energy_col %in% names(project_tbl)) {
    stop("`project_energy_col` must exist in `project_data`.", call. = FALSE)
  }

  baseline_service <- sum(dplyr::pull(baseline_tbl, !!rlang::sym(service_col)), na.rm = TRUE)
  project_service <- sum(dplyr::pull(project_tbl, !!rlang::sym(service_col)), na.rm = TRUE)

  if (baseline_service <= 0 || project_service <= 0) {
    stop("Service proxy totals must be positive for applicability checks.", call. = FALSE)
  }

  baseline_energy <- sum(dplyr::pull(baseline_tbl, !!rlang::sym(energy_col)), na.rm = TRUE)
  project_energy <- sum(dplyr::pull(project_tbl, !!rlang::sym(project_energy_col)), na.rm = TRUE)

  baseline_intensity <- baseline_energy / baseline_service
  project_intensity <- project_energy / project_service

  reduction_fraction <- (baseline_intensity - project_intensity) / baseline_intensity
  is.finite(reduction_fraction) && reduction_fraction >= tolerance
}

#' Check AMS-II.E fuel switching criterion
#'
#' Projects that replace fossil fuels with lower-carbon options must
#' demonstrate that the weighted emission factor of the project fuel mix is below
#' the baseline mix. This helper evaluates weighted averages using thermal energy
#' demand as weights.
#'
#' @param baseline_data Tibble containing baseline thermal energy and emission
#'   factors.
#' @param project_data Tibble containing project thermal energy and emission
#'   factors.
#' @param energy_col Baseline column storing thermal energy demand (GJ).
#' @param emission_factor_col Baseline column storing emission factors (tCO2e/GJ).
#' @param project_energy_col Project column storing thermal energy demand (GJ).
#' @param project_emission_factor_col Project column storing emission factors (tCO2e/GJ).
#' @return Logical scalar indicating whether the project fuel mix is lower in
#'   emissions than the baseline mix.
#' @examples
#' baseline <- tibble::tibble(thermal_energy_gj = c(320, 180), emission_factor_tco2_per_gj = c(0.07, 0.072))
#' project <- tibble::tibble(thermal_energy_gj = c(280, 120), emission_factor_tco2_per_gj = c(0.045, 0.05))
#' check_applicability_fuel_switching(baseline, project,
#'   energy_col = "thermal_energy_gj",
#'   emission_factor_col = "emission_factor_tco2_per_gj",
#'   project_energy_col = "thermal_energy_gj",
#'   project_emission_factor_col = "emission_factor_tco2_per_gj"
#' )
#' @export
check_applicability_fuel_switching <- function(baseline_data,
                                               project_data,
                                               energy_col = "baseline_thermal_energy_gj",
                                               emission_factor_col = "baseline_thermal_emission_factor_tco2_per_gj",
                                               project_energy_col = "project_thermal_energy_gj",
                                               project_emission_factor_col = "project_thermal_emission_factor_tco2_per_gj") {
  baseline_tbl <- dplyr::as_tibble(baseline_data)
  project_tbl <- dplyr::as_tibble(project_data)

  required_baseline <- c(energy_col, emission_factor_col)
  required_project <- c(project_energy_col, project_emission_factor_col)

  if (!all(required_baseline %in% names(baseline_tbl))) {
    stop("Baseline data must contain thermal energy and emission factor columns.", call. = FALSE)
  }
  if (!all(required_project %in% names(project_tbl))) {
    stop("Project data must contain thermal energy and emission factor columns.", call. = FALSE)
  }

  baseline_energy <- sum(dplyr::pull(baseline_tbl, !!rlang::sym(energy_col)), na.rm = TRUE)
  project_energy <- sum(dplyr::pull(project_tbl, !!rlang::sym(project_energy_col)), na.rm = TRUE)

  if (baseline_energy <= 0 || project_energy <= 0) {
    stop("Thermal energy totals must be positive for applicability checks.", call. = FALSE)
  }

  baseline_weighted <- sum(
    dplyr::pull(baseline_tbl, !!rlang::sym(energy_col)) *
      dplyr::pull(baseline_tbl, !!rlang::sym(emission_factor_col)),
    na.rm = TRUE
  ) / baseline_energy

  project_weighted <- sum(
    dplyr::pull(project_tbl, !!rlang::sym(project_energy_col)) *
      dplyr::pull(project_tbl, !!rlang::sym(project_emission_factor_col)),
    na.rm = TRUE
  ) / project_energy

  is.finite(baseline_weighted) && is.finite(project_weighted) && project_weighted <= baseline_weighted
}

#' Check AMS-II.E monitoring readiness
#'
#' AMS-II.E monitoring requires consistent records of project energy use,
#' occupancy or service indicators, and operating hours. This helper verifies the
#' presence of required columns and ensures they are complete.
#'
#' @param monitoring_data Tibble containing monitoring period records.
#' @param required_cols Character vector of columns that must be present.
#' @return Logical scalar indicating whether the monitoring dataset satisfies the
#'   completeness requirements.
#' @examples
#' monitoring <- tibble::tibble(
#'   building_id = "Office_A",
#'   project_total_energy_mwh = 12.5,
#'   service_level_indicator = 0.35,
#'   operating_hours = 220
#' )
#' check_applicability_monitoring(monitoring,
#'   required_cols = c("project_total_energy_mwh", "service_level_indicator", "operating_hours")
#' )
#' @export
check_applicability_monitoring <- function(monitoring_data,
                                           required_cols = c(
                                             "project_total_energy_mwh",
                                             "service_level_indicator",
                                             "operating_hours"
                                           )) {
  data_tbl <- dplyr::as_tibble(monitoring_data)

  if (!all(required_cols %in% names(data_tbl))) {
    return(FALSE)
  }

  complete_cases <- data_tbl |>
    dplyr::select(dplyr::all_of(required_cols)) |>
    stats::complete.cases()

  all(complete_cases)
}

#' Assess overall AMS-II.E applicability
#'
#' Combines individual applicability checks into a tidy diagnostics tibble. The
#' result includes boolean flags for each criterion and an overall indicator that
#' all requirements are met.
#'
#' @param baseline_data Baseline dataset used for applicability checks.
#' @param project_data Project dataset used for applicability checks.
#' @param monitoring_data Monitoring dataset used for completeness checks.
#' @param evaluate_fuel_switch Logical indicating whether to evaluate the fuel
#'   switching criterion. Set to `FALSE` when the project does not involve fuel
#'   switching.
#' @return A tibble summarising AMS-II.E applicability results.
#' @examples
#' baseline <- tibble::tibble(
#'   baseline_total_energy_mwh = c(220, 195),
#'   baseline_thermal_energy_gj = c(320, 180),
#'   baseline_thermal_emission_factor_tco2_per_gj = c(0.07, 0.072),
#'   service_level_indicator = c(4.2, 3.9)
#' )
#' project <- tibble::tibble(
#'   project_total_energy_mwh = c(150, 138),
#'   project_thermal_energy_gj = c(280, 120),
#'   project_thermal_emission_factor_tco2_per_gj = c(0.045, 0.05),
#'   service_level_indicator = c(4.2, 3.9)
#' )
#' monitoring <- tibble::tibble(
#'   building_id = c("Office_A", "Office_B"),
#'   project_total_energy_mwh = c(12.5, 11.2),
#'   service_level_indicator = c(0.35, 0.32),
#'   operating_hours = c(220, 215)
#' )
#' assess_ams_iie_applicability(baseline, project, monitoring)
#' @export
assess_ams_iie_applicability <- function(baseline_data,
                                         project_data,
                                         monitoring_data,
                                         evaluate_fuel_switch = TRUE) {
  energy_efficiency <- check_applicability_energy_efficiency(baseline_data, project_data)
  fuel_switching <- if (evaluate_fuel_switch) {
    check_applicability_fuel_switching(baseline_data, project_data)
  } else {
    TRUE
  }
  monitoring_ready <- check_applicability_monitoring(monitoring_data)

  tibble::tibble(
    energy_efficiency = energy_efficiency,
    fuel_switching = fuel_switching,
    monitoring_ready = monitoring_ready,
    overall_applicable = all(energy_efficiency, fuel_switching, monitoring_ready)
  )
}
