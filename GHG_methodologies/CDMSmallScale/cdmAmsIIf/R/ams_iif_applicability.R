#' Check AMS-II.F energy intensity criterion
#'
#' AMS-II.F requires that project activities deliver measurable reductions in
#' specific energy consumption for the agricultural service or product. This
#' helper compares baseline and project total energy use relative to an output
#' proxy (e.g. tonnes of grain processed, litres of milk cooled, or area irrigated)
#' and verifies that the fractional reduction exceeds a configurable tolerance.
#'
#' @param baseline_data Tibble containing baseline energy and output indicators.
#' @param project_data Tibble containing project energy and output indicators.
#' @param energy_col Baseline column storing total energy consumption (MWh).
#' @param project_energy_col Project column storing total energy consumption (MWh).
#' @param output_col Column storing the output proxy (e.g. tonnes processed). Must
#'   be present in both datasets.
#' @param tolerance Minimum fractional reduction in specific energy consumption
#'   required to satisfy the criterion. Defaults to 10%.
#' @return A logical scalar indicating whether the specific energy reduction meets
#'   the tolerance threshold.
#' @examples
#' baseline <- tibble::tibble(total_energy_mwh = c(220, 195), output_proxy = c(420, 380))
#' project <- tibble::tibble(total_energy_mwh = c(150, 138), output_proxy = c(420, 380))
#' check_applicability_energy_intensity_iif(baseline, project,
#'   energy_col = "total_energy_mwh",
#'   project_energy_col = "total_energy_mwh",
#'   output_col = "output_proxy"
#' )
#' @export
check_applicability_energy_intensity_iif <- function(baseline_data,
                                                     project_data,
                                                     energy_col = "baseline_total_energy_mwh",
                                                     project_energy_col = "project_total_energy_mwh",
                                                     output_col = "service_level_indicator",
                                                     tolerance = 0.1) {
  baseline_tbl <- dplyr::as_tibble(baseline_data)
  project_tbl <- dplyr::as_tibble(project_data)

  if (!output_col %in% names(baseline_tbl) || !output_col %in% names(project_tbl)) {
    stop("`output_col` must be present in both baseline and project data.", call. = FALSE)
  }

  if (!energy_col %in% names(baseline_tbl)) {
    stop("`energy_col` must exist in `baseline_data`.", call. = FALSE)
  }
  if (!project_energy_col %in% names(project_tbl)) {
    stop("`project_energy_col` must exist in `project_data`.", call. = FALSE)
  }

  baseline_output <- sum(dplyr::pull(baseline_tbl, !!rlang::sym(output_col)), na.rm = TRUE)
  project_output <- sum(dplyr::pull(project_tbl, !!rlang::sym(output_col)), na.rm = TRUE)

  if (baseline_output <= 0 || project_output <= 0) {
    stop("Output proxy totals must be positive for applicability checks.", call. = FALSE)
  }

  baseline_energy <- sum(dplyr::pull(baseline_tbl, !!rlang::sym(energy_col)), na.rm = TRUE)
  project_energy <- sum(dplyr::pull(project_tbl, !!rlang::sym(project_energy_col)), na.rm = TRUE)

  baseline_intensity <- baseline_energy / baseline_output
  project_intensity <- project_energy / project_output

  reduction_fraction <- (baseline_intensity - project_intensity) / baseline_intensity
  is.finite(reduction_fraction) && reduction_fraction >= tolerance
}

#' Check AMS-II.F fuel switching criterion
#'
#' Projects that replace high-carbon fuels with lower-emission alternatives must
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
#' check_applicability_fuel_switching_iif(baseline, project,
#'   energy_col = "thermal_energy_gj",
#'   emission_factor_col = "emission_factor_tco2_per_gj",
#'   project_energy_col = "thermal_energy_gj",
#'   project_emission_factor_col = "emission_factor_tco2_per_gj"
#' )
#' @export
check_applicability_fuel_switching_iif <- function(baseline_data,
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

#' Check AMS-II.F monitoring readiness
#'
#' AMS-II.F monitoring requires consistent records of project energy use,
#' agricultural output proxies, and operating hours. This helper verifies the
#' presence of required columns and ensures they are complete.
#'
#' @param monitoring_data Tibble containing monitoring period records.
#' @param required_cols Character vector of columns that must be present.
#' @return Logical scalar indicating whether the monitoring dataset satisfies the
#'   completeness requirements.
#' @examples
#' monitoring <- tibble::tibble(
#'   facility_id = "rice_mill_1",
#'   project_total_energy_mwh = 12.5,
#'   service_level_indicator = 0.35,
#'   operating_hours = 220
#' )
#' check_applicability_monitoring_iif(monitoring,
#'   required_cols = c("project_total_energy_mwh", "service_level_indicator", "operating_hours")
#' )
#' @export
check_applicability_monitoring_iif <- function(monitoring_data,
                                               required_cols = c(
                                                 "project_total_energy_mwh",
                                                 "service_level_indicator",
                                                 "operating_hours"
                                               )) {
  data_tbl <- dplyr::as_tibble(monitoring_data)

  if (!all(required_cols %in% names(data_tbl))) {
    missing_cols <- setdiff(required_cols, names(data_tbl))
    stop("Monitoring dataset is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  all(
    vapply(
      required_cols,
      function(col) all(!is.na(data_tbl[[col]])),
      logical(1)
    )
  )
}

#' Assess AMS-II.F applicability conditions collectively
#'
#' Bundles the individual applicability checks into a single helper that returns a
#' tidy summary of whether each criterion is satisfied. This wrapper is useful for
#' pre-screening project concepts prior to detailed calculations.
#'
#' @param baseline_data Tibble containing baseline inputs.
#' @param project_data Tibble containing project inputs.
#' @param monitoring_data Tibble containing monitoring dataset.
#' @param intensity_args Optional list of arguments passed to
#'   [check_applicability_energy_intensity_iif()].
#' @param fuel_switch_args Optional list of arguments passed to
#'   [check_applicability_fuel_switching_iif()].
#' @param monitoring_args Optional list of arguments passed to
#'   [check_applicability_monitoring_iif()].
#' @return A tibble with the columns `criterion` and `is_met`.
#' @examples
#' baseline <- tibble::tibble(
#'   baseline_total_energy_mwh = 220,
#'   service_level_indicator = 420,
#'   baseline_thermal_energy_gj = 320,
#'   baseline_thermal_emission_factor_tco2_per_gj = 0.072
#' )
#' project <- tibble::tibble(
#'   project_total_energy_mwh = 150,
#'   service_level_indicator = 420,
#'   project_thermal_energy_gj = 220,
#'   project_thermal_emission_factor_tco2_per_gj = 0.045
#' )
#' monitoring <- tibble::tibble(
#'   project_total_energy_mwh = 12.5,
#'   service_level_indicator = 0.35,
#'   operating_hours = 220
#' )
#' assess_ams_iif_applicability(baseline, project, monitoring)
#' @export
assess_ams_iif_applicability <- function(baseline_data,
                                         project_data,
                                         monitoring_data,
                                         intensity_args = list(),
                                         fuel_switch_args = list(),
                                         monitoring_args = list()) {
  intensity_result <- do.call(
    check_applicability_energy_intensity_iif,
    c(list(baseline_data = baseline_data, project_data = project_data), intensity_args)
  )

  fuel_switch_result <- do.call(
    check_applicability_fuel_switching_iif,
    c(list(baseline_data = baseline_data, project_data = project_data), fuel_switch_args)
  )

  monitoring_result <- do.call(
    check_applicability_monitoring_iif,
    c(list(monitoring_data = monitoring_data), monitoring_args)
  )

  tibble::tibble(
    criterion = c("energy_intensity", "fuel_switching", "monitoring"),
    is_met = c(intensity_result, fuel_switch_result, monitoring_result)
  )
}
