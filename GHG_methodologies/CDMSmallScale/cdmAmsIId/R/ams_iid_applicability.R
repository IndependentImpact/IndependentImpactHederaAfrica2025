#' Check AMS-II.D energy efficiency criteria
#'
#' AMS-II.D requires that project equipment delivers measurable energy savings
#' relative to the baseline. This helper compares baseline and project fuel use
#' (after efficiency adjustments) to confirm that the project scenario reduces
#' specific fuel consumption.
#'
#' @param baseline_data Tibble containing baseline fuel and efficiency metrics.
#' @param project_data Tibble containing project fuel and efficiency metrics.
#' @param fuel_col Baseline column storing fuel quantities.
#' @param efficiency_col Baseline column storing equipment efficiency.
#' @param project_fuel_col Project column storing fuel quantities.
#' @param project_efficiency_col Project column storing equipment efficiency.
#' @param tolerance Minimum fractional reduction in specific fuel consumption
#'   (fuel input per unit of baseline useful energy output) that must be
#'   achieved to satisfy the applicability condition. Defaults to 10%.
#' @return A logical scalar indicating whether the criterion is met.
#' @examples
#' baseline <- tibble::tibble(baseline_fuel_quantity = 1200, baseline_efficiency = 0.72)
#' project <- tibble::tibble(project_fuel_quantity = 950, project_efficiency = 0.84)
#' check_applicability_energy_efficiency(baseline, project)
#' @export
check_applicability_energy_efficiency <- function(baseline_data,
                                                  project_data,
                                                  fuel_col = "baseline_fuel_quantity",
                                                  efficiency_col = "baseline_efficiency",
                                                  project_fuel_col = "project_fuel_quantity",
                                                  project_efficiency_col = "project_efficiency",
                                                  tolerance = 0.1) {
  baseline_tbl <- dplyr::as_tibble(baseline_data)
  project_tbl <- dplyr::as_tibble(project_data)

  if (any(baseline_tbl[[efficiency_col]] <= 0, na.rm = TRUE) ||
      any(project_tbl[[project_efficiency_col]] <= 0, na.rm = TRUE)) {
    stop("Efficiencies must be positive for applicability checks.", call. = FALSE)
  }

  baseline_output <- sum(baseline_tbl[[fuel_col]] * baseline_tbl[[efficiency_col]], na.rm = TRUE)
  if (!is.finite(baseline_output) || baseline_output <= 0) {
    stop("Baseline useful energy output must be positive for applicability checks.", call. = FALSE)
  }

  specific_baseline <- sum(baseline_tbl[[fuel_col]], na.rm = TRUE) / baseline_output
  specific_project <- sum(project_tbl[[project_fuel_col]], na.rm = TRUE) / baseline_output

  reduction <- specific_baseline - specific_project
  reduction_fraction <- reduction / specific_baseline

  is.finite(reduction_fraction) && reduction_fraction >= tolerance
}

#' Check AMS-II.D fuel switching criteria
#'
#' Projects that switch to lower-carbon fuels must document that the project
#' fuel mix has a lower emission factor than the baseline mix. This helper
#' compares weighted emission factors.
#'
#' @param baseline_data Tibble containing baseline fuel quantities and emission
#'   factors.
#' @param project_data Tibble containing project fuel quantities and emission
#'   factors.
#' @param fuel_col Baseline column storing fuel quantities.
#' @param emission_factor_col Baseline column storing emission factors (tCO2e per
#'   unit energy).
#' @param project_fuel_col Project column storing fuel quantities.
#' @param project_emission_factor_col Project column storing emission factors.
#' @return Logical scalar indicating whether project emission factors are lower.
#' @examples
#' baseline <- tibble::tibble(baseline_fuel_quantity = c(800, 400),
#'                            baseline_emission_factor_tco2_per_gj = c(0.094, 0.098))
#' project <- tibble::tibble(project_fuel_quantity = c(700, 200),
#'                           project_emission_factor_tco2_per_gj = c(0.082, 0.082))
#' check_applicability_fuel_switching(baseline, project)
#' @export
check_applicability_fuel_switching <- function(baseline_data,
                                               project_data,
                                               fuel_col = "baseline_fuel_quantity",
                                               emission_factor_col = "baseline_emission_factor_tco2_per_gj",
                                               project_fuel_col = "project_fuel_quantity",
                                               project_emission_factor_col = "project_emission_factor_tco2_per_gj") {
  baseline_tbl <- dplyr::as_tibble(baseline_data)
  project_tbl <- dplyr::as_tibble(project_data)

  if (any(baseline_tbl[[fuel_col]] <= 0, na.rm = TRUE) || any(project_tbl[[project_fuel_col]] <= 0, na.rm = TRUE)) {
    stop("Fuel quantities must be positive for applicability checks.", call. = FALSE)
  }

  weighted_baseline <- sum(baseline_tbl[[fuel_col]] * baseline_tbl[[emission_factor_col]], na.rm = TRUE) /
    sum(baseline_tbl[[fuel_col]], na.rm = TRUE)
  weighted_project <- sum(project_tbl[[project_fuel_col]] * project_tbl[[project_emission_factor_col]], na.rm = TRUE) /
    sum(project_tbl[[project_fuel_col]], na.rm = TRUE)

  is.finite(weighted_baseline) && is.finite(weighted_project) && weighted_project <= weighted_baseline
}

#' Check AMS-II.D monitoring readiness
#'
#' AMS-II.D stipulates specific monitoring parameters, including records of fuel
#' consumption, equipment utilisation, and thermal output. This helper verifies
#' that required columns exist in a monitoring tibble and are free of missing
#' values.
#'
#' @param monitoring_data Tibble containing monitoring records.
#' @param required_cols Character vector of columns that must be present.
#' @return Logical scalar indicating whether the monitoring dataset meets the
#'   minimum completeness requirements.
#' @examples
#' monitoring <- tibble::tibble(
#'   unit = "Kiln",
#'   project_fuel_quantity = 80,
#'   project_efficiency = 0.84,
#'   useful_heat_output = 60
#' )
#' check_applicability_monitoring(monitoring, required_cols = c("project_fuel_quantity", "project_efficiency"))
#' @export
check_applicability_monitoring <- function(monitoring_data,
                                           required_cols = c(
                                             "project_fuel_quantity",
                                             "project_efficiency",
                                             "useful_heat_output"
                                           )) {
  data_tbl <- dplyr::as_tibble(monitoring_data)
  has_columns <- all(required_cols %in% names(data_tbl))
  if (!has_columns) {
    return(FALSE)
  }

  complete_cases <- data_tbl |>
    dplyr::select(dplyr::all_of(required_cols)) |>
    stats::complete.cases()

  all(complete_cases)
}

#' Assess overall AMS-II.D applicability
#'
#' Combines the individual applicability checks into a single tidy tibble of
#' diagnostics. The result includes boolean flags for each criterion and an
#' overall logical indicating whether all requirements are satisfied.
#'
#' @param baseline_data Baseline dataset used for applicability checks.
#' @param project_data Project dataset used for applicability checks.
#' @param monitoring_data Monitoring dataset used for completeness checks.
#' @param fuel_switch Logical indicating whether the project includes a fuel
#'   switching component. If `FALSE`, the fuel switching test is skipped.
#' @return A tibble with one row summarising applicability outcomes.
#' @examples
#' baseline <- tibble::tibble(
#'   baseline_fuel_quantity = c(1200, 900),
#'   baseline_efficiency = c(0.72, 0.68),
#'   baseline_emission_factor_tco2_per_gj = c(0.094, 0.094)
#' )
#' project <- tibble::tibble(
#'   project_fuel_quantity = c(950, 710),
#'   project_efficiency = c(0.84, 0.8),
#'   project_emission_factor_tco2_per_gj = c(0.082, 0.082)
#' )
#' monitoring <- tibble::tibble(
#'   unit = c("Kiln", "Dryer"),
#'   project_fuel_quantity = c(80, 70),
#'   project_efficiency = c(0.84, 0.8),
#'   useful_heat_output = c(60, 55)
#' )
#' assess_ams_iid_applicability(baseline, project, monitoring)
#' @export
assess_ams_iid_applicability <- function(baseline_data,
                                         project_data,
                                         monitoring_data,
                                         fuel_switch = TRUE) {
  energy_efficiency <- check_applicability_energy_efficiency(baseline_data, project_data)
  fuel_switching <- if (fuel_switch) {
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
