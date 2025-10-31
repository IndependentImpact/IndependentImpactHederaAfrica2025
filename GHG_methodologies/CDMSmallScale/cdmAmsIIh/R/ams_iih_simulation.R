#' Simulate monitoring data for AMS-II.H
#'
#' Generates synthetic baseline and project monitoring datasets for industrial
#' facilities that centralize the provision of utilities such as steam, hot
#' water, or chilled water. The simulation includes fuel use, emission factors,
#' useful energy delivered, auxiliary electricity, and leakage placeholders so
#' that the workflow in [estimate_emission_reductions_ams_iih()] can be executed
#' end-to-end.
#'
#' @param n_facilities Number of industrial facilities to simulate.
#' @param seed Optional integer seed for reproducibility.
#' @param baseline_fuel_mean Mean baseline fuel use per facility (GJ).
#' @param central_efficiency_gain Expected fractional reduction in specific
#'   energy consumption delivered by the centralized system (0-1).
#' @param electricity_intensity Project auxiliary electricity intensity in MWh
#'   per unit of useful output (GJ).
#' @param leakage_fraction Fraction of baseline fuel use that should be
#'   represented as leakage emissions.
#' @return A list containing three tibbles: `baseline_data`, `project_data`, and
#'   `leakage_data`.
#' @examples
#' simulate_ams_iih_inputs(n_facilities = 2, seed = 42)
#' @export
simulate_ams_iih_inputs <- function(n_facilities = 4,
                                    seed = NULL,
                                    baseline_fuel_mean = 4200,
                                    central_efficiency_gain = 0.35,
                                    electricity_intensity = 0.045,
                                    leakage_fraction = 0.02) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.numeric(n_facilities) || length(n_facilities) != 1 || n_facilities <= 0) {
    stop("`n_facilities` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(baseline_fuel_mean) || baseline_fuel_mean <= 0) {
    stop("`baseline_fuel_mean` must be positive.", call. = FALSE)
  }
  if (!is.numeric(central_efficiency_gain) || central_efficiency_gain <= 0 || central_efficiency_gain >= 1) {
    stop("`central_efficiency_gain` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(electricity_intensity) || electricity_intensity < 0) {
    stop("`electricity_intensity` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(leakage_fraction) || leakage_fraction < 0) {
    stop("`leakage_fraction` must be non-negative.", call. = FALSE)
  }

  facilities <- tibble::tibble(
    facility = sprintf("facility_%02d", seq_len(n_facilities)),
    service_growth = stats::rnorm(n_facilities, mean = 0.03, sd = 0.01)
  )

  baseline_fuel <- stats::rnorm(n_facilities, mean = baseline_fuel_mean, sd = baseline_fuel_mean * 0.12)
  baseline_fuel <- pmax(baseline_fuel, baseline_fuel_mean * 0.4)
  baseline_specific <- stats::rnorm(n_facilities, mean = 1.15, sd = 0.08)
  baseline_output <- baseline_fuel / baseline_specific
  baseline_emission_factor <- stats::runif(n_facilities, min = 0.066, max = 0.074)

  project_specific <- baseline_specific * (1 - central_efficiency_gain)
  project_output <- baseline_output * (1 + facilities$service_growth)
  project_fuel <- project_output * project_specific
  project_emission_factor <- baseline_emission_factor * stats::runif(n_facilities, min = 0.95, max = 1.02)
  project_electricity <- project_output * electricity_intensity

  baseline_data <- facilities |>
    dplyr::mutate(
      baseline_fuel_use_gj = baseline_fuel,
      baseline_emission_factor_tco2_per_gj = baseline_emission_factor,
      baseline_useful_output_gj = baseline_output,
      baseline_unit_count = sample(3:5, size = n_facilities, replace = TRUE)
    )

  project_data <- facilities |>
    dplyr::mutate(
      project_fuel_use_gj = project_fuel,
      project_emission_factor_tco2_per_gj = project_emission_factor,
      project_useful_output_gj = project_output,
      project_auxiliary_electricity_mwh = project_electricity,
      project_electricity_emission_factor_tco2_per_mwh = stats::runif(n_facilities, min = 0.45, max = 0.62),
      project_unit_count = 1L
    )

  leakage_data <- facilities |>
    dplyr::mutate(
      leakage_emissions_tco2e = baseline_fuel * baseline_emission_factor * leakage_fraction
    )

  list(
    baseline_data = baseline_data,
    project_data = project_data,
    leakage_data = leakage_data
  )
}
