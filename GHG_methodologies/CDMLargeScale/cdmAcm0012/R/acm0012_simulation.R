#' Simulate ACM0012 monitoring data
#'
#' Generates synthetic monitoring records for waste energy recovery projects
#' covered by ACM0012 using base R random generation. The simulation produces inputs for
#' baseline, project, leakage, and applicability helper functions.
#'
#' @param n_periods Integer number of monitoring periods to simulate. Defaults
#'   to 12.
#' @param observations_per_period Integer number of observations per period.
#'   Defaults to 30.
#' @param seed Optional integer seed for reproducibility.
#' @return Tibble containing simulated monitoring records.
#' @examples
#' simulate_acm0012_dataset(2, observations_per_period = 5, seed = 2024)
#' @export
simulate_acm0012_dataset <- function(n_periods = 12,
                                     observations_per_period = 30,
                                     seed = NULL) {
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    rlang::abort("`n_periods` must be a positive integer.")
  }
  if (!is.numeric(observations_per_period) || length(observations_per_period) != 1 ||
      observations_per_period <= 0) {
    rlang::abort("`observations_per_period` must be a positive integer.")
  }
  n_periods <- as.integer(n_periods)
  observations_per_period <- as.integer(observations_per_period)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      rlang::abort("`seed` must be a single numeric value.")
    }
    set.seed(as.integer(seed))
  }

  N <- n_periods * observations_per_period

  monitoring_data <- tibble::tibble(
    period = rep(sprintf("Period %02d", seq_len(n_periods)), each = observations_per_period),
    electricity_export_mwh = pmax(stats::rnorm(N, mean = 85, sd = 12), 0),
    baseline_grid_ef_t_per_mwh = pmax(stats::rnorm(N, mean = 0.82, sd = 0.04), 0.6),
    thermal_export_gj = pmax(stats::rnorm(N, mean = 140, sd = 25), 0),
    baseline_thermal_ef_t_per_gj = pmax(stats::rnorm(N, mean = 0.055, sd = 0.004), 0.04),
    flare_gas_displacement_emissions = pmax(stats::rnorm(N, mean = 1.5, sd = 0.4), 0),
    electricity_import_mwh = pmax(stats::rnorm(N, mean = 6.5, sd = 1.2), 0),
    project_grid_ef_t_per_mwh = pmax(stats::rnorm(N, mean = 0.75, sd = 0.03), 0.6),
    auxiliary_fuel_tj = pmax(stats::rnorm(N, mean = 0.08, sd = 0.015), 0),
    auxiliary_ef_t_per_tj = pmax(stats::rnorm(N, mean = 56, sd = 1.5), 40),
    methane_leakage_nm3 = pmax(stats::rnorm(N, mean = 45, sd = 8), 0),
    methane_density_t_per_nm3 = 0.000716,
    gwp_ch4 = 28,
    leakage_energy_mwh = pmax(stats::rnorm(N, mean = 4.5, sd = 0.8), 0),
    leakage_ef_t_per_mwh = pmax(stats::rnorm(N, mean = 0.08, sd = 0.01), 0),
    waste_energy_fraction = stats::runif(N, min = 0.7, max = 0.95),
    measurement_uncertainty = stats::runif(N, min = 0.005, max = 0.02),
    baseline_operating_hours = pmax(stats::rnorm(N, mean = 7800, sd = 200), 7000),
    project_operating_hours = pmax(stats::rnorm(N, mean = 7600, sd = 180), 7000)
  ) |>
    dplyr::mutate(
      waste_energy_fraction = pmin(pmax(waste_energy_fraction, 0), 1),
      measurement_uncertainty = pmin(pmax(measurement_uncertainty, 0), 0.05)
    )

  monitoring_data
}
