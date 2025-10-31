#' Simulate ACM0001 monitoring data
#'
#' Generates synthetic landfill gas monitoring data consistent with ACM0001
#' inputs, including methane generation, capture, destruction efficiency, and
#' auxiliary energy use.
#'
#' @param n_periods Integer number of monitoring periods to simulate. Defaults
#'   to 12.
#' @param seed Optional integer seed for reproducibility.
#' @return Tibble containing simulated monitoring records.
#' @examples
#' simulate_acm0001_dataset(2, seed = 42)
#' @export
simulate_acm0001_dataset <- function(n_periods = 12, seed = NULL) {
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    rlang::abort("`n_periods` must be a positive integer.")
  }
  n_periods <- as.integer(n_periods)
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      rlang::abort("`seed` must be a single numeric value.")
    }
    set.seed(as.integer(seed))
  }

  period <- seq_len(n_periods)
  methane_generation <- stats::rnorm(n_periods, mean = 4500, sd = 500)
  capture_eff <- stats::rnorm(n_periods, mean = 0.25, sd = 0.05)
  destruction_eff <- stats::rnorm(n_periods, mean = 0.9, sd = 0.03)
  auxiliary_fuel <- stats::rnorm(n_periods, mean = 0.02, sd = 0.005)
  electricity_imports <- stats::rnorm(n_periods, mean = 15, sd = 5)
  leakage_fraction <- stats::rnorm(n_periods, mean = 0.03, sd = 0.01)
  oxidation_fraction <- stats::rnorm(n_periods, mean = 0.05, sd = 0.02)

  tibble::tibble(
    period = sprintf("Period %02d", period),
    methane_generation_m3 = pmax(methane_generation, 0),
    baseline_capture_efficiency = pmin(pmax(stats::runif(n_periods, 0, 0.1), 0), 1),
    methane_captured_m3 = pmax(methane_generation * pmin(pmax(capture_eff, 0), 1), 0),
    destruction_efficiency = pmin(pmax(destruction_eff, 0), 1),
    auxiliary_fuel_tj = pmax(auxiliary_fuel, 0),
    auxiliary_ef_t_per_tj = 74,
    electricity_import_mwh = pmax(electricity_imports, 0),
    import_ef_t_per_mwh = 0.75,
    leakage_fraction = pmin(pmax(leakage_fraction, 0), 0.2),
    oxidation_fraction = pmin(pmax(oxidation_fraction, 0), 0.5),
    methane_density_t_per_m3 = 0.000716,
    gwp_ch4 = 28
  )
}
