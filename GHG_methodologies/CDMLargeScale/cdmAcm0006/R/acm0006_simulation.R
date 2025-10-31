#' Simulate ACM0006 monitoring data
#'
#' Generates synthetic monitoring data for biomass-based combined heat and power
#' systems addressed by ACM0006.
#'
#' @param n_periods Integer number of monitoring periods to simulate. Defaults
#'   to 12.
#' @param seed Optional integer seed for reproducibility.
#' @return Tibble containing simulated monitoring records.
#' @examples
#' simulate_acm0006_dataset(2, seed = 42)
#' @export
simulate_acm0006_dataset <- function(n_periods = 12, seed = NULL) {
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
  heat_output <- stats::rnorm(n_periods, mean = 8, sd = 1)
  electricity_output <- stats::rnorm(n_periods, mean = 400, sd = 60)
  baseline_heat_ef <- stats::rnorm(n_periods, mean = 80, sd = 5)
  baseline_electricity_ef <- stats::rnorm(n_periods, mean = 0.7, sd = 0.05)
  auxiliary_fossil <- stats::rnorm(n_periods, mean = 0.15, sd = 0.03)
  electricity_imports <- stats::rnorm(n_periods, mean = 25, sd = 8)
  biomass_transport <- stats::rnorm(n_periods, mean = 450, sd = 90)
  leakage_fraction <- stats::rnorm(n_periods, mean = 0.03, sd = 0.01)

  tibble::tibble(
    period = sprintf("Period %02d", period),
    heat_output_tj = pmax(heat_output, 0),
    electricity_output_mwh = pmax(electricity_output, 0),
    baseline_heat_ef = pmax(baseline_heat_ef, 60),
    baseline_electricity_ef = pmax(baseline_electricity_ef, 0.4),
    auxiliary_fossil_tj = pmax(auxiliary_fossil, 0),
    auxiliary_fossil_ef = 74,
    electricity_import_mwh = pmax(electricity_imports, 0),
    import_emission_factor = 0.7,
    biomass_transport_tkm = pmax(biomass_transport, 0),
    transport_emission_factor = 0.0001,
    leakage_fraction = pmin(pmax(leakage_fraction, 0), 0.1)
  )
}
