#' Simulate ACM0018 monitoring data
#'
#' Generates synthetic monitoring data for biomass power plants covered by
#' ACM0018.
#'
#' @param n_periods Integer number of monitoring periods to simulate. Defaults
#'   to 12.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return Tibble containing simulated monitoring records.
#' @examples
#' simulate_acm0018_dataset(2, seed = 42)
#' @export
simulate_acm0018_dataset <- function(n_periods = 12, seed = NULL) {
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
  electricity_output <- stats::rnorm(n_periods, mean = 48000, sd = 3500)
  baseline_ef <- stats::rnorm(n_periods, mean = 0.82, sd = 0.05)
  auxiliary_fossil <- stats::rnorm(n_periods, mean = 0.18, sd = 0.04)
  onsite_generation <- stats::rnorm(n_periods, mean = 60, sd = 15)
  biomass_transport <- stats::rnorm(n_periods, mean = 520, sd = 110)
  leakage_fraction <- stats::rnorm(n_periods, mean = 0.04, sd = 0.01)

  tibble::tibble(
    period = sprintf("Period %02d", period),
    electricity_output_mwh = pmax(electricity_output, 0),
    baseline_emission_factor = pmax(baseline_ef, 0.5),
    auxiliary_fossil_tj = pmax(auxiliary_fossil, 0),
    auxiliary_fossil_ef = 74,
    onsite_generation_mwh = pmax(onsite_generation, 0),
    onsite_emission_factor = 0.7,
    biomass_transport_tkm = pmax(biomass_transport, 0),
    transport_emission_factor = 0.00012,
    leakage_fraction = pmin(pmax(leakage_fraction, 0), 0.1)
  )
}
