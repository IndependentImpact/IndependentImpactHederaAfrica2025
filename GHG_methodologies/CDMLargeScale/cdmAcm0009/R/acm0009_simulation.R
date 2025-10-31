#' Simulate ACM0009 monitoring datasets
#'
#' Generates synthetic monitoring data for ACM0009 projects converting existing
#' coal or petroleum-fired plants to natural gas. The simulation draws fuel
#' consumption, net calorific values, emission factors, and methane slip within
#' realistic ranges observed in thermal power applications.
#'
#' @param periods Number of monitoring periods to simulate. Defaults to `12`.
#' @param seed Optional seed for reproducibility.
#'
#' @return Tibble with simulated monitoring inputs.
#' @examples
#' simulate_acm0009_dataset(periods = 6, seed = 123)
#' @export
simulate_acm0009_dataset <- function(periods = 12, seed = NULL) {
  if (!is.numeric(periods) || length(periods) != 1 || periods <= 0) {
    rlang::abort("`periods` must be a single positive numeric value.")
  }
  periods <- as.integer(periods)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  baseline_options <- c("bituminous coal", "sub-bituminous coal", "heavy fuel oil")

  tibble::tibble(
    period = seq_len(periods),
    baseline_fuel_type = sample(baseline_options, size = periods, replace = TRUE),
    project_fuel_type = rep("natural gas", periods),
    baseline_fuel_quantity = stats::runif(periods, min = 8000, max = 14000),
    baseline_ncv_tj_per_unit = stats::runif(periods, min = 0.022, max = 0.027),
    baseline_emission_factor_tco2_per_tj = stats::runif(periods, min = 90, max = 99),
    project_fuel_quantity = stats::runif(periods, min = 9000, max = 15000),
    project_ncv_tj_per_unit = stats::runif(periods, min = 0.035, max = 0.04),
    project_emission_factor_tco2_per_tj = stats::runif(periods, min = 53, max = 57),
    methane_slip_m3 = stats::runif(periods, min = 0, max = 400),
    additional_leakage_tco2e = stats::runif(periods, min = 0, max = 8)
  )
}
