#' Simulate monitoring data for ACM0013
#'
#' Generates a synthetic monitoring dataset reflecting grid-connected fossil fuel
#' power projects deploying less greenhouse gas intensive technologies under
#' ACM0013.
#'
#' @param periods Number of monitoring periods to simulate. Defaults to `12`.
#' @param seed Optional seed for reproducibility.
#' @return A tibble with simulated monitoring inputs for ACM0013 workflows.
#' @examples
#' simulate_acm0013_dataset(periods = 6, seed = 100)
#' @export
simulate_acm0013_dataset <- function(periods = 12, seed = NULL) {
  if (!is.numeric(periods) || length(periods) != 1 || periods <= 0) {
    rlang::abort("`periods` must be a single positive numeric value.")
  }
  periods <- as.integer(periods)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  baseline_emission_factor <- stats::runif(periods, min = 0.8, max = 1.0)
  technology_emission_factor <- baseline_emission_factor * stats::runif(periods, min = 0.65, max = 0.85)

  tibble::tibble(
    period = seq_len(periods),
    is_new_plant = TRUE,
    grid_connected = TRUE,
    electricity_sent_out_mwh = stats::runif(periods, min = 35000, max = 55000),
    baseline_emission_factor_tco2_per_mwh = baseline_emission_factor,
    technology_emission_factor_tco2_per_mwh = technology_emission_factor,
    fuel_consumed = stats::runif(periods, min = 6.5e7, max = 8.5e7),
    fuel_emission_factor_tco2_per_unit = stats::runif(periods, min = 7.0e-5, max = 8.0e-5),
    auxiliary_electricity_mwh = stats::runif(periods, min = 400, max = 800),
    auxiliary_emission_factor_tco2_per_mwh = stats::runif(periods, min = 0.6, max = 0.9),
    other_project_emissions_tco2e = stats::runif(periods, min = 0, max = 50),
    upstream_fuel_emissions_tco2e = stats::runif(periods, min = 100, max = 250),
    displaced_unit_emissions_tco2e = stats::runif(periods, min = 50, max = 120),
    other_leakage_tco2e = stats::runif(periods, min = 0, max = 20)
  )
}
