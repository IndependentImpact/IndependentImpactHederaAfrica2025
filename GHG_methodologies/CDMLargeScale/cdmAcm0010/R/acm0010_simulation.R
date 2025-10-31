#' Simulate monitoring data for ACM0010
#'
#' Generates a synthetic monitoring dataset reflecting anaerobic digestion
#' projects that treat manure or high-strength wastewater streams under ACM0010.
#'
#' @param periods Number of monitoring periods to simulate. Defaults to `12`.
#' @param seed Optional seed for reproducibility.
#' @return A tibble with simulated monitoring inputs for ACM0010 workflows.
#' @examples
#' simulate_acm0010_dataset(periods = 6, seed = 100)
#' @export
simulate_acm0010_dataset <- function(periods = 12, seed = NULL) {
  if (!is.numeric(periods) || length(periods) != 1 || periods <= 0) {
    rlang::abort("`periods` must be a single positive numeric value.")
  }
  periods <- as.integer(periods)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  tibble::tibble(
    period = seq_len(periods),
    baseline_system = sample(
      c("open lagoon", "anaerobic pond", "manure pile"),
      size = periods,
      replace = TRUE
    ),
    cod_in_mg_l = stats::runif(periods, min = 3000, max = 6000),
    cod_out_mg_l = cod_in_mg_l * stats::runif(periods, min = 0.1, max = 0.3),
    flow_m3 = stats::runif(periods, min = 150, max = 400),
    methane_conversion_factor = stats::runif(periods, min = 0.6, max = 0.9),
    methane_recovered_m3 = stats::runif(periods, min = 800, max = 1600),
    combustion_efficiency = stats::runif(periods, min = 0.92, max = 0.99),
    electricity_consumed_mwh = stats::runif(periods, min = 10, max = 25),
    grid_emission_factor = stats::runif(periods, min = 0.5, max = 0.9),
    additional_leakage_tco2e = stats::runif(periods, min = 0, max = 5)
  )
}
