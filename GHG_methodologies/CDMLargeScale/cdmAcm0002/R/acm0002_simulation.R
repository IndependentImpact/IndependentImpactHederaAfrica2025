#' Simulate ACM0002 monitoring data
#'
#' Generates synthetic monitoring data consistent with ACM0002 requirements,
#' including renewable generation, auxiliary consumption, fossil back-up use,
#' and grid emission factors.
#'
#' @param n_periods Integer number of monitoring periods to simulate. Defaults
#'   to 12.
#' @param seed Optional integer seed for reproducibility.
#' @return Tibble containing simulated monitoring records.
#' @examples
#' simulate_acm0002_dataset(2, seed = 42)
#' @export
simulate_acm0002_dataset <- function(n_periods = 12, seed = NULL) {
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
  gross <- stats::rnorm(n_periods, mean = 15000, sd = 1200)
  auxiliary <- stats::rnorm(n_periods, mean = 600, sd = 80)
  combined_margin <- stats::rnorm(n_periods, mean = 0.75, sd = 0.05)
  fossil_use <- stats::rnorm(n_periods, mean = 0.05, sd = 0.01)
  electricity_imports <- stats::rnorm(n_periods, mean = 30, sd = 10)

  tibble::tibble(
    period = sprintf("Period %02d", period),
    gross_generation_mwh = pmax(gross, 0),
    auxiliary_consumption_mwh = pmax(auxiliary, 0),
    combined_margin_ef = pmax(combined_margin, 0.4),
    fossil_fuel_tj = pmax(fossil_use, 0),
    fossil_emission_factor = 74,
    electricity_import_mwh = pmax(electricity_imports, 0),
    import_emission_factor = 0.75,
    leakage_emissions = 0
  )
}
