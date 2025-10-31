#' Simulate monitoring data for ACM0008
#'
#' Creates a tidy dataset representing coal mine methane recovery operations.
#' The simulation generates methane flow, concentration, oxidation efficiency, and
#' auxiliary electricity use across several monitoring periods. Random draws are
#' reproducible via the `seed` argument and leverage `DeclareDesign`-style design
#' principles using the tidyverse toolchain.
#'
#' @param periods Number of monitoring periods to simulate.
#' @param observations_per_period Number of observations to create for each
#'   monitoring period.
#' @param seed Optional integer used to initialise the random number generator.
#'
#' @return Tibble containing simulated monitoring records with the columns used
#'   by the ACM0008 calculation helpers.
#'
#' @examples
#' simulate_acm0008_dataset(2, observations_per_period = 10, seed = 123)
#' @export
simulate_acm0008_dataset <- function(periods = 6,
                                     observations_per_period = 30,
                                     seed = NULL) {
  if (!is.numeric(periods) || length(periods) != 1 || periods <= 0) {
    rlang::abort("`periods` must be a positive numeric value.")
  }
  if (!is.numeric(observations_per_period) ||
      length(observations_per_period) != 1 ||
      observations_per_period <= 0) {
    rlang::abort("`observations_per_period` must be a positive numeric value.")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      rlang::abort("`seed` must be a single numeric value if provided.")
    }
    set.seed(seed)
  }

  periods <- as.integer(periods)
  observations_per_period <- as.integer(observations_per_period)

  start_date <- as.Date("2025-01-01")
  period_starts <- seq(start_date, by = "month", length.out = periods)

  purrr::imap_dfr(period_starts, function(start, idx) {
    tibble::tibble(
      period = sprintf("P%02d", idx),
      year = as.integer(format(start, "%Y")),
      month = as.integer(format(start, "%m")),
      day = sample(1:28, observations_per_period, replace = TRUE),
      observation = seq_len(observations_per_period),
      flow_rate_m3_per_h = pmax(stats::rnorm(observations_per_period, 520, 55), 300),
      operating_hours = rep(720 / observations_per_period, observations_per_period),
      methane_fraction = stats::runif(observations_per_period, 0.32, 0.45),
      oxidation_efficiency = stats::runif(observations_per_period, 0.92, 0.99),
      electricity_use_mwh = pmax(stats::rnorm(observations_per_period, 4.5, 0.6), 2.5),
      grid_emission_factor_t_per_mwh = stats::runif(observations_per_period, 0.58, 0.75)
    )
  })
}
