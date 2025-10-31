#' Simulate an AMS-I.E monitoring dataset
#'
#' Generates a tidy tibble emulating monitoring data for AMS-I.E projects. The
#' simulation includes user identifiers, monitoring periods, biomass
#' consumption, non-renewable fractions, emission factors, and project fossil
#' energy use.
#'
#' @param n_users Number of unique users or sites.
#' @param n_periods Number of monitoring periods per user.
#' @param start_year Starting calendar year for the monitoring periods.
#' @param seed Optional random seed for reproducibility.
#' @return Tibble with simulated monitoring observations.
#' @examples
#' simulate_ams_ie_dataset(n_users = 2, n_periods = 3)
#' @export
simulate_ams_ie_dataset <- function(n_users = 3,
                                    n_periods = 4,
                                    start_year = 2024,
                                    seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  users <- paste0("user-", seq_len(n_users))
  periods <- tibble::tibble(
    year = rep(start_year + seq_len(n_periods) - 1, each = n_users),
    month = rep(sample(1:12, n_periods, replace = TRUE), each = n_users)
  )

  baseline_consumption <- stats::rnorm(n_users * n_periods, mean = 12, sd = 2)
  baseline_consumption <- pmax(baseline_consumption, 6)

  non_renewable_fraction <- stats::rnorm(n_users * n_periods, mean = 0.85, sd = 0.05)
  non_renewable_fraction <- pmin(pmax(non_renewable_fraction, 0.5), 1)

  tibble::tibble(
    user_id = rep(users, times = n_periods),
    year = periods$year,
    month = periods$month,
    biomass_consumption_tonnes = baseline_consumption,
    non_renewable_fraction = non_renewable_fraction,
    net_calorific_value = stats::rnorm(n_users * n_periods, mean = 15, sd = 1),
    emission_factor = 0.0001,
    project_energy_mj = stats::rnorm(n_users * n_periods, mean = 150, sd = 40) |> pmax(0),
    project_emission_factor = 0.00009
  )
}
