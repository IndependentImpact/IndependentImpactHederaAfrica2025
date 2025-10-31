#' Simulate monitoring data for AMS-II.G
#'
#' Generates synthetic monitoring observations for household or institutional
#' cookstove programmes that improve thermal efficiency while continuing to use
#' biomass fuels. The simulation provides baseline and project biomass
#' consumption, non-renewable fractions, net calorific values, emission factors,
#' leakage placeholders, and basic monitoring metadata.
#'
#' @param n_sites Number of project sites or households to simulate.
#' @param n_periods Number of monitoring periods per site.
#' @param start_year Starting calendar year for the monitoring periods.
#' @param start_month Starting month for the monitoring periods (1-12).
#' @param seed Optional integer seed for reproducibility.
#' @param baseline_consumption_mean Mean baseline biomass consumption per period
#'   (tonnes).
#' @param efficiency_gain Expected fractional reduction in biomass consumption due
#'   to efficiency improvements (0-1).
#' @param baseline_fraction_mean Mean baseline non-renewable fraction.
#' @param project_fraction_mean Mean project non-renewable fraction.
#' @return Tibble containing monitoring metadata and the columns required by the
#'   AMS-II.G workflow.
#' @examples
#' simulate_ams_iig_dataset(n_sites = 2, n_periods = 3, seed = 123)
#' @export
simulate_ams_iig_dataset <- function(n_sites = 5,
                                     n_periods = 4,
                                     start_year = 2024,
                                     start_month = 1,
                                     seed = NULL,
                                     baseline_consumption_mean = 14,
                                     efficiency_gain = 0.45,
                                     baseline_fraction_mean = 0.85,
                                     project_fraction_mean = 0.4) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.numeric(n_sites) || length(n_sites) != 1 || n_sites <= 0) {
    stop("`n_sites` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(baseline_consumption_mean) || baseline_consumption_mean <= 0) {
    stop("`baseline_consumption_mean` must be positive.", call. = FALSE)
  }
  if (!is.numeric(efficiency_gain) || efficiency_gain <= 0 || efficiency_gain >= 1) {
    stop("`efficiency_gain` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(baseline_fraction_mean) || baseline_fraction_mean <= 0 || baseline_fraction_mean > 1) {
    stop("`baseline_fraction_mean` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(project_fraction_mean) || project_fraction_mean < 0 || project_fraction_mean > 1) {
    stop("`project_fraction_mean` must be between 0 and 1.", call. = FALSE)
  }

  site_ids <- sprintf("site_%02d", seq_len(n_sites))
  periods <- expand.grid(
    site_id = site_ids,
    period = seq_len(n_periods),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  periods <- periods[order(periods$site_id, periods$period), , drop = FALSE]

  offset <- periods$period - 1
  periods$year <- start_year + ((start_month - 1 + offset) %/% 12)
  periods$month <- ((start_month - 1 + offset) %% 12) + 1
  periods$day <- sample.int(28, size = nrow(periods), replace = TRUE)
  periods$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", periods$year, periods$month, periods$day))
  periods$monitoring_label <- sprintf("%04d-%02d", periods$year, periods$month)

  baseline_consumption <- stats::rnorm(nrow(periods), mean = baseline_consumption_mean, sd = baseline_consumption_mean * 0.15)
  baseline_consumption <- pmax(baseline_consumption, 0.5)

  project_consumption <- baseline_consumption * (1 - efficiency_gain)

  baseline_fraction <- stats::rnorm(nrow(periods), mean = baseline_fraction_mean, sd = 0.04)
  baseline_fraction <- pmin(pmax(baseline_fraction, 0.4), 1)

  project_fraction <- stats::rnorm(nrow(periods), mean = project_fraction_mean, sd = 0.05)
  project_fraction <- pmin(pmax(project_fraction, 0), baseline_fraction)

  baseline_ncv <- stats::rnorm(nrow(periods), mean = 15.0, sd = 0.6)
  project_ncv <- baseline_ncv + stats::rnorm(nrow(periods), mean = 0.2, sd = 0.15)

  baseline_emission_factor <- stats::runif(nrow(periods), min = 0.000085, max = 0.000095)
  project_emission_factor <- baseline_emission_factor * stats::runif(nrow(periods), min = 0.95, max = 1.05)

  leakage <- baseline_consumption * baseline_fraction * 0.01

  tibble::as_tibble(periods) |>
    dplyr::mutate(
      baseline_biomass_consumption_tonnes = baseline_consumption,
      baseline_non_renewable_fraction = baseline_fraction,
      baseline_net_calorific_value_mj_per_tonne = baseline_ncv,
      baseline_emission_factor_tco2_per_mj = baseline_emission_factor,
      project_biomass_consumption_tonnes = project_consumption,
      project_non_renewable_fraction = project_fraction,
      project_net_calorific_value_mj_per_tonne = project_ncv,
      project_emission_factor_tco2_per_mj = project_emission_factor,
      leakage_emissions_tco2e = leakage,
      service_level_indicator = stats::rnorm(nrow(periods), mean = 1, sd = 0.1)
    )
}
