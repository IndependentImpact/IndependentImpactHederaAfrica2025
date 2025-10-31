#' Simulate monitoring data for AMS-III.A projects
#'
#' Generates a tidy tibble representing farms that replace synthetic nitrogen
#' fertilizer with rhizobium inoculants in legume cultivation. The simulation
#' yields baseline fertilizer use, project-period residual fertilizer,
#' inoculant application rates, and leakage placeholders along with monitoring
#' metadata suitable for the AMS-III.A workflow.
#'
#' @param n_farms Number of participating farms to simulate.
#' @param n_periods Number of monitoring periods per farm (e.g. seasons).
#' @param start_year Calendar year of the first monitoring period.
#' @param seed Optional integer seed for reproducibility.
#' @param baseline_n_mean Mean baseline synthetic nitrogen use per period (kg N).
#' @param replacement_fraction Expected fractional reduction in fertilizer use
#'   attributable to inoculants (0-1).
#' @param legume_area_mean Mean legume area planted per farm (hectares).
#' @param inoculant_rate_mean Mean inoculant application rate (kg/ha).
#' @param fertilizer_production_ef Upstream emission factor for fertilizer
#'   production (tCO2e per kg N).
#' @param fertilizer_field_ef Direct soil emission factor (tCO2e per kg N).
#' @param inoculant_ef Emission factor for inoculant production and application
#'   (tCO2e per kg inoculant).
#' @return Tibble containing monitoring metadata and columns required by the
#'   AMS-III.A workflow.
#' @examples
#' simulate_ams_iiia_dataset(n_farms = 3, n_periods = 2, seed = 42)
#' @export
simulate_ams_iiia_dataset <- function(n_farms = 10,
                                      n_periods = 3,
                                      start_year = 2024,
                                      seed = NULL,
                                      baseline_n_mean = 90,
                                      replacement_fraction = 0.7,
                                      legume_area_mean = 22,
                                      inoculant_rate_mean = 0.45,
                                      fertilizer_production_ef = 0.004,
                                      fertilizer_field_ef = 0.01,
                                      inoculant_ef = 0.002) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (n_farms <= 0 || n_periods <= 0) {
    stop("`n_farms` and `n_periods` must be positive integers.", call. = FALSE)
  }
  if (replacement_fraction <= 0 || replacement_fraction >= 1) {
    stop("`replacement_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  farms <- sprintf("farm_%02d", seq_len(n_farms))
  periods <- expand.grid(
    farm_id = farms,
    monitoring_period = seq_len(n_periods),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  periods <- periods[order(periods$farm_id, periods$monitoring_period), , drop = FALSE]

  periods$year <- start_year + (periods$monitoring_period - 1) %/% 2
  periods$season <- ifelse(periods$monitoring_period %% 2 == 1, "wet", "dry")

  baseline_n <- stats::rnorm(nrow(periods), mean = baseline_n_mean, sd = baseline_n_mean * 0.2)
  baseline_n <- pmax(baseline_n, baseline_n_mean * 0.3)

  residual_fraction <- stats::rnorm(nrow(periods), mean = 1 - replacement_fraction, sd = 0.05)
  residual_fraction <- pmin(pmax(residual_fraction, 0.05), 0.5)
  project_n <- baseline_n * residual_fraction

  legume_area <- stats::rnorm(nrow(periods), mean = legume_area_mean, sd = legume_area_mean * 0.15)
  legume_area <- pmax(legume_area, legume_area_mean * 0.4)
  total_area <- legume_area / stats::runif(nrow(periods), min = 0.4, max = 0.8)

  inoculant_rate <- stats::rnorm(nrow(periods), mean = inoculant_rate_mean, sd = inoculant_rate_mean * 0.1)
  inoculant_rate <- pmax(inoculant_rate, inoculant_rate_mean * 0.5)

  leakage <- project_n * 0.01

  tibble::as_tibble(periods) |>
    dplyr::mutate(
      baseline_synthetic_n_kg = baseline_n,
      baseline_production_ef_tco2_per_kg = fertilizer_production_ef,
      baseline_field_ef_tco2_per_kg = fertilizer_field_ef,
      project_synthetic_n_kg = project_n,
      project_production_ef_tco2_per_kg = fertilizer_production_ef,
      project_field_ef_tco2_per_kg = fertilizer_field_ef,
      inoculant_rate_kg_per_ha = inoculant_rate,
      legume_area_ha = legume_area,
      total_area_ha = total_area,
      inoculant_ef_tco2_per_kg = inoculant_ef,
      inoculant_registered = sample(c(TRUE, TRUE, TRUE, FALSE), size = nrow(periods), replace = TRUE),
      leakage_emissions_tco2e = leakage
    )
}
