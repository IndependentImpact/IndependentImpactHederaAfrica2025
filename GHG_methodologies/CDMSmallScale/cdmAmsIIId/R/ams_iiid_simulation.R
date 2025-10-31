#' Simulate monitoring data for AMS-III.D methane recovery projects
#'
#' Generates a tidy tibble representing farms equipped with anaerobic digesters
#' that recover methane from animal manure. The simulation emits columns required
#' by the AMS-III.D workflow, including baseline and project methane parameters,
#' applicability indicators, and leakage placeholders.
#'
#' @param n_farms Number of farms or digesters to simulate.
#' @param n_periods Number of monitoring periods per farm.
#' @param start_year Calendar year of the first monitoring period.
#' @param seed Optional integer seed for reproducibility.
#' @param vs_mean Mean volatile solids generation per day (kg).
#' @param methane_potential_mean Mean methane potential (m3 CH4/kg VS).
#' @param baseline_mcf_mean Mean baseline methane conversion factor (fraction).
#' @param project_mcf_mean Mean project methane conversion factor (fraction).
#' @return Tibble containing simulated monitoring data compatible with the
#'   package helpers.
#' @examples
#' simulate_ams_iiid_dataset(n_farms = 2, n_periods = 3, seed = 101)
#' @export
simulate_ams_iiid_dataset <- function(n_farms = 5,
                                      n_periods = 6,
                                      start_year = 2024,
                                      seed = NULL,
                                      vs_mean = 35,
                                      methane_potential_mean = 0.24,
                                      baseline_mcf_mean = 0.8,
                                      project_mcf_mean = 0.6) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (n_farms <= 0 || n_periods <= 0) {
    stop("`n_farms` and `n_periods` must be positive integers.", call. = FALSE)
  }

  farms <- sprintf("farm_%02d", seq_len(n_farms))
  periods <- expand.grid(
    farm_id = farms,
    monitoring_period = seq_len(n_periods),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  periods <- periods[order(periods$farm_id, periods$monitoring_period), , drop = FALSE]

  periods$year <- start_year + (periods$monitoring_period - 1) %/% 12
  periods$month <- ((periods$monitoring_period - 1) %% 12) + 1
  periods$days_in_period <- 30

  volatile_solids <- stats::rnorm(nrow(periods), mean = vs_mean, sd = vs_mean * 0.15)
  volatile_solids <- pmax(volatile_solids, vs_mean * 0.4)

  methane_potential <- stats::rnorm(nrow(periods), mean = methane_potential_mean, sd = methane_potential_mean * 0.05)
  methane_potential <- pmax(methane_potential, methane_potential_mean * 0.8)

  baseline_mcf <- stats::rnorm(nrow(periods), mean = baseline_mcf_mean, sd = 0.05)
  baseline_mcf <- pmin(pmax(baseline_mcf, 0.6), 0.95)

  project_mcf <- stats::rnorm(nrow(periods), mean = project_mcf_mean, sd = 0.04)
  project_mcf <- pmin(pmax(project_mcf, 0.4), 0.8)

  capture_efficiency <- stats::runif(nrow(periods), min = 0.8, max = 0.95)
  destruction_efficiency <- stats::runif(nrow(periods), min = 0.95, max = 0.99)

  methane_generated_baseline <- volatile_solids * methane_potential * baseline_mcf * periods$days_in_period
  methane_generated_project <- volatile_solids * methane_potential * project_mcf * periods$days_in_period
  methane_captured <- methane_generated_project * capture_efficiency
  methane_destroyed <- methane_captured * destruction_efficiency
  methane_recovered_m3 <- methane_destroyed / (0.00067 * 28)

  leakage <- methane_generated_project * 0.01 * 0.00067 * 28

  system_types <- c("anaerobic lagoon", "uncovered digester", "liquid slurry store")
  controls_pool <- c(TRUE, TRUE, TRUE, FALSE)

  tibble::as_tibble(periods) |>
    dplyr::mutate(
      volatile_solids_kg_per_day = volatile_solids,
      methane_potential_m3_per_kg_vs = methane_potential,
      baseline_mcf_fraction = baseline_mcf,
      project_mcf_fraction = project_mcf,
      capture_efficiency_fraction = capture_efficiency,
      destruction_efficiency_fraction = destruction_efficiency,
      methane_recovered_m3 = methane_recovered_m3,
      leakage_emissions_tco2e = leakage,
      system_type = sample(system_types, size = nrow(periods), replace = TRUE),
      measurements_per_month = sample(4:12, size = nrow(periods), replace = TRUE),
      leakage_controls_in_place = sample(controls_pool, size = nrow(periods), replace = TRUE, prob = c(0.35, 0.3, 0.25, 0.1))
    )
}
