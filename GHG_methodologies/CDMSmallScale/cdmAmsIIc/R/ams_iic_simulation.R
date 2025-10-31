#' Simulate a dataset compliant with AMS-II.C
#'
#' Generates tidy monitoring data for demand-side energy efficiency projects
#' covered by AMS-II.C. The simulation produces baseline energy, project energy,
#' and emission factors for multiple sites across monitoring periods.
#'
#' @param n_sites Number of project sites to simulate.
#' @param n_periods Number of monitoring periods per site.
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param mean_baseline_mwh Mean annual baseline energy consumption per site in MWh.
#' @param sd_baseline_mwh Standard deviation of baseline energy consumption in MWh.
#' @param savings_fraction Expected fractional reduction in energy use from the
#'   project technology (0-1).
#' @param emission_factor_tco2e_mwh Emission factor applied to the energy savings.
#' @return A tibble with site identifiers, monitoring metadata, baseline and project
#'   energy, emission factors, and technology labels.
#' @examples
#' simulate_ams_iic_dataset(n_sites = 4)
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @export
simulate_ams_iic_dataset <- function(n_sites = 10,
                                     n_periods = 12,
                                     start_year = 2023,
                                     start_month = 1,
                                     mean_baseline_mwh = 1800,
                                     sd_baseline_mwh = 260,
                                     savings_fraction = 0.35,
                                     emission_factor_tco2e_mwh = 0.68) {
  if (!is.numeric(n_sites) || length(n_sites) != 1 || n_sites <= 0) {
    stop("`n_sites` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(mean_baseline_mwh) || mean_baseline_mwh <= 0) {
    stop("`mean_baseline_mwh` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(sd_baseline_mwh) || sd_baseline_mwh < 0) {
    stop("`sd_baseline_mwh` must be a non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(savings_fraction) || length(savings_fraction) != 1 ||
      savings_fraction <= 0 || savings_fraction >= 1) {
    stop("`savings_fraction` must be a numeric value between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(emission_factor_tco2e_mwh) || length(emission_factor_tco2e_mwh) != 1 ||
      emission_factor_tco2e_mwh < 0) {
    stop("`emission_factor_tco2e_mwh` must be a non-negative numeric value.", call. = FALSE)
  }

  site_ids <- sprintf("site_%02d", seq_len(n_sites))
  period_index <- seq_len(n_periods)

  grid <- expand.grid(
    site_id = site_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$site_id, grid$monitoring_period), , drop = FALSE]

  month_offset <- grid$monitoring_period - 1L
  grid$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  grid$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  grid$day <- sample.int(28L, size = nrow(grid), replace = TRUE)
  grid$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", grid$year, grid$month, grid$day))
  grid$monitoring_label <- sprintf("%04d-%02d", grid$year, grid$month)

  baseline_draws <- stats::rnorm(nrow(grid), mean = mean_baseline_mwh / n_periods,
                                 sd = sd_baseline_mwh / sqrt(n_periods))
  baseline_energy_mwh <- pmax(baseline_draws, 0.01)

  realised_savings_fraction <- pmin(pmax(stats::rnorm(n_sites, mean = savings_fraction, sd = 0.05), 0.05), 0.8)
  project_energy_mwh <- baseline_energy_mwh * (1 - realised_savings_fraction[match(grid$site_id, site_ids)])

  technology_catalogue <- c(
    "efficient_lighting",
    "efficient_motors",
    "hvac_optimization",
    "refrigeration_efficiency",
    "variable_speed_drives",
    "compressed_air_improvement"
  )
  technology_assignment <- sample(technology_catalogue, size = n_sites, replace = TRUE)

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      baseline_energy_mwh = baseline_energy_mwh,
      project_energy_mwh = project_energy_mwh,
      emission_factor_tco2e_mwh = emission_factor_tco2e_mwh,
      technology = technology_assignment[match(site_id, site_ids)]
    )
}
