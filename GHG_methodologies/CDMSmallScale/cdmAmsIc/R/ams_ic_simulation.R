#' Simulate a dataset compliant with AMS-I.C
#'
#' Generates a tidy dataset representing thermal energy production from renewable systems that
#' displace fossil-fuel-based heat generation. The simulation provides monitoring metadata and
#' emission outcomes compatible with the package aggregation helpers.
#'
#' @param n_facilities Number of thermal energy facilities to simulate.
#' @param n_periods Number of monitoring periods per facility.
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param mean_thermal_mwh Mean annual useful thermal energy per facility in MWhth.
#' @param sd_thermal_mwh Standard deviation of annual thermal energy in MWhth.
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWhth.
#' @param project_emission_factor Project emission factor in tCO2e/MWhth (default 0.01).
#' @return A tibble containing facility identifiers, monitoring period metadata, thermal energy, and emissions.
#' @examples
#' simulate_ams_ic_dataset(n_facilities = 5)
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @export
simulate_ams_ic_dataset <- function(n_facilities = 10,
                                     n_periods = 12,
                                     start_year = 2023,
                                     start_month = 1,
                                     mean_thermal_mwh = 12000,
                                     sd_thermal_mwh = 1800,
                                     baseline_emission_factor = 0.27,
                                     project_emission_factor = 0.01) {
  if (!is.numeric(n_facilities) || length(n_facilities) != 1 || n_facilities <= 0) {
    stop("`n_facilities` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(baseline_emission_factor) || length(baseline_emission_factor) != 1 || baseline_emission_factor < 0) {
    stop("`baseline_emission_factor` must be a non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(project_emission_factor) || length(project_emission_factor) != 1 || project_emission_factor < 0) {
    stop("`project_emission_factor` must be a non-negative numeric value.", call. = FALSE)
  }

  facility_ids <- paste0("facility_", seq_len(n_facilities))
  period_index <- seq_len(n_periods)

  grid <- expand.grid(
    facility_id = facility_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$facility_id, grid$monitoring_period), , drop = FALSE]

  month_offset <- grid$monitoring_period - 1L
  grid$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  grid$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  grid$day <- sample.int(28L, size = nrow(grid), replace = TRUE)
  grid$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", grid$year, grid$month, grid$day))
  grid$monitoring_label <- sprintf("%04d-%02d", grid$year, grid$month)

  thermal_draws <- stats::rnorm(
    n = nrow(grid),
    mean = mean_thermal_mwh / n_periods,
    sd = sd_thermal_mwh / sqrt(n_periods)
  )
  thermal_energy_mwh <- pmax(thermal_draws, 0)

  baseline_thermal_output_mwh <- thermal_energy_mwh
  project_thermal_output_mwh <- thermal_energy_mwh * (project_emission_factor / max(baseline_emission_factor, 1e-9))

  baseline_emissions_tco2e <- baseline_thermal_output_mwh * baseline_emission_factor
  project_emissions_tco2e <- thermal_energy_mwh * project_emission_factor
  emission_reductions_tco2e <- baseline_emissions_tco2e - project_emissions_tco2e

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      thermal_energy_mwh = thermal_energy_mwh,
      baseline_emission_factor = baseline_emission_factor,
      project_emission_factor = project_emission_factor,
      baseline_thermal_output_mwh = baseline_thermal_output_mwh,
      project_thermal_output_mwh = project_thermal_output_mwh,
      baseline_emissions_tco2e = baseline_emissions_tco2e,
      project_emissions_tco2e = project_emissions_tco2e,
      emission_reductions_tco2e = emission_reductions_tco2e
    )
}
