#' Simulate a dataset compliant with AMS-II.E
#'
#' Generates tidy monitoring data for building portfolios implementing energy
#' efficiency and fuel switching measures under AMS-II.E. The simulation produces
#' multi-period observations with baseline and project thermal and electrical
#' energy parameters, emission factors, service indicators, leakage placeholders,
#' and monitoring metadata.
#'
#' @param n_buildings Number of buildings to simulate.
#' @param n_periods Number of monitoring periods per building (default monthly).
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param baseline_electricity_mean Mean baseline electricity consumption per
#'   period (MWh).
#' @param baseline_thermal_mean Mean baseline thermal energy demand per period
#'   (GJ).
#' @param electricity_savings Expected fractional reduction in electricity demand
#'   under the project (0-1).
#' @param thermal_savings Expected fractional reduction in thermal demand under
#'   the project (0-1).
#' @param fuel_switch_reduction Expected fractional reduction in thermal emission
#'   factors due to fuel switching (0-1).
#' @return A tibble containing building IDs, monitoring metadata, baseline and
#'   project energy parameters, service indicators, and leakage placeholders.
#' @examples
#' simulate_ams_iie_dataset(n_buildings = 3)
#' @importFrom stats rnorm runif
#' @importFrom tibble as_tibble
#' @export
simulate_ams_iie_dataset <- function(n_buildings = 6,
                                     n_periods = 12,
                                     start_year = 2023,
                                     start_month = 1,
                                     baseline_electricity_mean = 140,
                                     baseline_thermal_mean = 380,
                                     electricity_savings = 0.25,
                                     thermal_savings = 0.35,
                                     fuel_switch_reduction = 0.3) {
  if (!is.numeric(n_buildings) || length(n_buildings) != 1 || n_buildings <= 0) {
    stop("`n_buildings` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(baseline_electricity_mean) || baseline_electricity_mean <= 0) {
    stop("`baseline_electricity_mean` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(baseline_thermal_mean) || baseline_thermal_mean <= 0) {
    stop("`baseline_thermal_mean` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(electricity_savings) || electricity_savings <= 0 || electricity_savings >= 1) {
    stop("`electricity_savings` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(thermal_savings) || thermal_savings <= 0 || thermal_savings >= 1) {
    stop("`thermal_savings` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(fuel_switch_reduction) || fuel_switch_reduction < 0 || fuel_switch_reduction >= 1) {
    stop("`fuel_switch_reduction` must be between 0 (inclusive) and 1 (exclusive).", call. = FALSE)
  }

  building_ids <- sprintf("building_%02d", seq_len(n_buildings))
  period_index <- seq_len(n_periods)

  grid <- expand.grid(
    building_id = building_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$building_id, grid$monitoring_period), , drop = FALSE]

  month_offset <- grid$monitoring_period - 1L
  grid$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  grid$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  grid$day <- sample.int(28L, size = nrow(grid), replace = TRUE)
  grid$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", grid$year, grid$month, grid$day))
  grid$monitoring_label <- sprintf("%04d-%02d", grid$year, grid$month)

  baseline_electricity <- stats::rnorm(nrow(grid), mean = baseline_electricity_mean, sd = baseline_electricity_mean * 0.15)
  baseline_electricity <- pmax(baseline_electricity, 1e-3)

  baseline_thermal <- stats::rnorm(nrow(grid), mean = baseline_thermal_mean, sd = baseline_thermal_mean * 0.18)
  baseline_thermal <- pmax(baseline_thermal, 1e-3)

  baseline_electricity_factor <- stats::runif(nrow(grid), min = 0.55, max = 0.68)
  baseline_thermal_factor <- stats::runif(nrow(grid), min = 0.055, max = 0.075)

  project_electricity <- baseline_electricity * (1 - electricity_savings)
  project_thermal <- baseline_thermal * (1 - thermal_savings)
  project_electricity_factor <- baseline_electricity_factor
  project_thermal_factor <- baseline_thermal_factor * (1 - fuel_switch_reduction)

  floor_area <- stats::runif(nrow(grid), min = 0.25, max = 0.45)
  operating_hours <- stats::rnorm(nrow(grid), mean = 220, sd = 20)
  operating_hours <- pmax(operating_hours, 1)

  baseline_total_energy_mwh <- baseline_electricity + baseline_thermal / 3.6
  project_total_energy_mwh <- project_electricity + project_thermal / 3.6

  leakage_component <- baseline_thermal * baseline_thermal_factor * 0.01

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      baseline_electricity_mwh = baseline_electricity,
      baseline_thermal_energy_gj = baseline_thermal,
      baseline_electricity_emission_factor_tco2_per_mwh = baseline_electricity_factor,
      baseline_thermal_emission_factor_tco2_per_gj = baseline_thermal_factor,
      baseline_total_energy_mwh = baseline_total_energy_mwh,
      project_electricity_mwh = project_electricity,
      project_thermal_energy_gj = project_thermal,
      project_electricity_emission_factor_tco2_per_mwh = project_electricity_factor,
      project_thermal_emission_factor_tco2_per_gj = project_thermal_factor,
      project_total_energy_mwh = project_total_energy_mwh,
      service_level_indicator = floor_area,
      operating_hours = operating_hours,
      leakage_emissions_tco2e = leakage_component
    )
}
