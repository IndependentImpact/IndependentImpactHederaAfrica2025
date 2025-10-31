#' Simulate a dataset compliant with AMS-II.F
#'
#' Generates tidy monitoring data for agricultural facilities implementing energy
#' efficiency and fuel switching measures under AMS-II.F. The simulation produces
#' multi-period observations with baseline and project fuel/electricity use,
#' emission factors, production proxies, leakage placeholders, and monitoring
#' metadata.
#'
#' @param n_facilities Number of agricultural facilities to simulate.
#' @param n_periods Number of monitoring periods per facility (default monthly).
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param baseline_electricity_mean Mean baseline electricity consumption per
#'   period (MWh).
#' @param baseline_fuel_mean Mean baseline thermal fuel energy demand per period
#'   (GJ).
#' @param electricity_savings Expected fractional reduction in electricity demand
#'   under the project (0-1).
#' @param fuel_savings Expected fractional reduction in fuel demand under the
#'   project (0-1).
#' @param fuel_switch_reduction Expected fractional reduction in thermal emission
#'   factors due to fuel switching (0-1).
#' @return A tibble containing facility IDs, monitoring metadata, baseline and
#'   project energy parameters, production proxies, and leakage placeholders.
#' @examples
#' simulate_ams_iif_dataset(n_facilities = 3)
#' @importFrom stats rnorm runif
#' @importFrom tibble as_tibble
#' @export
simulate_ams_iif_dataset <- function(n_facilities = 6,
                                     n_periods = 12,
                                     start_year = 2023,
                                     start_month = 1,
                                     baseline_electricity_mean = 140,
                                     baseline_fuel_mean = 420,
                                     electricity_savings = 0.22,
                                     fuel_savings = 0.38,
                                     fuel_switch_reduction = 0.35) {
  if (!is.numeric(n_facilities) || length(n_facilities) != 1 || n_facilities <= 0) {
    stop("`n_facilities` must be a positive numeric value.", call. = FALSE)
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
  if (!is.numeric(baseline_fuel_mean) || baseline_fuel_mean <= 0) {
    stop("`baseline_fuel_mean` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(electricity_savings) || electricity_savings <= 0 || electricity_savings >= 1) {
    stop("`electricity_savings` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(fuel_savings) || fuel_savings <= 0 || fuel_savings >= 1) {
    stop("`fuel_savings` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(fuel_switch_reduction) || fuel_switch_reduction < 0 || fuel_switch_reduction >= 1) {
    stop("`fuel_switch_reduction` must be between 0 (inclusive) and 1 (exclusive).", call. = FALSE)
  }

  facility_ids <- sprintf("facility_%02d", seq_len(n_facilities))
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

  baseline_electricity <- stats::rnorm(nrow(grid), mean = baseline_electricity_mean, sd = baseline_electricity_mean * 0.18)
  baseline_electricity <- pmax(baseline_electricity, 1e-3)

  baseline_fuel <- stats::rnorm(nrow(grid), mean = baseline_fuel_mean, sd = baseline_fuel_mean * 0.2)
  baseline_fuel <- pmax(baseline_fuel, 1e-3)

  baseline_electricity_factor <- stats::runif(nrow(grid), min = 0.5, max = 0.68)
  baseline_fuel_factor <- stats::runif(nrow(grid), min = 0.06, max = 0.08)

  project_electricity <- baseline_electricity * (1 - electricity_savings)
  project_fuel <- baseline_fuel * (1 - fuel_savings)
  project_electricity_factor <- baseline_electricity_factor
  project_fuel_factor <- baseline_fuel_factor * (1 - fuel_switch_reduction)

  production_proxy <- stats::rnorm(nrow(grid), mean = 160, sd = 18)
  production_proxy <- pmax(production_proxy, 1)
  operating_hours <- stats::rnorm(nrow(grid), mean = 210, sd = 18)
  operating_hours <- pmax(operating_hours, 1)

  baseline_total_energy_mwh <- baseline_electricity + baseline_fuel / 3.6
  project_total_energy_mwh <- project_electricity + project_fuel / 3.6

  leakage_component <- baseline_fuel * baseline_fuel_factor * 0.012

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      baseline_electricity_mwh = baseline_electricity,
      baseline_fuel_energy_gj = baseline_fuel,
      baseline_electricity_emission_factor_tco2_per_mwh = baseline_electricity_factor,
      baseline_fuel_emission_factor_tco2_per_gj = baseline_fuel_factor,
      baseline_total_energy_mwh = baseline_total_energy_mwh,
      project_electricity_mwh = project_electricity,
      project_fuel_energy_gj = project_fuel,
      project_electricity_emission_factor_tco2_per_mwh = project_electricity_factor,
      project_fuel_emission_factor_tco2_per_gj = project_fuel_factor,
      project_total_energy_mwh = project_total_energy_mwh,
      service_level_indicator = production_proxy,
      operating_hours = operating_hours,
      leakage_emissions_tco2e = leakage_component
    )
}
