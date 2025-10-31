#' Simulate a dataset compliant with AMS-II.D
#'
#' Generates tidy monitoring data for industrial facilities implementing energy
#' efficiency and fuel switching measures under AMS-II.D. The simulation creates
#' multi-period observations with baseline and project fuel parameters, emission
#' factors, efficiencies, leakage placeholders, and monitoring metadata.
#'
#' @param n_facilities Number of industrial facilities to simulate.
#' @param n_periods Number of monitoring periods per facility (default monthly).
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param baseline_fuel_mean Mean baseline fuel consumption per period (in
#'   physical units consistent with the emission factor inputs).
#' @param baseline_fuel_sd Standard deviation of baseline fuel consumption per
#'   period.
#' @param efficiency_improvement Expected fractional increase in thermal
#'   efficiency delivered by the project (0-1).
#' @param fuel_switch_reduction Expected fractional reduction in emission factor
#'   due to fuel switching (0-1).
#' @return A tibble containing facility IDs, monitoring metadata, baseline and
#'   project parameters, and derived leakage placeholders.
#' @examples
#' simulate_ams_iid_dataset(n_facilities = 3)
#' @importFrom stats rnorm runif
#' @importFrom tibble as_tibble
#' @export
simulate_ams_iid_dataset <- function(n_facilities = 6,
                                     n_periods = 12,
                                     start_year = 2023,
                                     start_month = 1,
                                     baseline_fuel_mean = 1100,
                                     baseline_fuel_sd = 160,
                                     efficiency_improvement = 0.15,
                                     fuel_switch_reduction = 0.12) {
  if (!is.numeric(n_facilities) || length(n_facilities) != 1 || n_facilities <= 0) {
    stop("`n_facilities` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(baseline_fuel_mean) || baseline_fuel_mean <= 0) {
    stop("`baseline_fuel_mean` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(baseline_fuel_sd) || baseline_fuel_sd < 0) {
    stop("`baseline_fuel_sd` must be a non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(efficiency_improvement) || efficiency_improvement <= 0 || efficiency_improvement >= 1) {
    stop("`efficiency_improvement` must be between 0 and 1.", call. = FALSE)
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

  baseline_fuel <- stats::rnorm(nrow(grid), mean = baseline_fuel_mean, sd = baseline_fuel_sd)
  baseline_fuel <- pmax(baseline_fuel, 0.01)

  baseline_ncv <- stats::runif(nrow(grid), min = 0.035, max = 0.045)
  baseline_efficiency <- stats::runif(nrow(grid), min = 0.6, max = 0.75)
  baseline_emission_factor <- stats::runif(nrow(grid), min = 0.09, max = 0.1)

  project_efficiency <- pmin(baseline_efficiency * (1 + efficiency_improvement), 0.95)
  project_fuel <- baseline_fuel * baseline_efficiency / project_efficiency
  project_ncv <- baseline_ncv
  project_emission_factor <- baseline_emission_factor * (1 - fuel_switch_reduction)
  electricity_emissions <- stats::runif(nrow(grid), min = 0.1, max = 1.5)

  useful_heat_output <- project_fuel * project_ncv * project_efficiency
  leakage_component <- baseline_fuel * baseline_ncv * 0.002

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      baseline_fuel_quantity = baseline_fuel,
      baseline_ncv_gj_per_unit = baseline_ncv,
      baseline_emission_factor_tco2_per_gj = baseline_emission_factor,
      baseline_efficiency = baseline_efficiency,
      project_fuel_quantity = project_fuel,
      project_ncv_gj_per_unit = project_ncv,
      project_emission_factor_tco2_per_gj = project_emission_factor,
      project_efficiency = project_efficiency,
      electricity_emissions_tco2e = electricity_emissions,
      useful_heat_output = useful_heat_output,
      leakage_emissions_tco2e = leakage_component
    )
}
