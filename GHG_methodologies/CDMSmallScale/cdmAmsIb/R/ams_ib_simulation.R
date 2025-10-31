#' Simulate a dataset compliant with AMS-I.B
#'
#' Generates a tidy dataset representing mechanical energy services (e.g. water pumping or milling)
#' delivered by renewable systems that displace fossil fuel-based equipment.
#'
#' @param n_machines Number of mechanical systems to simulate.
#' @param n_periods Number of monitoring periods per system.
#' @param start_year Calendar year of the first monitoring period.
#' @param start_month Calendar month (1-12) of the first monitoring period.
#' @param mean_service_mj Mean annual mechanical energy service per machine in MJ.
#' @param sd_service_mj Standard deviation of annual mechanical energy service in MJ.
#' @param emission_factor Baseline emission factor in tCO2e/MJ.
#' @param project_fossil_share Share of mechanical service retained by fossil back-up systems (0-1).
#' @return A tibble containing monitoring metadata, baseline fuel characteristics, and emission outcomes.
#' @examples
#' simulate_ams_ib_dataset(n_machines = 5)
#' @importFrom stats rnorm runif
#' @importFrom tibble as_tibble
#' @export
simulate_ams_ib_dataset <- function(n_machines = 15,
                                    n_periods = 12,
                                    start_year = 2023,
                                    start_month = 1,
                                    mean_service_mj = 48000,
                                    sd_service_mj = 6000,
                                    emission_factor = 0.00007,
                                    project_fossil_share = 0.05) {
  if (!is.numeric(n_machines) || length(n_machines) != 1 || n_machines <= 0) {
    stop("`n_machines` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(emission_factor) || length(emission_factor) != 1 || emission_factor < 0) {
    stop("`emission_factor` must be a non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(project_fossil_share) || length(project_fossil_share) != 1 ||
      project_fossil_share < 0 || project_fossil_share > 1) {
    stop("`project_fossil_share` must be between 0 and 1.", call. = FALSE)
  }

  machine_ids <- paste0("machine_", seq_len(n_machines))
  period_index <- seq_len(n_periods)

  grid <- expand.grid(
    machine_id = machine_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$machine_id, grid$monitoring_period), , drop = FALSE]

  month_offset <- grid$monitoring_period - 1L
  grid$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  grid$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  grid$day <- sample.int(28L, size = nrow(grid), replace = TRUE)
  grid$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", grid$year, grid$month, grid$day))
  grid$monitoring_label <- sprintf("%04d-%02d", grid$year, grid$month)

  service_draws <- stats::rnorm(
    n = nrow(grid),
    mean = mean_service_mj / n_periods,
    sd = sd_service_mj / sqrt(n_periods)
  )
  service_mj <- pmax(service_draws, 0)

  net_calorific_value <- stats::runif(nrow(grid), min = 40, max = 45)
  fuel_consumption <- ifelse(net_calorific_value > 0, service_mj / net_calorific_value, 0)

  project_energy_mj <- service_mj * project_fossil_share
  baseline_energy_mj <- service_mj

  baseline_emissions_tco2e <- baseline_energy_mj * emission_factor
  project_emissions_tco2e <- project_energy_mj * emission_factor
  emission_reductions_tco2e <- baseline_emissions_tco2e - project_emissions_tco2e

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      service_output_mj = service_mj,
      fuel_consumption = fuel_consumption,
      net_calorific_value = net_calorific_value,
      emission_factor = emission_factor,
      project_energy_mj = project_energy_mj,
      baseline_energy_mj = baseline_energy_mj,
      baseline_emissions_tco2e = baseline_emissions_tco2e,
      project_emissions_tco2e = project_emissions_tco2e,
      emission_reductions_tco2e = emission_reductions_tco2e
    )
}
