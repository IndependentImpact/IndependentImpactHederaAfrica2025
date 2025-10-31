#' Simulate a dataset compliant with AMS-I.F
#'
#' Generates a tidy dataset representing electricity supplied by renewable
#' generation assets to captive and mini-grid consumers. The simulation provides
#' monitoring metadata, captive use shares, and emission outcomes compatible
#' with the aggregation helpers.
#'
#' @param n_grids Number of captive systems or mini-grids to simulate.
#' @param n_periods Number of monitoring periods per mini-grid.
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param mean_supply_mwh Mean annual electricity supplied per mini-grid in MWh.
#' @param sd_supply_mwh Standard deviation of annual electricity supplied in MWh.
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWh.
#' @param project_emission_factor Project emission factor in tCO2e/MWh (default 0.02).
#' @param captive_use_share_mean Mean share of electricity consumed within the captive system.
#' @param captive_use_share_sd Standard deviation of the captive use share.
#' @return A tibble containing grid identifiers, monitoring period metadata,
#'   electricity supply, emissions, and captive use shares.
#' @examples
#' simulate_ams_if_dataset(n_grids = 5)
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @export
simulate_ams_if_dataset <- function(n_grids = 10,
                                    n_periods = 12,
                                    start_year = 2023,
                                    start_month = 1,
                                    mean_supply_mwh = 18000,
                                    sd_supply_mwh = 2500,
                                    baseline_emission_factor = 0.7,
                                    project_emission_factor = 0.02,
                                    captive_use_share_mean = 0.7,
                                    captive_use_share_sd = 0.08) {
  if (!is.numeric(n_grids) || length(n_grids) != 1 || n_grids <= 0) {
    stop("`n_grids` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(captive_use_share_mean) || length(captive_use_share_mean) != 1 ||
      captive_use_share_mean <= 0 || captive_use_share_mean >= 1) {
    stop("`captive_use_share_mean` must be a numeric value between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(captive_use_share_sd) || length(captive_use_share_sd) != 1 || captive_use_share_sd <= 0) {
    stop("`captive_use_share_sd` must be a positive numeric value.", call. = FALSE)
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

  grid_ids <- paste0("grid_", seq_len(n_grids))
  period_index <- seq_len(n_periods)

  grid <- expand.grid(
    grid_id = grid_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$grid_id, grid$monitoring_period), , drop = FALSE]

  month_offset <- grid$monitoring_period - 1L
  grid$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  grid$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  grid$day <- sample.int(28L, size = nrow(grid), replace = TRUE)
  grid$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", grid$year, grid$month, grid$day))
  grid$monitoring_label <- sprintf("%04d-%02d", grid$year, grid$month)

  supply_draws <- stats::rnorm(
    n = nrow(grid),
    mean = mean_supply_mwh / n_periods,
    sd = sd_supply_mwh / sqrt(n_periods)
  )
  electricity_mwh <- pmax(supply_draws, 0)

  baseline_electricity_mwh <- electricity_mwh
  project_electricity_mwh <- electricity_mwh * (project_emission_factor / max(baseline_emission_factor, 1e-9))

  baseline_emissions_tco2e <- baseline_electricity_mwh * baseline_emission_factor
  project_emissions_tco2e <- electricity_mwh * project_emission_factor
  emission_reductions_tco2e <- baseline_emissions_tco2e - project_emissions_tco2e

  captive_use_share <- pmin(pmax(stats::rnorm(nrow(grid), captive_use_share_mean, captive_use_share_sd), 0), 1)

  tibble::as_tibble(grid) |>
    dplyr::mutate(
      electricity_mwh = electricity_mwh,
      baseline_emission_factor = baseline_emission_factor,
      project_emission_factor = project_emission_factor,
      baseline_electricity_mwh = baseline_electricity_mwh,
      project_electricity_mwh = project_electricity_mwh,
      baseline_emissions_tco2e = baseline_emissions_tco2e,
      project_emissions_tco2e = project_emissions_tco2e,
      emission_reductions_tco2e = emission_reductions_tco2e,
      captive_use_share = captive_use_share
    )
}
