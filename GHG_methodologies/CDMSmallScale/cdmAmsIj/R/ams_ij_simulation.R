#' Simulate a dataset compliant with AMS-I.J
#'
#' Generates a tidy dataset representing useful thermal output from solar water
#' heating systems along with auxiliary energy contributions and monitoring
#' metadata. The simulation supports tests and documentation for the AMS-I.J
#' helpers.
#'
#' @param n_sites Number of solar water heating sites to simulate.
#' @param n_periods Number of monitoring periods per site.
#' @param start_year Calendar year for the first monitoring period.
#' @param start_month Calendar month (1-12) for the first monitoring period.
#' @param mean_useful_heat_mwh Mean annual useful thermal output per site in
#'   MWhth.
#' @param sd_useful_heat_mwh Standard deviation of annual useful thermal output
#'   in MWhth.
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWhth.
#' @param auxiliary_emission_factor Auxiliary system emission factor in
#'   tCO2e/MWhth.
#' @param mean_auxiliary_share Mean share of auxiliary energy relative to useful
#'   solar heat (0-1).
#' @param sd_auxiliary_share Standard deviation of the auxiliary share.
#' @return A tibble containing site identifiers, monitoring metadata, useful
#'   thermal output, auxiliary energy, emission factors, and resulting emissions.
#' @examples
#' simulate_ams_ij_dataset(n_sites = 3)
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @export
simulate_ams_ij_dataset <- function(n_sites = 10,
                                    n_periods = 12,
                                    start_year = 2023,
                                    start_month = 1,
                                    mean_useful_heat_mwh = 360,
                                    sd_useful_heat_mwh = 55,
                                    baseline_emission_factor = 0.22,
                                    auxiliary_emission_factor = 0.18,
                                    mean_auxiliary_share = 0.15,
                                    sd_auxiliary_share = 0.05) {
  if (!is.numeric(n_sites) || length(n_sites) != 1 || n_sites <= 0) {
    stop("`n_sites` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_periods) || length(n_periods) != 1 || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }
  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(mean_useful_heat_mwh) || length(mean_useful_heat_mwh) != 1 || mean_useful_heat_mwh <= 0) {
    stop("`mean_useful_heat_mwh` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(sd_useful_heat_mwh) || length(sd_useful_heat_mwh) != 1 || sd_useful_heat_mwh <= 0) {
    stop("`sd_useful_heat_mwh` must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(baseline_emission_factor) || length(baseline_emission_factor) != 1 || baseline_emission_factor < 0) {
    stop("`baseline_emission_factor` must be a non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(auxiliary_emission_factor) || length(auxiliary_emission_factor) != 1 || auxiliary_emission_factor < 0) {
    stop("`auxiliary_emission_factor` must be a non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(mean_auxiliary_share) || length(mean_auxiliary_share) != 1 ||
      mean_auxiliary_share < 0 || mean_auxiliary_share > 1) {
    stop("`mean_auxiliary_share` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(sd_auxiliary_share) || length(sd_auxiliary_share) != 1 || sd_auxiliary_share <= 0) {
    stop("`sd_auxiliary_share` must be a positive numeric value.", call. = FALSE)
  }

  site_ids <- paste0("site_", seq_len(n_sites))
  period_index <- seq_len(n_periods)

  layout <- expand.grid(
    site_id = site_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  layout <- layout[order(layout$site_id, layout$monitoring_period), , drop = FALSE]

  month_offset <- layout$monitoring_period - 1L
  layout$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  layout$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  layout$day <- sample.int(28L, size = nrow(layout), replace = TRUE)
  layout$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", layout$year, layout$month, layout$day))
  layout$monitoring_label <- sprintf("%04d-%02d", layout$year, layout$month)

  useful_draws <- stats::rnorm(
    n = nrow(layout),
    mean = mean_useful_heat_mwh / n_periods,
    sd = sd_useful_heat_mwh / sqrt(n_periods)
  )
  useful_heat_mwh <- pmax(useful_draws, 0)

  auxiliary_share <- pmin(pmax(stats::rnorm(nrow(layout), mean_auxiliary_share, sd_auxiliary_share), 0), 1)
  auxiliary_energy_mwh <- useful_heat_mwh * auxiliary_share

  baseline_emissions_tco2e <- useful_heat_mwh * baseline_emission_factor
  project_emissions_tco2e <- auxiliary_energy_mwh * auxiliary_emission_factor
  emission_reductions_tco2e <- baseline_emissions_tco2e - project_emissions_tco2e

  solar_fraction <- 1 - auxiliary_share

  tibble::as_tibble(layout) |>
    dplyr::mutate(
      useful_heat_mwh = useful_heat_mwh,
      auxiliary_energy_mwh = auxiliary_energy_mwh,
      baseline_emission_factor = baseline_emission_factor,
      auxiliary_emission_factor = auxiliary_emission_factor,
      baseline_emissions_tco2e = baseline_emissions_tco2e,
      project_emissions_tco2e = project_emissions_tco2e,
      emission_reductions_tco2e = emission_reductions_tco2e,
      solar_fraction = solar_fraction
    )
}
