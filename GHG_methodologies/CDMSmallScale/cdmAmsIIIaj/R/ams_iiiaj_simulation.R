#' Simulate AMS-III.AJ monitoring datasets
#'
#' Generates simple synthetic datasets that mimic the monitoring requirements for
#' recycling projects under AMS-III.AJ. The function returns a list containing
#' baseline, project, leakage, and applicability tibbles for use in examples and
#' tests.
#'
#' @param n_facilities Number of recycling facilities to simulate.
#' @param n_periods Number of monitoring periods per facility.
#' @param seed Optional seed for reproducibility (not used in the deterministic
#'   generator but accepted for API compatibility).
#' @return A list with `baseline`, `project`, `leakage`, and `applicability`
#'   components.
#' @examples
#' simulate_ams_iiiaj_dataset(n_facilities = 2, n_periods = 3)
#' @export
simulate_ams_iiiaj_dataset <- function(n_facilities = 3,
                                       n_periods = 4,
                                       seed = NULL) {
  facility_ids <- paste0("F", seq_len(n_facilities))
  periods <- seq_len(n_periods)
  grid <- tidyr::expand_grid(facility_id = facility_ids, period = periods)
  n_rows <- nrow(grid)

  baseline <- tibble::tibble(
    facility_id = grid$facility_id,
    period = grid$period,
    days_in_period = 30,
    material_processed_tonnes = 35 + 5 * grid$period + seq_len(n_rows) %% 3,
    virgin_material_ef_tco2_per_tonne = 1.6 + 0.05 * (seq_len(n_rows) %% 4),
    baseline_disposal_ef_tco2_per_tonne = 0.28 + 0.01 * (seq_len(n_rows) %% 3),
    residual_fraction = 0.04 + 0.005 * (seq_len(n_rows) %% 4)
  )

  project <- tibble::tibble(
    facility_id = grid$facility_id,
    period = grid$period,
    days_in_period = 30,
    electricity_consumption_mwh = 10 + 0.5 * baseline$material_processed_tonnes / 10,
    electricity_ef_tco2_per_mwh = 0.44 + 0.01 * (seq_len(n_rows) %% 3),
    thermal_fuel_consumption_gj = 3.5 + 0.2 * baseline$material_processed_tonnes / 20,
    thermal_fuel_ef_tco2_per_gj = 0.066 + 0.002 * (seq_len(n_rows) %% 2),
    supplementary_material_tonnes = 0.3 + 0.02 * baseline$material_processed_tonnes / 40,
    supplementary_material_ef_tco2_per_tonne = 0.45 + 0.02 * (seq_len(n_rows) %% 3)
  )

  leakage <- tibble::tibble(
    facility_id = grid$facility_id,
    period = grid$period,
    material_collected_tonnes = baseline$material_processed_tonnes * 1.1,
    transport_distance_km = 15 + 2 * (seq_len(n_rows) %% 5),
    transport_ef_tco2_per_tkm = 0.00011 + 0.000005 * (seq_len(n_rows) %% 4),
    residual_disposal_tonnes = baseline$material_processed_tonnes * baseline$residual_fraction,
    residual_disposal_ef_tco2_per_tonne = 0.29 + 0.01 * (seq_len(n_rows) %% 3),
    market_leakage_fraction = -0.02 + 0.01 * (seq_len(n_rows) %% 5),
    market_emission_factor_tco2_per_tonne = 0.5 + 0.05 * (seq_len(n_rows) %% 4)
  )

  applicability <- tidyr::expand_grid(
    facility_id = facility_ids,
    material_type = c("paper", "plastics", "metals")
  )
  applicability <- applicability |>
    dplyr::mutate(
      contamination_rate = 0.04 + 0.01 * (seq_len(dplyr::n()) %% 3),
      specification_met = TRUE,
      segregation_rate = 0.7 + 0.02 * (seq_len(dplyr::n()) %% 4),
      coverage_rate = 0.78 + 0.01 * (seq_len(dplyr::n()) %% 3),
      logistics_score = 0.75 + 0.015 * (seq_len(dplyr::n()) %% 5),
      throughput_monitoring = TRUE,
      residual_tracking = (seq_len(dplyr::n()) %% 5) != 0,
      calibration_events = 1 + (seq_len(dplyr::n()) %% 3)
    )

  list(
    baseline = baseline,
    project = project,
    leakage = leakage,
    applicability = applicability
  )
}
