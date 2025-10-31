#' Simulate monitoring data for AMS-III.H projects
#'
#' Generates synthetic wastewater monitoring datasets consistent with the
#' AMS-III.H helper functions. The output bundles baseline, project, leakage, and
#' applicability tables that can be reused in examples and automated tests.
#'
#' @param n_sites Number of wastewater facilities to simulate.
#' @param n_periods Number of monitoring periods per facility.
#' @param seed Optional seed for reproducibility.
#' @return List containing tibbles `baseline`, `project`, `leakage`, and
#'   `applicability`.
#' @examples
#' simulate_ams_iiih_dataset(n_sites = 2, n_periods = 3, seed = 2025)
#' @export
simulate_ams_iiih_dataset <- function(n_sites = 3,
                                      n_periods = 4,
                                      seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  site_ids <- paste0("WW", seq_len(n_sites))
  periods <- seq_len(n_periods)

  expand_index <- tidyr::expand_grid(site_id = site_ids, period = periods)

  baseline <- expand_index |>
    dplyr::mutate(
      influent_flow_m3_per_day = stats::runif(dplyr::n(), 2500, 6000),
      cod_mg_l = stats::runif(dplyr::n(), 2500, 4500),
      biodegradable_fraction = stats::runif(dplyr::n(), 0.65, 0.85),
      baseline_capture_efficiency_fraction = stats::runif(dplyr::n(), 0.02, 0.08),
      baseline_oxidation_fraction = stats::runif(dplyr::n(), 0.02, 0.12),
      days_in_period = stats::runif(dplyr::n(), 28, 35)
    )

  project <- baseline |>
    dplyr::transmute(
      site_id,
      period,
      influent_flow_m3_per_day = influent_flow_m3_per_day,
      cod_mg_l = cod_mg_l,
      biodegradable_fraction = biodegradable_fraction,
      project_capture_efficiency_fraction = stats::runif(dplyr::n(), 0.65, 0.85),
      destruction_efficiency_fraction = stats::runif(dplyr::n(), 0.97, 0.995),
      project_oxidation_fraction = stats::runif(dplyr::n(), 0.05, 0.15),
      fugitive_leakage_fraction = stats::runif(dplyr::n(), 0.005, 0.02),
      days_in_period = days_in_period,
      electricity_consumption_mwh = stats::runif(dplyr::n(), 20, 55),
      electricity_ef_tco2_per_mwh = stats::runif(dplyr::n(), 0.35, 0.6),
      thermal_energy_consumption_gj = stats::runif(dplyr::n(), 5, 18),
      thermal_energy_ef_tco2_per_gj = stats::runif(dplyr::n(), 0.055, 0.07)
    )

  leakage <- baseline |>
    dplyr::transmute(
      site_id,
      period,
      sludge_tonnes = stats::runif(dplyr::n(), 200, 480),
      transport_distance_km = stats::runif(dplyr::n(), 10, 45),
      transport_ef_tco2_per_tkm = stats::runif(dplyr::n(), 0.00008, 0.00013),
      sludge_treatment_ef_tco2_per_tonne = stats::runif(dplyr::n(), 0.25, 0.5),
      chemical_usage_tonnes = stats::runif(dplyr::n(), 5, 15),
      chemical_ef_tco2_per_tonne = stats::runif(dplyr::n(), 0.6, 1),
      displaced_fossil_fuel_gj = stats::runif(dplyr::n(), 50, 160),
      displaced_fossil_fuel_ef_tco2_per_gj = stats::runif(dplyr::n(), 0.055, 0.07)
    )

  applicability <- tibble::tibble(
    site_id = site_ids,
    wastewater_type = sample(
      c("industrial", "agro-industrial", "domestic"),
      size = n_sites,
      replace = TRUE
    ),
    cod_mg_l = stats::runif(n_sites, 2200, 4800),
    anaerobic_fraction = stats::runif(n_sites, 0.45, 0.85),
    baseline_system = sample(
      c("open anaerobic lagoon", "covered anaerobic lagoon", "uncovered reactor"),
      size = n_sites,
      replace = TRUE
    ),
    gas_capture_installed = sample(c(TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
    destruction_technology = sample(
      c("enclosed flare", "boiler firing", "electricity generation", "biogas upgrading"),
      size = n_sites,
      replace = TRUE
    ),
    operating_hours_per_week = stats::runif(n_sites, 15, 60),
    redundancy_installed = sample(c(TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.55, 0.25, 0.2)),
    utilisation_documented = sample(c(TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.7, 0.3)),
    flow_measurements_per_week = stats::rpois(n_sites, lambda = 10),
    methane_measurements_per_week = stats::rpois(n_sites, lambda = 5),
    cod_samples_per_month = stats::rpois(n_sites, lambda = 6),
    calibration_events_per_year = stats::rpois(n_sites, lambda = 5)
  )

  list(
    baseline = baseline,
    project = project,
    leakage = leakage,
    applicability = applicability
  )
}
