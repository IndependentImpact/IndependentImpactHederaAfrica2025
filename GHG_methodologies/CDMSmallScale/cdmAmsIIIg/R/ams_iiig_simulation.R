#' Simulate monitoring data for AMS-III.G projects
#'
#' Generates synthetic monitoring datasets consistent with the AMS-III.G helper
#' functions. The output bundles baseline, project, leakage, and applicability
#' tables with identifiers suitable for examples and automated tests.
#'
#' @param n_sites Number of landfill sites to simulate.
#' @param n_periods Number of monitoring periods per site.
#' @param seed Optional seed for reproducibility.
#' @return List containing tibbles `baseline`, `project`, `leakage`, and
#'   `applicability`.
#' @examples
#' simulate_ams_iiig_dataset(n_sites = 2, n_periods = 3, seed = 2025)
#' @export
simulate_ams_iiig_dataset <- function(n_sites = 3,
                                      n_periods = 4,
                                      seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  site_ids <- paste0("LF", seq_len(n_sites))
  periods <- seq_len(n_periods)

  expand_index <- tidyr::expand_grid(site_id = site_ids, period = periods)

  baseline <- expand_index |>
    dplyr::mutate(
      waste_disposed_tonnes = stats::runif(dplyr::n(), 8000, 15000),
      methane_generation_potential_m3_per_tonne = stats::runif(dplyr::n(), 75, 95),
      baseline_collection_efficiency_fraction = stats::runif(dplyr::n(), 0.05, 0.12),
      oxidation_fraction = stats::runif(dplyr::n(), 0.02, 0.12),
      days_in_period = stats::runif(dplyr::n(), 28, 35)
    )

  project <- baseline |>
    dplyr::transmute(
      site_id,
      period,
      waste_disposed_tonnes = waste_disposed_tonnes,
      methane_generation_potential_m3_per_tonne = methane_generation_potential_m3_per_tonne,
      project_collection_efficiency_fraction = stats::runif(dplyr::n(), 0.6, 0.8),
      destruction_efficiency_fraction = stats::runif(dplyr::n(), 0.96, 0.995),
      oxidation_fraction = stats::runif(dplyr::n(), 0.05, 0.15),
      days_in_period = days_in_period,
      electricity_consumption_mwh = stats::runif(dplyr::n(), 15, 45),
      electricity_ef_tco2_per_mwh = stats::runif(dplyr::n(), 0.35, 0.65),
      diesel_consumption_litres = stats::runif(dplyr::n(), 800, 1600),
      diesel_ef_tco2_per_litre = stats::runif(dplyr::n(), 0.0026, 0.0029)
    )

  leakage <- baseline |>
    dplyr::transmute(
      site_id,
      period,
      waste_transported_tonnes = waste_disposed_tonnes * stats::runif(dplyr::n(), 0.02, 0.06),
      transport_distance_km = stats::runif(dplyr::n(), 5, 35),
      transport_ef_tco2_per_tkm = stats::runif(dplyr::n(), 0.00008, 0.00012),
      residual_waste_tonnes = waste_disposed_tonnes * stats::runif(dplyr::n(), 0.01, 0.03),
      residual_waste_ef_tco2_per_tonne = stats::runif(dplyr::n(), 0.25, 0.45),
      displaced_electricity_mwh = stats::runif(dplyr::n(), 3, 12),
      displaced_electricity_ef_tco2_per_mwh = stats::runif(dplyr::n(), 0.4, 0.7)
    )

  applicability <- tibble::tibble(
    site_id = site_ids,
    landfill_status = sample(
      c("open dump", "controlled landfill", "sanitary landfill"),
      size = n_sites,
      replace = TRUE
    ),
    biodegradable_fraction = stats::runif(n_sites, 0.35, 0.65),
    gas_collection_ready = sample(c(TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
    destruction_technology = sample(
      c("enclosed flare", "electricity generation", "open flare"),
      size = n_sites,
      replace = TRUE
    ),
    operating_hours_per_week = stats::runif(n_sites, 10, 50),
    redundancy_installed = sample(c(TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.55, 0.25, 0.2)),
    flow_measurements_per_week = stats::rpois(n_sites, lambda = 5),
    methane_measurements_per_week = stats::rpois(n_sites, lambda = 4),
    calibration_events_per_year = stats::rpois(n_sites, lambda = 5)
  )

  list(
    baseline = baseline,
    project = project,
    leakage = leakage,
    applicability = applicability
  )
}
