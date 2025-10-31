#' Simulate monitoring data for AMS-III.F projects
#'
#' Generates synthetic monitoring datasets compatible with the AMS-III.F helpers.
#' The output bundles baseline, project, leakage, and applicability tables to
#' streamline examples, demos, and automated tests.
#'
#' @param n_sites Number of composting facilities to simulate.
#' @param n_periods Number of monitoring periods per facility.
#' @param seed Optional seed for reproducibility.
#' @return List containing tibbles `baseline`, `project`, `leakage`, and
#'   `applicability`.
#' @examples
#' simulate_ams_iiif_dataset(n_sites = 2, n_periods = 3, seed = 123)
#' @export
simulate_ams_iiif_dataset <- function(n_sites = 3,
                                      n_periods = 4,
                                      seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  site_ids <- paste0("ST", seq_len(n_sites))
  periods <- seq_len(n_periods)

  expand_index <- tidyr::expand_grid(site_id = site_ids, period = periods)

  baseline <- expand_index |>
    dplyr::mutate(
      waste_tonnes = stats::runif(dplyr::n(), 600, 1400),
      doc_fraction = stats::runif(dplyr::n(), 0.12, 0.18),
      docf_fraction = stats::runif(dplyr::n(), 0.45, 0.55),
      baseline_mcf_fraction = stats::runif(dplyr::n(), 0.6, 0.8),
      baseline_oxidation_fraction = stats::runif(dplyr::n(), 0, 0.1)
    )

  project <- baseline |>
    dplyr::transmute(
      site_id,
      period,
      composted_waste_tonnes = waste_tonnes * stats::runif(dplyr::n(), 0.85, 0.98),
      doc_fraction = doc_fraction,
      docf_fraction = docf_fraction,
      compost_mcf_fraction = stats::runif(dplyr::n(), 0.05, 0.15),
      compost_oxidation_fraction = stats::runif(dplyr::n(), 0.05, 0.15),
      electricity_mwh = stats::runif(dplyr::n(), 20, 60),
      grid_ef_tco2_per_mwh = stats::runif(dplyr::n(), 0.4, 0.7),
      diesel_litres = stats::runif(dplyr::n(), 1500, 3500),
      diesel_ef_tco2_per_litre = stats::runif(dplyr::n(), 0.0025, 0.0028)
    )

  leakage <- baseline |>
    dplyr::transmute(
      site_id,
      period,
      compost_transported_tonnes = waste_tonnes * stats::runif(dplyr::n(), 0.9, 1.05),
      transport_distance_km = stats::runif(dplyr::n(), 5, 40),
      transport_ef_tco2_per_tkm = stats::runif(dplyr::n(), 0.00008, 0.00014),
      residual_waste_tonnes = waste_tonnes * stats::runif(dplyr::n(), 0.02, 0.08),
      residual_waste_ef_tco2_per_tonne = stats::runif(dplyr::n(), 0.3, 0.6),
      displaced_fertiliser_tonnes = stats::runif(dplyr::n(), 20, 60),
      fertiliser_ef_tco2_per_tonne = stats::runif(dplyr::n(), 0.35, 0.5)
    )

  applicability <- tibble::tibble(
    site_id = site_ids,
    waste_type = sample(
      c("municipal organics", "market waste", "food processing waste", "mixed waste"),
      size = n_sites,
      replace = TRUE
    ),
    source_segregated = sample(c(TRUE, TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.45, 0.25, 0.2, 0.1)),
    contamination_fraction = stats::runif(n_sites, 0.05, 0.2),
    aeration_method = sample(c("forced aeration", "turned windrow", "static pile"), size = n_sites, replace = TRUE),
    retention_days = stats::runif(n_sites, 30, 60),
    leachate_managed = sample(c(TRUE, TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.6, 0.2, 0.15, 0.05)),
    curing_phase = sample(c(TRUE, TRUE, FALSE), size = n_sites, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
    organic_samples_per_year = stats::rpois(n_sites, lambda = 15),
    temperature_checks_per_week = stats::rpois(n_sites, lambda = 4),
    moisture_checks_per_week = stats::rpois(n_sites, lambda = 3)
  )

  list(
    baseline = baseline,
    project = project,
    leakage = leakage,
    applicability = applicability
  )
}
