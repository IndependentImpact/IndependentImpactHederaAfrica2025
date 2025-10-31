#' Simulate monitoring data for AMS-III.E projects
#'
#' Generates synthetic monitoring datasets compatible with the AMS-III.E helpers.
#' The output bundles baseline, project, leakage, and applicability tables to
#' streamline examples, demos, and automated tests.
#'
#' @param n_plants Number of facilities to simulate.
#' @param n_periods Number of monitoring periods per facility.
#' @param seed Optional seed for reproducibility.
#' @return List containing tibbles `baseline`, `project`, `leakage`, and
#'   `applicability`.
#' @examples
#' simulate_ams_iiie_dataset(n_plants = 2, n_periods = 3, seed = 123)
#' @export
simulate_ams_iiie_dataset <- function(n_plants = 3,
                                      n_periods = 4,
                                      seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  plant_ids <- paste0("PL", seq_len(n_plants))
  periods <- seq_len(n_periods)

  expand_index <- tidyr::expand_grid(plant_id = plant_ids, period = periods)

  baseline <- expand_index |>
    dplyr::mutate(
      biomass_tonnes = stats::runif(dplyr::n(), 800, 1500),
      methane_potential_m3_per_tonne = stats::rnorm(dplyr::n(), mean = 115, sd = 10),
      anaerobic_decay_fraction = stats::runif(dplyr::n(), 0.5, 0.75),
      days_in_period = 365 / n_periods
    )

  project <- baseline |>
    dplyr::transmute(
      plant_id,
      period,
      treated_biomass_tonnes = biomass_tonnes * stats::runif(dplyr::n(), 0.85, 0.98),
      methane_potential_m3_per_tonne = methane_potential_m3_per_tonne,
      methane_slip_fraction = stats::runif(dplyr::n(), 0.03, 0.07),
      auxiliary_fuel_consumption_tj = stats::runif(dplyr::n(), 0.6, 1.5),
      auxiliary_fuel_ef_tco2_per_tj = stats::runif(dplyr::n(), 60, 75),
      days_in_period = days_in_period
    )

  leakage <- baseline |>
    dplyr::transmute(
      plant_id,
      period,
      biomass_transported_tonnes = biomass_tonnes * stats::runif(dplyr::n(), 0.95, 1.05),
      transport_distance_km = stats::runif(dplyr::n(), 10, 60),
      transport_ef_tco2_per_tkm = stats::runif(dplyr::n(), 0.00008, 0.00015),
      alternative_use_fraction = stats::runif(dplyr::n(), 0.02, 0.12),
      alternative_use_ef_tco2_per_tonne = stats::runif(dplyr::n(), 0.3, 0.6)
    )

  applicability <- tibble::tibble(
    plant_id = plant_ids,
    feedstock_type = sample(
      c("agricultural residues", "forest residues", "industrial biomass"),
      size = n_plants,
      replace = TRUE
    ),
    moisture_fraction = stats::runif(n_plants, 0.35, 0.6),
    anaerobic_baseline = sample(c(TRUE, TRUE, TRUE, FALSE), size = n_plants, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    biomass_control_plan = sample(c(TRUE, TRUE, FALSE), size = n_plants, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    energy_measurements_per_month = stats::rpois(n_plants, lambda = 6),
    operating_hours_per_period = stats::runif(n_plants, 380, 500),
    feedstock_samples_per_month = stats::rpois(n_plants, lambda = 3)
  )

  list(
    baseline = baseline,
    project = project,
    leakage = leakage,
    applicability = applicability
  )
}
