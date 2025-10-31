#' Simulate AMS-II.Q compliant monitoring datasets
#'
#' Generates example baseline, project, and leakage datasets for commercial
#' buildings so that the workflow in [estimate_emission_reductions_ams_iiq()] can
#' be executed end-to-end during tests or onboarding exercises. The simulation
#' draws baseline energy intensities, service output, emission factors, and
#' retrofit savings and then derives project energy use, onsite supply, and
#' leakage consistent with AMS-II.Q requirements.
#'
#' @param n_buildings Number of commercial buildings to simulate.
#' @param seed Optional integer used to seed the random number generator.
#' @return A list with `baseline_data`, `project_data`, and `leakage_data`
#'   tibbles.
#' @examples
#' simulate_ams_iiq_inputs(n_buildings = 2, seed = 42)
#' @export
simulate_ams_iiq_inputs <- function(n_buildings = 4, seed = NULL) {
  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv)
    old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (is.null(old_seed)) {
        rm(.Random.seed, envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  building_ids <- paste0("B", seq_len(n_buildings))
  service_output <- stats::runif(n_buildings, min = 450, max = 900)
  baseline_intensity <- stats::rnorm(n_buildings, mean = 1.15, sd = 0.1)
  baseline_energy <- baseline_intensity * service_output
  baseline_emission_factor <- stats::runif(n_buildings, min = 0.58, max = 0.66)

  savings_fraction <- stats::runif(n_buildings, min = 0.18, max = 0.32)
  onsite_fraction <- stats::runif(n_buildings, min = 0.05, max = 0.15)
  project_energy <- baseline_energy * (1 - savings_fraction)
  project_emission_factor <- stats::runif(n_buildings, min = 0.52, max = 0.6)
  project_service_output <- service_output
  onsite_energy <- project_energy * onsite_fraction
  project_energy <- project_energy - onsite_energy
  onsite_emission_factor <- stats::runif(n_buildings, min = 0.045, max = 0.06)

  leakage <- onsite_energy * onsite_emission_factor * stats::runif(n_buildings, min = 0.02, max = 0.05)

  baseline_data <- tibble::tibble(
    building_id = building_ids,
    baseline_energy_use_mwh = baseline_energy,
    baseline_emission_factor_tco2_per_mwh = baseline_emission_factor,
    baseline_service_output_mwh = project_service_output
  )

  project_data <- tibble::tibble(
    building_id = building_ids,
    project_energy_use_mwh = project_energy,
    project_emission_factor_tco2_per_mwh = project_emission_factor,
    project_service_output_mwh = project_service_output,
    project_onsite_energy_gj = onsite_energy,
    project_onsite_emission_factor_tco2_per_gj = onsite_emission_factor
  )

  leakage_data <- tibble::tibble(
    building_id = building_ids,
    leakage_emissions_tco2e = leakage
  )

  list(
    baseline_data = baseline_data,
    project_data = project_data,
    leakage_data = leakage_data
  )
}
