#' Estimate emission reductions for AMS-III.E projects
#'
#' This meta-function composes the AMS-III.E helpers to transform monitoring
#' inputs into net emission reductions. Supply baseline, project, and optional
#' leakage datasets alongside relevant column mappings.
#'
#' @param baseline_data Tibble containing baseline biomass information.
#' @param project_data Tibble containing project operation data.
#' @param leakage_data Optional tibble containing leakage monitoring data.
#' @param group_cols Optional character vector identifying grouping columns.
#' @param baseline_args Named list of arguments passed to
#'   `calculate_baseline_methane_emissions_iiie`.
#' @param project_args Named list of arguments passed to
#'   `calculate_project_emissions_iiie`.
#' @param leakage_args Named list of arguments passed to
#'   `calculate_leakage_emissions_iiie`.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' monitoring <- simulate_ams_iiie_dataset(n_plants = 1, n_periods = 2, seed = 42)
#' estimate_emission_reductions_ams_iiie(
#'   baseline_data = monitoring$baseline,
#'   project_data = monitoring$project,
#'   leakage_data = monitoring$leakage,
#'   group_cols = "plant_id"
#' )
#' @export
estimate_emission_reductions_ams_iiie <- function(baseline_data,
                                                  project_data,
                                                  leakage_data = NULL,
                                                  group_cols = NULL,
                                                  baseline_args = list(),
                                                  project_args = list(),
                                                  leakage_args = list()) {
  baseline_tbl <- do.call(
    calculate_baseline_methane_emissions_iiie,
    c(list(data = baseline_data, group_cols = group_cols), baseline_args)
  )

  project_tbl <- do.call(
    calculate_project_emissions_iiie,
    c(list(data = project_data, group_cols = group_cols), project_args)
  )

  leakage_tbl <- if (is.null(leakage_data)) {
    NULL
  } else {
    do.call(
      calculate_leakage_emissions_iiie,
      c(list(data = leakage_data, group_cols = group_cols), leakage_args)
    )
  }

  calculate_emission_reductions_iiie(
    baseline_emissions = baseline_tbl,
    project_emissions = project_tbl,
    leakage_emissions = leakage_tbl,
    group_cols = group_cols
  )
}
