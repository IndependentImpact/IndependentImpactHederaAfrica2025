#' Estimate emission reductions for AMS-III.G projects
#'
#' Orchestrates the baseline, project, and leakage helpers to transform landfill
#' monitoring inputs into net emission reductions. Named argument lists allow the
#' caller to customise column mappings or methodological constants.
#'
#' @param baseline_data Tibble containing baseline landfill data.
#' @param project_data Tibble containing project monitoring data.
#' @param leakage_data Optional tibble containing leakage monitoring data.
#' @param group_cols Optional character vector identifying grouping columns.
#' @param baseline_args Named list of arguments passed to
#'   `calculate_baseline_methane_emissions_iiig`.
#' @param project_args Named list of arguments passed to
#'   `calculate_project_emissions_iiig`.
#' @param leakage_args Named list of arguments passed to
#'   `calculate_leakage_emissions_iiig`.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' monitoring <- simulate_ams_iiig_dataset(n_sites = 1, n_periods = 2, seed = 99)
#' estimate_emission_reductions_ams_iiig(
#'   baseline_data = monitoring$baseline,
#'   project_data = monitoring$project,
#'   leakage_data = monitoring$leakage,
#'   group_cols = "site_id"
#' )
#' @export
estimate_emission_reductions_ams_iiig <- function(baseline_data,
                                                  project_data,
                                                  leakage_data = NULL,
                                                  group_cols = NULL,
                                                  baseline_args = list(),
                                                  project_args = list(),
                                                  leakage_args = list()) {
  baseline_tbl <- do.call(
    calculate_baseline_methane_emissions_iiig,
    c(list(data = baseline_data, group_cols = group_cols), baseline_args)
  )

  project_tbl <- do.call(
    calculate_project_emissions_iiig,
    c(list(data = project_data, group_cols = group_cols), project_args)
  )

  leakage_tbl <- if (is.null(leakage_data)) {
    NULL
  } else {
    do.call(
      calculate_leakage_emissions_iiig,
      c(list(data = leakage_data, group_cols = group_cols), leakage_args)
    )
  }

  calculate_emission_reductions_iiig(
    baseline_emissions = baseline_tbl,
    project_emissions = project_tbl,
    leakage_emissions = leakage_tbl,
    group_cols = group_cols
  )
}
