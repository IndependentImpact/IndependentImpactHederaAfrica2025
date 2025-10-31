#' Estimate emission reductions for AMS-III.H projects
#'
#' Coordinates the baseline, project, and leakage helpers for AMS-III.H to
#' transform wastewater monitoring inputs into net emission reductions. Named
#' argument lists allow callers to customise column mappings or methodological
#' constants without rewriting workflows.
#'
#' @param baseline_data Tibble containing baseline wastewater data.
#' @param project_data Tibble containing project monitoring data.
#' @param leakage_data Optional tibble containing leakage monitoring data.
#' @param group_cols Optional character vector identifying grouping columns.
#' @param baseline_args Named list of arguments passed to
#'   `calculate_baseline_methane_emissions_iiih`.
#' @param project_args Named list of arguments passed to
#'   `calculate_project_emissions_iiih`.
#' @param leakage_args Named list of arguments passed to
#'   `calculate_leakage_emissions_iiih`.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' monitoring <- simulate_ams_iiih_dataset(n_sites = 1, n_periods = 2, seed = 99)
#' estimate_emission_reductions_ams_iiih(
#'   baseline_data = monitoring$baseline,
#'   project_data = monitoring$project,
#'   leakage_data = monitoring$leakage,
#'   group_cols = "site_id"
#' )
#' @export
estimate_emission_reductions_ams_iiih <- function(baseline_data,
                                                  project_data,
                                                  leakage_data = NULL,
                                                  group_cols = NULL,
                                                  baseline_args = list(),
                                                  project_args = list(),
                                                  leakage_args = list()) {
  baseline_tbl <- do.call(
    calculate_baseline_methane_emissions_iiih,
    c(list(data = baseline_data, group_cols = group_cols), baseline_args)
  )

  project_tbl <- do.call(
    calculate_project_emissions_iiih,
    c(list(data = project_data, group_cols = group_cols), project_args)
  )

  leakage_tbl <- if (is.null(leakage_data)) {
    NULL
  } else {
    do.call(
      calculate_leakage_emissions_iiih,
      c(list(data = leakage_data, group_cols = group_cols), leakage_args)
    )
  }

  calculate_emission_reductions_iiih(
    baseline_emissions = baseline_tbl,
    project_emissions = project_tbl,
    leakage_emissions = leakage_tbl,
    group_cols = group_cols
  )
}
