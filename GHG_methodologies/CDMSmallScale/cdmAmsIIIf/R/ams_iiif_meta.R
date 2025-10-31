#' Estimate emission reductions for AMS-III.F projects
#'
#' This meta-function composes the AMS-III.F helpers to transform monitoring
#' inputs into net emission reductions. Provide baseline, project, and optional
#' leakage datasets along with argument lists that customise column mappings or
#' methodological constants.
#'
#' @param baseline_data Tibble containing baseline waste generation data.
#' @param project_data Tibble containing monitored composting operation data.
#' @param leakage_data Optional tibble containing leakage monitoring data.
#' @param group_cols Optional character vector identifying grouping columns.
#' @param baseline_args Named list of arguments passed to
#'   `calculate_baseline_methane_emissions_iiif`.
#' @param project_args Named list of arguments passed to
#'   `calculate_project_emissions_iiif`.
#' @param leakage_args Named list of arguments passed to
#'   `calculate_leakage_emissions_iiif`.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' monitoring <- simulate_ams_iiif_dataset(n_sites = 1, n_periods = 2, seed = 42)
#' estimate_emission_reductions_ams_iiif(
#'   baseline_data = monitoring$baseline,
#'   project_data = monitoring$project,
#'   leakage_data = monitoring$leakage,
#'   group_cols = "site_id"
#' )
#' @export
estimate_emission_reductions_ams_iiif <- function(baseline_data,
                                                  project_data,
                                                  leakage_data = NULL,
                                                  group_cols = NULL,
                                                  baseline_args = list(),
                                                  project_args = list(),
                                                  leakage_args = list()) {
  baseline_tbl <- do.call(
    calculate_baseline_methane_emissions_iiif,
    c(list(data = baseline_data, group_cols = group_cols), baseline_args)
  )

  project_tbl <- do.call(
    calculate_project_emissions_iiif,
    c(list(data = project_data, group_cols = group_cols), project_args)
  )

  leakage_tbl <- if (is.null(leakage_data)) {
    NULL
  } else {
    do.call(
      calculate_leakage_emissions_iiif,
      c(list(data = leakage_data, group_cols = group_cols), leakage_args)
    )
  }

  calculate_emission_reductions_iiif(
    baseline_emissions = baseline_tbl,
    project_emissions = project_tbl,
    leakage_emissions = leakage_tbl,
    group_cols = group_cols
  )
}
