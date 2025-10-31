#' Estimate emission reductions for AMS-III.AJ recycling projects
#'
#' Wrapper around the equation-level helpers that orchestrates baseline,
#' project, and leakage calculations before returning net emission reductions.
#' Additional arguments for the underlying helpers can be provided via lists.
#'
#' @param baseline_data Monitoring dataset for baseline emissions.
#' @param project_data Monitoring dataset for project emissions.
#' @param leakage_data Monitoring dataset for leakage emissions.
#' @param group_cols Character vector with the identifiers used across tables.
#' @param baseline_args Optional named list of arguments passed to
#'   [calculate_baseline_emissions_iiiaj()].
#' @param project_args Optional named list of arguments passed to
#'   [calculate_project_emissions_iiiaj()].
#' @param leakage_args Optional named list of arguments passed to
#'   [calculate_leakage_emissions_iiiaj()].
#' @return Tibble containing baseline, project, leakage, and net reductions.
#' @examples
#' monitoring <- simulate_ams_iiiaj_dataset(n_facilities = 2, n_periods = 2, seed = 123)
#' estimate_emission_reductions_ams_iiiaj(
#'   baseline_data = monitoring$baseline,
#'   project_data = monitoring$project,
#'   leakage_data = monitoring$leakage,
#'   group_cols = "facility_id",
#'   baseline_args = list(days_col = "days_in_period"),
#'   project_args = list(days_col = "days_in_period")
#' )
#' @export
estimate_emission_reductions_ams_iiiaj <- function(baseline_data,
                                                   project_data,
                                                   leakage_data,
                                                   group_cols,
                                                   baseline_args = NULL,
                                                   project_args = NULL,
                                                   leakage_args = NULL) {
  groups <- ensure_group_cols(group_cols)
  baseline_args <- enforce_named_list(baseline_args)
  project_args <- enforce_named_list(project_args)
  leakage_args <- enforce_named_list(leakage_args)

  baseline_tbl <- do.call(
    calculate_baseline_emissions_iiiaj,
    c(list(data = baseline_data, group_cols = groups), baseline_args)
  )
  project_tbl <- do.call(
    calculate_project_emissions_iiiaj,
    c(list(data = project_data, group_cols = groups), project_args)
  )
  leakage_tbl <- do.call(
    calculate_leakage_emissions_iiiaj,
    c(list(data = leakage_data, group_cols = groups), leakage_args)
  )

  reductions <- calculate_emission_reductions_iiiaj(
    baseline_tbl,
    project_tbl,
    leakage_tbl,
    group_cols = groups
  )

  reductions
}
