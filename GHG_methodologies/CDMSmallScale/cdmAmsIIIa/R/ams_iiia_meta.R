#' Estimate emission reductions for AMS-III.A projects
#'
#' This meta-function composes the AMS-III.A equation helpers to transform
#' baseline fertilizer use, project monitoring data, and leakage observations into
#' net emission reductions. It accepts tidy data frames and returns a tibble with
#' baseline emissions, project emissions, leakage, and emission reductions.
#'
#' @param baseline_data Tibble containing baseline fertilizer use observations.
#' @param project_data Tibble containing project-period monitoring data.
#' @param leakage_data Optional tibble containing leakage emissions in tCO2e.
#' @param group_cols Optional character vector of grouping columns shared across
#'   inputs (e.g. `farm_id`).
#' @param baseline_fertilizer_col Column storing baseline synthetic nitrogen use (kg N).
#' @param baseline_production_ef_col Column storing upstream emission factors for
#'   baseline fertilizer production (tCO2e per kg N).
#' @param baseline_field_ef_col Column storing baseline direct soil emission
#'   factors (tCO2e per kg N).
#' @param project_fertilizer_col Column storing the residual synthetic nitrogen
#'   applied during the project (kg N).
#' @param project_production_ef_col Column storing emission factors for project
#'   fertilizer production (tCO2e per kg N).
#' @param project_field_ef_col Column storing project direct soil emission factors
#'   (tCO2e per kg N).
#' @param inoculant_rate_col Column storing inoculant application rates (kg/ha).
#' @param area_planted_col Column storing legume area planted during the project (ha).
#' @param inoculant_emission_factor_col Column storing emission factors for inoculant
#'   production and application (tCO2e per kg).
#' @param leakage_col Column storing leakage emissions (tCO2e) when
#'   `leakage_data` is supplied.
#' @return Tibble containing baseline emissions, project emissions, leakage, and
#'   net emission reductions.
#' @examples
#' baseline <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   baseline_synthetic_n_kg = c(120, 90),
#'   baseline_production_ef_tco2_per_kg = 0.004,
#'   baseline_field_ef_tco2_per_kg = 0.01
#' )
#' project <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   project_synthetic_n_kg = c(20, 15),
#'   project_production_ef_tco2_per_kg = 0.004,
#'   project_field_ef_tco2_per_kg = 0.01,
#'   inoculant_rate_kg_per_ha = c(0.5, 0.4),
#'   legume_area_ha = c(30, 25),
#'   inoculant_ef_tco2_per_kg = 0.002
#' )
#' estimate_emission_reductions_ams_iiia(
#'   baseline,
#'   project,
#'   group_cols = "farm_id"
#' )
#' @export
estimate_emission_reductions_ams_iiia <- function(baseline_data,
                                                  project_data,
                                                  leakage_data = NULL,
                                                  group_cols = NULL,
                                                  baseline_fertilizer_col = "baseline_synthetic_n_kg",
                                                  baseline_production_ef_col = "baseline_production_ef_tco2_per_kg",
                                                  baseline_field_ef_col = "baseline_field_ef_tco2_per_kg",
                                                  project_fertilizer_col = "project_synthetic_n_kg",
                                                  project_production_ef_col = "project_production_ef_tco2_per_kg",
                                                  project_field_ef_col = "project_field_ef_tco2_per_kg",
                                                  inoculant_rate_col = "inoculant_rate_kg_per_ha",
                                                  area_planted_col = "legume_area_ha",
                                                  inoculant_emission_factor_col = "inoculant_ef_tco2_per_kg",
                                                  leakage_col = "leakage_emissions_tco2e") {
  join_cols <- if (is.null(group_cols)) character() else group_cols

  baseline <- calculate_baseline_fertilizer_emissions_iiia(
    data = baseline_data,
    fertilizer_use_col = baseline_fertilizer_col,
    production_emission_factor_col = baseline_production_ef_col,
    field_emission_factor_col = baseline_field_ef_col,
    group_cols = group_cols
  )

  project_fertilizer <- calculate_project_residual_fertilizer_emissions_iiia(
    data = project_data,
    fertilizer_use_col = project_fertilizer_col,
    production_emission_factor_col = project_production_ef_col,
    field_emission_factor_col = project_field_ef_col,
    group_cols = group_cols
  )

  project_inoculant <- calculate_project_inoculant_emissions_iiia(
    data = project_data,
    inoculant_rate_col = inoculant_rate_col,
    area_planted_col = area_planted_col,
    inoculant_emission_factor_col = inoculant_emission_factor_col,
    group_cols = group_cols
  )

  project_combined <- if (length(join_cols) == 0) {
    dplyr::bind_cols(project_fertilizer, project_inoculant)
  } else {
    project_fertilizer |>
      dplyr::left_join(project_inoculant, by = join_cols)
  }

  leakage <- if (is.null(leakage_data)) {
    NULL
  } else {
    calculate_leakage_emissions_iiia(
      data = leakage_data,
      leakage_col = leakage_col,
      group_cols = group_cols
    )
  }

  calculate_emission_reductions_iiia(
    baseline_emissions = baseline,
    project_emissions = project_combined,
    leakage_emissions = leakage,
    group_cols = group_cols
  )
}
