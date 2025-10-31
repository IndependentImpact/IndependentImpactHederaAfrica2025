#' Aggregate monitoring periods for ACM0019
#'
#' Summarises nitric acid plant monitoring data, computes baseline, project,
#' leakage, and net emission reductions, and aggregates to the supplied
#' monitoring periods.
#'
#' @param monitoring_data Data frame or tibble containing at least the columns
#'   `period`, `production_tonnes`, `baseline_ef_kg_per_tonne`,
#'   `project_ef_kg_per_tonne`, `electricity_mwh`, `grid_ef_t_per_mwh`,
#'   `steam_tonnes`, and `steam_ef_t_per_tonne`.
#'
#' @return Tibble with period-level totals including emissions components and
#'   net reductions.
#' @examples
#' monitoring <- simulate_acm0019_dataset(4, seed = 99)
#' aggregate_monitoring_periods_acm0019(monitoring)
#' @export
aggregate_monitoring_periods_acm0019 <- function(monitoring_data) {
  required_columns <- c(
    "period",
    "production_tonnes",
    "baseline_ef_kg_per_tonne",
    "project_ef_kg_per_tonne",
    "electricity_mwh",
    "grid_ef_t_per_mwh",
    "steam_tonnes",
    "steam_ef_t_per_tonne"
  )

  missing <- setdiff(required_columns, names(monitoring_data))
  if (length(missing) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }

  safe_weighted_mean <- function(x, w) {
    if (all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
      return(mean(x, na.rm = TRUE))
    }
    stats::weighted.mean(x, w, na.rm = TRUE)
  }

  monitoring_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      baseline_emissions = calculate_baseline_emissions_acm0019(
        production_tonnes,
        baseline_ef_kg_per_tonne
      ),
      project_emissions = calculate_project_emissions_acm0019(
        production_tonnes,
        project_ef_kg_per_tonne
      ),
      leakage_emissions = calculate_leakage_emissions_acm0019(
        electricity_mwh,
        grid_ef_t_per_mwh,
        steam_tonnes,
        steam_ef_t_per_tonne
      ),
      net_emission_reductions = calculate_net_emission_reductions_acm0019(
        baseline_emissions,
        project_emissions,
        leakage_emissions
      ),
      production_weight = production_tonnes,
      electricity_weight = electricity_mwh,
      steam_weight = steam_tonnes
    ) |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      production_tonnes = sum(production_tonnes, na.rm = TRUE),
      baseline_ef_kg_per_tonne = safe_weighted_mean(
        baseline_ef_kg_per_tonne,
        production_weight
      ),
      project_ef_kg_per_tonne = safe_weighted_mean(
        project_ef_kg_per_tonne,
        production_weight
      ),
      electricity_mwh = sum(electricity_mwh, na.rm = TRUE),
      grid_ef_t_per_mwh = safe_weighted_mean(
        grid_ef_t_per_mwh,
        electricity_weight
      ),
      steam_tonnes = sum(steam_tonnes, na.rm = TRUE),
      steam_ef_t_per_tonne = safe_weighted_mean(
        steam_ef_t_per_tonne,
        steam_weight
      ),
      baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      project_emissions = sum(project_emissions, na.rm = TRUE),
      leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      net_emission_reductions = sum(net_emission_reductions, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Estimate total emission reductions under ACM0019
#'
#' Aggregates monitoring data to compute total baseline, project, leakage, and
#' net emission reductions for reporting.
#'
#' @inheritParams aggregate_monitoring_periods_acm0019
#' @return Tibble with one row summarising total results.
#' @examples
#' monitoring <- simulate_acm0019_dataset(6, seed = 101)
#' estimate_emission_reductions_acm0019(monitoring)
#' @export
estimate_emission_reductions_acm0019 <- function(monitoring_data) {
  aggregated <- aggregate_monitoring_periods_acm0019(monitoring_data)

  aggregated |>
    dplyr::summarise(
      total_production_tonnes = sum(production_tonnes, na.rm = TRUE),
      total_baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
      total_project_emissions = sum(project_emissions, na.rm = TRUE),
      total_leakage_emissions = sum(leakage_emissions, na.rm = TRUE),
      total_net_emission_reductions = sum(net_emission_reductions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
