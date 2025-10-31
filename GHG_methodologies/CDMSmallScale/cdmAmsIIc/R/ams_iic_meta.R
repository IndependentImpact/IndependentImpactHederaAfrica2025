#' Estimate emission reductions under AMS-II.C
#'
#' Chains the AMS-II.C equation helpers to compute emission reductions for demand
#' side efficiency projects. Provide baseline and project energy observations (in
#' MWh) along with the emission factor for the displaced electricity or fuel.
#'
#' @param baseline_data Tibble containing baseline energy observations.
#' @param project_data Tibble containing project energy observations.
#' @param emission_factor Numeric scalar or vector of emission factors in
#'   tCO2e/MWh applied to the energy savings.
#' @param group_cols Optional character vector of grouping columns shared between
#'   baseline and project data.
#' @param baseline_energy_col Column name storing baseline energy values.
#' @param project_energy_col Column name storing project energy values.
#' @return A tibble containing baseline energy, project energy, energy savings,
#'   and emission reductions.
#' @examples
#' baseline <- tibble::tibble(site = c("A", "B"), baseline_energy_mwh = c(46, 28))
#' project <- tibble::tibble(site = c("A", "B"), project_energy_mwh = c(24, 18))
#' estimate_emission_reductions_ams_iic(baseline, project, emission_factor = 0.68)
#' @export
estimate_emission_reductions_ams_iic <- function(baseline_data,
                                                 project_data,
                                                 emission_factor,
                                                 group_cols = NULL,
                                                 baseline_energy_col = "baseline_energy_mwh",
                                                 project_energy_col = "project_energy_mwh") {
  baseline_totals <- calculate_baseline_energy_consumption(
    baseline_data,
    energy_col = baseline_energy_col,
    group_cols = group_cols,
    output_col = baseline_energy_col
  )

  project_totals <- calculate_project_energy_consumption(
    project_data,
    energy_col = project_energy_col,
    group_cols = group_cols,
    output_col = project_energy_col
  )

  savings <- calculate_energy_savings(
    baseline_totals,
    project_totals,
    baseline_col = baseline_energy_col,
    project_col = project_energy_col,
    output_col = "energy_savings_mwh"
  )

  calculate_emission_reductions(
    energy_savings = savings,
    savings_col = "energy_savings_mwh",
    emission_factor = emission_factor,
    output_col = "emission_reductions_tco2e"
  )
}

#' Aggregate AMS-II.C monitoring data by period
#'
#' Summarises baseline and project energy measurements across monitoring periods
#' and applies emission factors to derive period-level emission reductions.
#'
#' @param efficiency_data Tibble containing monitoring records with columns for
#'   grouping, monitoring period, baseline energy, project energy, and emission
#'   factors.
#' @param monitoring_cols Character vector defining the monitoring-period columns
#'   (e.g. `c("year", "month")`).
#' @param group_cols Character vector defining entity identifiers (e.g.
#'   `site_id`).
#' @param baseline_energy_col Column storing baseline energy in MWh.
#' @param project_energy_col Column storing project energy in MWh.
#' @param emission_factor_col Column storing emission factors in tCO2e/MWh.
#' @return A tibble grouped by entity and monitoring period with baseline energy,
#'   project energy, energy savings, and emission reductions.
#' @examples
#' data <- simulate_ams_iic_dataset(n_sites = 3, n_periods = 4)
#' aggregate_monitoring_periods(
#'   data,
#'   monitoring_cols = c("year", "month"),
#'   group_cols = "site_id"
#' )
#' @export
aggregate_monitoring_periods <- function(efficiency_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "site_id",
                                         baseline_energy_col = "baseline_energy_mwh",
                                         project_energy_col = "project_energy_mwh",
                                         emission_factor_col = "emission_factor_tco2e_mwh") {
  data_tbl <- tibble::as_tibble(efficiency_data)

  keys <- unique(c(group_cols, monitoring_cols))
  required_cols <- unique(c(
    keys,
    baseline_energy_col,
    project_energy_col,
    emission_factor_col
  ))

  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`efficiency_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  baseline_sym <- rlang::sym(baseline_energy_col)
  project_sym <- rlang::sym(project_energy_col)
  emission_sym <- rlang::sym(emission_factor_col)

  summarised <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      baseline_energy_mwh = sum(!!baseline_sym, na.rm = TRUE),
      project_energy_mwh = sum(!!project_sym, na.rm = TRUE),
      emission_factor_tco2e_mwh = dplyr::first(!!emission_sym),
      .groups = "drop"
    )

  baseline_totals <- summarised |>
    dplyr::select(dplyr::all_of(keys), baseline_energy_mwh)
  project_totals <- summarised |>
    dplyr::select(dplyr::all_of(keys), project_energy_mwh)

  savings <- calculate_energy_savings(
    baseline_totals,
    project_totals,
    baseline_col = "baseline_energy_mwh",
    project_col = "project_energy_mwh",
    output_col = "energy_savings_mwh"
  )

  savings <- dplyr::left_join(
    savings,
    summarised |>
      dplyr::select(dplyr::all_of(keys), emission_factor_tco2e_mwh),
    by = keys
  )

  calculate_emission_reductions(
    energy_savings = savings,
    savings_col = "energy_savings_mwh",
    emission_factor = savings$emission_factor_tco2e_mwh,
    output_col = "emission_reductions_tco2e"
  )
}
