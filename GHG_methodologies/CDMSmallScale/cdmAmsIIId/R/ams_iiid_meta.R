#' Aggregate monitoring periods for AMS-III.D datasets
#'
#' Monitoring data for AMS-III.D typically contains multiple observations per
#' digester or farm. This helper aggregates tidy inputs to the grouping level used
#' in the emission calculations by summing flow variables and averaging
#' coefficients.
#'
#' @param data Tibble containing period-level monitoring data.
#' @param group_cols Character vector of grouping columns (e.g. `farm_id`).
#' @param sum_cols Character vector of numeric columns to sum across monitoring
#'   periods. Defaults include volatile solids, recovered methane, leakage, and
#'   days in period.
#' @param mean_cols Character vector of numeric columns to average across periods
#'   (e.g. methane potential, methane conversion factors, capture efficiencies).
#' @param na_rm Logical indicating whether to remove missing values when
#'   summarising. Defaults to `TRUE`.
#' @return Tibble aggregated to the specified grouping columns.
#' @examples
#' monitoring <- tibble::tibble(
#'   farm_id = rep("A", 2),
#'   monitoring_period = c(1, 2),
#'   volatile_solids_kg_per_day = c(40, 42),
#'   methane_recovered_m3 = c(1200, 1300),
#'   days_in_period = c(30, 30),
#'   methane_potential_m3_per_kg_vs = c(0.23, 0.24),
#'   project_mcf_fraction = c(0.6, 0.58)
#' )
#' aggregate_monitoring_periods_iiid(monitoring, group_cols = "farm_id")
#' @export
aggregate_monitoring_periods_iiid <- function(data,
                                              group_cols,
                                              sum_cols = c(
                                                "volatile_solids_kg_per_day",
                                                "methane_recovered_m3",
                                                "leakage_emissions_tco2e",
                                                "days_in_period"
                                              ),
                                              mean_cols = c(
                                                "methane_potential_m3_per_kg_vs",
                                                "baseline_mcf_fraction",
                                                "project_mcf_fraction",
                                                "capture_efficiency_fraction",
                                                "destruction_efficiency_fraction"
                                              ),
                                              na_rm = TRUE) {
  data_tbl <- tibble::as_tibble(data)
  groups <- group_cols

  summarised <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups)))

  sum_cols_present <- intersect(sum_cols, names(data_tbl))
  if (!is.null(sum_cols_present) && length(sum_cols_present) > 0) {
    summarised <- summarised |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(sum_cols_present),
          ~ sum(.x, na.rm = na_rm),
          .names = "{.col}"
        ),
        .groups = "drop_last"
      )
  } else {
    summarised <- summarised |>
      dplyr::summarise(.groups = "drop_last")
  }

  mean_cols_present <- intersect(mean_cols, names(data_tbl))
  if (!is.null(mean_cols_present) && length(mean_cols_present) > 0) {
    means <- data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(mean_cols_present),
          ~ mean(.x, na.rm = na_rm),
          .names = "{.col}"
        ),
        .groups = "drop"
      )
    summarised <- summarised |>
      dplyr::left_join(means, by = groups)
  } else {
    summarised <- summarised |>
      dplyr::ungroup()
  }

  summarised
}

#' Estimate emission reductions for AMS-III.D projects
#'
#' This meta-function composes the AMS-III.D equation helpers to transform
#' baseline manure management data, project monitoring data, methane recovery
#' observations, and leakage information into net emission reductions.
#'
#' @param baseline_data Tibble containing baseline manure management data.
#' @param project_data Tibble containing project monitoring data.
#' @param recovery_data Optional tibble containing methane recovery data. When
#'   `NULL`, `project_data` is reused.
#' @param leakage_data Optional tibble containing leakage emissions in tCO2e.
#' @param group_cols Optional character vector of grouping columns shared across
#'   all inputs.
#' @param volatile_solids_col Column storing volatile solids generated per day
#'   (kg VS/day).
#' @param methane_potential_col Column storing methane producing potential
#'   (m3 CH4/kg VS).
#' @param baseline_mcf_col Column storing the baseline methane conversion factor
#'   (fraction).
#' @param project_mcf_col Column storing the project methane conversion factor
#'   (fraction).
#' @param capture_efficiency_col Column storing methane capture efficiency
#'   (fraction).
#' @param destruction_efficiency_col Column storing destruction efficiency for
#'   captured methane (fraction).
#' @param days_col Column storing the number of days in each monitoring period.
#' @param methane_recovered_col Column storing methane recovered in cubic metres.
#' @param leakage_col Column storing leakage emissions in tCO2e.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#' @param gwp_ch4 Global warming potential of methane.
#' @return Tibble containing emission reductions in tCO2e per group.
#' @examples
#' baseline <- tibble::tibble(
#'   farm_id = "A",
#'   volatile_solids_kg_per_day = 40,
#'   methane_potential_m3_per_kg_vs = 0.25,
#'   baseline_mcf_fraction = 0.85,
#'   days_in_period = 365
#' )
#' project <- tibble::tibble(
#'   farm_id = "A",
#'   volatile_solids_kg_per_day = 40,
#'   methane_potential_m3_per_kg_vs = 0.25,
#'   project_mcf_fraction = 0.6,
#'   capture_efficiency_fraction = 0.9,
#'   destruction_efficiency_fraction = 0.98,
#'   days_in_period = 365,
#'   methane_recovered_m3 = 15000
#' )
#' estimate_emission_reductions_ams_iiid(
#'   baseline,
#'   project,
#'   group_cols = "farm_id"
#' )
#' @export
estimate_emission_reductions_ams_iiid <- function(baseline_data,
                                                  project_data,
                                                  recovery_data = NULL,
                                                  leakage_data = NULL,
                                                  group_cols = NULL,
                                                  volatile_solids_col = "volatile_solids_kg_per_day",
                                                  methane_potential_col = "methane_potential_m3_per_kg_vs",
                                                  baseline_mcf_col = "baseline_mcf_fraction",
                                                  project_mcf_col = "project_mcf_fraction",
                                                  capture_efficiency_col = "capture_efficiency_fraction",
                                                  destruction_efficiency_col = "destruction_efficiency_fraction",
                                                  days_col = "days_in_period",
                                                  methane_recovered_col = "methane_recovered_m3",
                                                  leakage_col = "leakage_emissions_tco2e",
                                                  methane_density_t_per_m3 = 0.00067,
                                                  gwp_ch4 = 28) {
  group_cols_join <- if (is.null(group_cols)) character() else group_cols
  recovery_data <- if (is.null(recovery_data)) project_data else recovery_data

  baseline <- calculate_baseline_methane_emissions_iiid(
    data = baseline_data,
    volatile_solids_col = volatile_solids_col,
    methane_potential_col = methane_potential_col,
    methane_conversion_factor_col = baseline_mcf_col,
    days_col = days_col,
    group_cols = group_cols,
    methane_density_t_per_m3 = methane_density_t_per_m3,
    gwp_ch4 = gwp_ch4
  )

  project <- calculate_project_methane_emissions_iiid(
    data = project_data,
    volatile_solids_col = volatile_solids_col,
    methane_potential_col = methane_potential_col,
    project_methane_conversion_factor_col = project_mcf_col,
    capture_efficiency_col = capture_efficiency_col,
    destruction_efficiency_col = destruction_efficiency_col,
    days_col = days_col,
    group_cols = group_cols,
    methane_density_t_per_m3 = methane_density_t_per_m3,
    gwp_ch4 = gwp_ch4
  )

  recovered <- calculate_recovered_methane_iiid(
    data = recovery_data,
    methane_recovered_col = methane_recovered_col,
    destruction_efficiency_col = destruction_efficiency_col,
    group_cols = group_cols,
    methane_density_t_per_m3 = methane_density_t_per_m3,
    gwp_ch4 = gwp_ch4
  )

  leakage <- if (is.null(leakage_data)) {
    zero_data <- if (is.null(group_cols)) {
      tibble::tibble(!!leakage_col := 0)
    } else {
      baseline |>
        dplyr::select(dplyr::all_of(group_cols)) |>
        dplyr::distinct() |>
        dplyr::mutate(!!leakage_col := 0)
    }
    calculate_leakage_emissions_iiid(
      data = zero_data,
      leakage_col = leakage_col,
      group_cols = group_cols
    )
  } else {
    calculate_leakage_emissions_iiid(
      data = leakage_data,
      leakage_col = leakage_col,
      group_cols = group_cols
    )
  }

  calculate_emission_reductions_iiid(
    baseline = baseline,
    project = project,
    recovered = recovered,
    leakage = leakage,
    group_cols = group_cols_join
  )
}
