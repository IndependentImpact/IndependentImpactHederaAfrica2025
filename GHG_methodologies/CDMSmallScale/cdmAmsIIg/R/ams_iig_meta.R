#' Run the AMS-II.G emission reduction workflow
#'
#' Chains the AMS-II.G equation helpers to convert baseline and project biomass
#' monitoring data into emission reductions. Users provide baseline and project
#' datasets containing biomass consumption, non-renewable fractions, net
#' calorific values, and emission factors. Optional leakage estimates can be
#' supplied to capture upstream biomass effects.
#'
#' @param baseline_data Tibble containing baseline monitoring observations.
#' @param project_data Tibble containing project monitoring observations.
#' @param leakage_data Optional tibble containing leakage emissions.
#' @param group_cols Optional character vector of grouping columns shared across
#'   baseline, project, and leakage datasets.
#' @param baseline_consumption_col Column storing baseline biomass consumption
#'   (tonnes).
#' @param baseline_fraction_col Column storing the baseline fraction of
#'   non-renewable biomass.
#' @param baseline_ncv_col Column storing the baseline net calorific value in MJ
#'   per tonne.
#' @param baseline_emission_factor_col Column storing the baseline emission factor
#'   in tCO2e/MJ.
#' @param project_consumption_col Column storing project biomass consumption
#'   (tonnes).
#' @param project_fraction_col Column storing the project non-renewable biomass
#'   fraction.
#' @param project_ncv_col Column storing the project net calorific value in MJ per
#'   tonne.
#' @param project_emission_factor_col Column storing the project emission factor
#'   in tCO2e/MJ.
#' @param leakage_col Column storing leakage emissions in tCO2e when
#'   `leakage_data` is supplied.
#' @return Tibble containing baseline and project non-renewable biomass, thermal
#'   energy, emissions, leakage, and net emission reductions.
#' @examples
#' baseline <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   baseline_biomass_consumption_tonnes = c(12, 14),
#'   baseline_non_renewable_fraction = c(0.82, 0.88),
#'   baseline_net_calorific_value_mj_per_tonne = c(15.2, 14.8),
#'   baseline_emission_factor_tco2_per_mj = c(0.00009, 0.000092)
#' )
#' project <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   project_biomass_consumption_tonnes = c(7.4, 8.0),
#'   project_non_renewable_fraction = c(0.41, 0.39),
#'   project_net_calorific_value_mj_per_tonne = c(15.4, 15.0),
#'   project_emission_factor_tco2_per_mj = c(0.00009, 0.000092)
#' )
#' estimate_emission_reductions_ams_iig(
#'   baseline,
#'   project,
#'   group_cols = "site_id"
#' )
#' @export
estimate_emission_reductions_ams_iig <- function(baseline_data,
                                                 project_data,
                                                 leakage_data = NULL,
                                                 group_cols = NULL,
                                                 baseline_consumption_col = "baseline_biomass_consumption_tonnes",
                                                 baseline_fraction_col = "baseline_non_renewable_fraction",
                                                 baseline_ncv_col = "baseline_net_calorific_value_mj_per_tonne",
                                                 baseline_emission_factor_col = "baseline_emission_factor_tco2_per_mj",
                                                 project_consumption_col = "project_biomass_consumption_tonnes",
                                                 project_fraction_col = "project_non_renewable_fraction",
                                                 project_ncv_col = "project_net_calorific_value_mj_per_tonne",
                                                 project_emission_factor_col = "project_emission_factor_tco2_per_mj",
                                                 leakage_col = "leakage_emissions_tco2e") {
  baseline_nrb <- calculate_baseline_non_renewable_biomass_iig(
    baseline_data = baseline_data,
    consumption_col = baseline_consumption_col,
    fraction_col = baseline_fraction_col,
    group_cols = group_cols
  )

  project_nrb <- calculate_project_non_renewable_biomass_iig(
    project_data = project_data,
    project_consumption_col = project_consumption_col,
    project_fraction_col = project_fraction_col,
    group_cols = group_cols
  )

  baseline_energy <- calculate_baseline_thermal_energy_iig(
    non_renewable_biomass = baseline_nrb,
    baseline_data = baseline_data,
    ncv_col = baseline_ncv_col,
    group_cols = group_cols
  )

  project_energy <- calculate_project_thermal_energy_iig(
    non_renewable_biomass = project_nrb,
    project_data = project_data,
    ncv_col = project_ncv_col,
    group_cols = group_cols
  )

  baseline_emissions <- calculate_emissions_from_energy_iig(
    energy_data = baseline_energy,
    energy_col = "baseline_thermal_energy_mj",
    factor_data = baseline_data,
    emission_factor_col = baseline_emission_factor_col,
    group_cols = group_cols,
    output_col = "baseline_emissions_tco2e"
  )

  project_emissions <- calculate_emissions_from_energy_iig(
    energy_data = project_energy,
    energy_col = "project_thermal_energy_mj",
    factor_data = project_data,
    emission_factor_col = project_emission_factor_col,
    group_cols = group_cols,
    output_col = "project_emissions_tco2e"
  )

  if (is.null(group_cols) || length(group_cols) == 0) {
    join_cols <- character()
  } else {
    join_cols <- group_cols
  }

  leakage_totals <- if (is.null(leakage_data)) {
    base_tbl <- if (length(join_cols) == 0) {
      tibble::tibble(dummy = 1)
    } else {
      baseline_nrb |>
        dplyr::select(dplyr::all_of(join_cols))
    }
    base_tbl[["leakage_emissions_tco2e"]] <- 0
    if (length(join_cols) == 0) {
      base_tbl <- dplyr::select(base_tbl, "leakage_emissions_tco2e")
    }
    base_tbl
  } else {
    calculate_leakage_emissions_iig(
      leakage_data = leakage_data,
      leakage_col = leakage_col,
      group_cols = group_cols,
      output_col = "leakage_emissions_tco2e"
    )
  }

  components <- if (length(join_cols) == 0) {
    dplyr::bind_cols(
      baseline_nrb,
      project_nrb,
      baseline_energy,
      project_energy,
      baseline_emissions,
      project_emissions,
      leakage_totals
    )
  } else {
    baseline_nrb |>
      dplyr::left_join(project_nrb, by = join_cols) |>
      dplyr::left_join(baseline_energy, by = join_cols) |>
      dplyr::left_join(project_energy, by = join_cols) |>
      dplyr::left_join(baseline_emissions, by = join_cols) |>
      dplyr::left_join(project_emissions, by = join_cols) |>
      dplyr::left_join(leakage_totals, by = join_cols)
  }

  reductions <- calculate_emission_reductions_iig(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions,
    leakage_emissions = leakage_totals,
    group_cols = group_cols
  )

  if (length(join_cols) == 0) {
    dplyr::bind_cols(components, reductions)
  } else {
    components |>
      dplyr::left_join(reductions, by = join_cols)
  }
}

#' Aggregate monitoring periods for AMS-II.G
#'
#' Summarises monitoring data across reporting periods by aggregating biomass
#' consumption, updating mean non-renewable fractions, and summing leakage. The
#' helper returns a tidy tibble containing baseline and project inputs ready for
#' `estimate_emission_reductions_ams_iig()`.
#'
#' @param monitoring_data Tibble containing monitoring period observations.
#' @param group_cols Character vector of entity identifiers (e.g. household IDs).
#' @param period_cols Character vector defining the monitoring period (e.g. year,
#'   month).
#' @param baseline_consumption_col Column storing baseline biomass consumption.
#' @param baseline_fraction_col Column storing the baseline non-renewable
#'   fraction.
#' @param baseline_ncv_col Column storing the baseline NCV in MJ per tonne.
#' @param baseline_emission_factor_col Column storing the baseline emission factor
#'   in tCO2e/MJ.
#' @param project_consumption_col Column storing project biomass consumption.
#' @param project_fraction_col Column storing the project non-renewable fraction.
#' @param project_ncv_col Column storing the project NCV in MJ per tonne.
#' @param project_emission_factor_col Column storing the project emission factor
#'   in tCO2e/MJ.
#' @param leakage_col Column storing leakage emissions in tCO2e.
#' @return Tibble aggregated by the combination of `group_cols` and `period_cols`.
#' @examples
#' data <- simulate_ams_iig_dataset(n_sites = 2, n_periods = 3, seed = 42)
#' aggregate_monitoring_periods_iig(data)
#' @export
aggregate_monitoring_periods_iig <- function(monitoring_data,
                                             group_cols = "site_id",
                                             period_cols = c("year", "month"),
                                             baseline_consumption_col = "baseline_biomass_consumption_tonnes",
                                             baseline_fraction_col = "baseline_non_renewable_fraction",
                                             baseline_ncv_col = "baseline_net_calorific_value_mj_per_tonne",
                                             baseline_emission_factor_col = "baseline_emission_factor_tco2_per_mj",
                                             project_consumption_col = "project_biomass_consumption_tonnes",
                                             project_fraction_col = "project_non_renewable_fraction",
                                             project_ncv_col = "project_net_calorific_value_mj_per_tonne",
                                             project_emission_factor_col = "project_emission_factor_tco2_per_mj",
                                             leakage_col = "leakage_emissions_tco2e") {
  data_tbl <- dplyr::as_tibble(monitoring_data)

  required_cols <- unique(c(
    group_cols,
    period_cols,
    baseline_consumption_col,
    baseline_fraction_col,
    baseline_ncv_col,
    baseline_emission_factor_col,
    project_consumption_col,
    project_fraction_col,
    project_ncv_col,
    project_emission_factor_col,
    leakage_col
  ))

  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`monitoring_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  grouping <- unique(c(group_cols, period_cols))

  consumption_sym <- rlang::sym(baseline_consumption_col)
  project_consumption_sym <- rlang::sym(project_consumption_col)
  fraction_sym <- rlang::sym(baseline_fraction_col)
  project_fraction_sym <- rlang::sym(project_fraction_col)
  ncv_sym <- rlang::sym(baseline_ncv_col)
  project_ncv_sym <- rlang::sym(project_ncv_col)
  emission_factor_sym <- rlang::sym(baseline_emission_factor_col)
  project_emission_factor_sym <- rlang::sym(project_emission_factor_col)
  leakage_sym <- rlang::sym(leakage_col)

  data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
    dplyr::summarise(
      !!baseline_consumption_col := sum(dplyr::coalesce(!!consumption_sym, 0), na.rm = TRUE),
      !!project_consumption_col := sum(dplyr::coalesce(!!project_consumption_sym, 0), na.rm = TRUE),
      !!baseline_fraction_col := {
        total_baseline <- sum(dplyr::coalesce(!!consumption_sym, 0), na.rm = TRUE)
        if (total_baseline > 0) {
          sum(dplyr::coalesce(!!consumption_sym, 0) * dplyr::coalesce(!!fraction_sym, 0), na.rm = TRUE) /
            total_baseline
        } else {
          mean(dplyr::coalesce(!!fraction_sym, 0), na.rm = TRUE)
        }
      },
      !!project_fraction_col := {
        total_project <- sum(dplyr::coalesce(!!project_consumption_sym, 0), na.rm = TRUE)
        if (total_project > 0) {
          sum(dplyr::coalesce(!!project_consumption_sym, 0) * dplyr::coalesce(!!project_fraction_sym, 0), na.rm = TRUE) /
            total_project
        } else {
          mean(dplyr::coalesce(!!project_fraction_sym, 0), na.rm = TRUE)
        }
      },
      !!baseline_ncv_col := mean(dplyr::coalesce(!!ncv_sym, 0), na.rm = TRUE),
      !!project_ncv_col := mean(dplyr::coalesce(!!project_ncv_sym, 0), na.rm = TRUE),
      !!baseline_emission_factor_col := mean(dplyr::coalesce(!!emission_factor_sym, 0), na.rm = TRUE),
      !!project_emission_factor_col := mean(dplyr::coalesce(!!project_emission_factor_sym, 0), na.rm = TRUE),
      !!leakage_col := sum(dplyr::coalesce(!!leakage_sym, 0), na.rm = TRUE),
      .groups = "drop"
    )
}
