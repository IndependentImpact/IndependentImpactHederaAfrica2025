#' Calculate baseline methane emissions from unmanaged biomass decay
#'
#' Equation (1) of AMS-III.E estimates baseline emissions from biomass that would
#' have decayed anaerobically in the absence of the project. The helper multiplies
#' biomass quantity, methane generation potential, the fraction subject to
#' anaerobic conditions, and monitoring duration to obtain emissions expressed in
#' tonnes of CO2 equivalent.
#'
#' @param data Tibble containing baseline monitoring data.
#' @param biomass_col Column storing biomass quantity (tonnes).
#' @param methane_potential_col Column storing methane generation potential
#'   (m3 CH4 per tonne of biomass).
#' @param decay_fraction_col Column storing the fraction of biomass that would
#'   undergo anaerobic decay in the baseline scenario.
#' @param days_col Column storing the number of days covered by each record.
#' @param group_cols Optional character vector specifying grouping columns
#'   (e.g. plant identifier).
#' @param output_col Name of the output column containing baseline emissions in
#'   tCO2e. Defaults to `"baseline_emissions_tco2e"`.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#'   Defaults to 0.00067 t CH4/m3.
#' @param gwp_ch4 Global warming potential of methane. Defaults to 28.
#' @return Tibble containing grouped baseline methane emissions.
#' @examples
#' library(tibble)
#' baseline <- tibble(
#'   plant_id = c("A", "B"),
#'   biomass_tonnes = c(1200, 900),
#'   methane_potential_m3_per_tonne = c(120, 110),
#'   anaerobic_decay_fraction = c(0.65, 0.55),
#'   days_in_period = 365
#' )
#' calculate_baseline_methane_emissions_iiie(baseline)
#' @export
calculate_baseline_methane_emissions_iiie <- function(data,
                                                      biomass_col = "biomass_tonnes",
                                                      methane_potential_col = "methane_potential_m3_per_tonne",
                                                      decay_fraction_col = "anaerobic_decay_fraction",
                                                      days_col = "days_in_period",
                                                      group_cols = NULL,
                                                      output_col = "baseline_emissions_tco2e",
                                                      methane_density_t_per_m3 = 0.00067,
                                                      gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  biomass_col <- if (rlang::is_string(biomass_col)) biomass_col else rlang::as_name(rlang::enquo(biomass_col))
  methane_potential_col <- if (rlang::is_string(methane_potential_col)) methane_potential_col else rlang::as_name(rlang::enquo(methane_potential_col))
  decay_fraction_col <- if (rlang::is_string(decay_fraction_col)) decay_fraction_col else rlang::as_name(rlang::enquo(decay_fraction_col))
  days_col <- if (rlang::is_string(days_col)) days_col else rlang::as_name(rlang::enquo(days_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .baseline_component = .data[[biomass_col]] *
        .data[[methane_potential_col]] *
        .data[[decay_fraction_col]] *
        .data[[days_col]] *
        methane_density_t_per_m3 *
        gwp_ch4 / 365
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.baseline_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.baseline_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate project emissions for AMS-III.E biomass treatment
#'
#' Equation (2) of AMS-III.E captures project emissions from methane slip during
#' thermal treatment alongside auxiliary fossil fuel use. The helper converts
#' treated biomass and efficiency parameters into methane emissions and adds
#' auxiliary and backup fuel contributions.
#'
#' @inheritParams calculate_baseline_methane_emissions_iiie
#' @param treated_biomass_col Column storing biomass treated in the project scenario
#'   (tonnes).
#' @param methane_slip_fraction_col Column storing the fraction of methane not
#'   destroyed by the technology (dimensionless).
#' @param auxiliary_fuel_consumption_col Column storing auxiliary fuel
#'   consumption (TJ).
#' @param auxiliary_fuel_emission_factor_col Column storing the emission factor
#'   for auxiliary fuel (tCO2/TJ).
#' @param fossil_backup_consumption_col Optional column storing backup fossil
#'   fuel use (TJ). Set to `NULL` to ignore.
#' @param fossil_backup_ef_col Optional column storing emission factors for the
#'   backup fuel (tCO2/TJ). Set to `NULL` to ignore.
#' @param output_col Name of the output column containing project emissions in
#'   tCO2e. Defaults to `"project_emissions_tco2e"`.
#' @return Tibble containing grouped project emissions.
#' @examples
#' project <- tibble(
#'   plant_id = c("A", "B"),
#'   treated_biomass_tonnes = c(1100, 880),
#'   methane_potential_m3_per_tonne = c(120, 110),
#'   methane_slip_fraction = c(0.05, 0.04),
#'   auxiliary_fuel_consumption_tj = c(1.2, 0.9),
#'   auxiliary_fuel_ef_tco2_per_tj = 74,
#'   days_in_period = 365
#' )
#' calculate_project_emissions_iiie(project)
#' @export
calculate_project_emissions_iiie <- function(data,
                                             treated_biomass_col = "treated_biomass_tonnes",
                                             methane_potential_col = "methane_potential_m3_per_tonne",
                                             methane_slip_fraction_col = "methane_slip_fraction",
                                             days_col = "days_in_period",
                                             auxiliary_fuel_consumption_col = "auxiliary_fuel_consumption_tj",
                                             auxiliary_fuel_emission_factor_col = "auxiliary_fuel_ef_tco2_per_tj",
                                             fossil_backup_consumption_col = NULL,
                                             fossil_backup_ef_col = NULL,
                                             group_cols = NULL,
                                             output_col = "project_emissions_tco2e",
                                             methane_density_t_per_m3 = 0.00067,
                                             gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  treated_biomass_col <- if (rlang::is_string(treated_biomass_col)) treated_biomass_col else rlang::as_name(rlang::enquo(treated_biomass_col))
  methane_potential_col <- if (rlang::is_string(methane_potential_col)) methane_potential_col else rlang::as_name(rlang::enquo(methane_potential_col))
  methane_slip_fraction_col <- if (rlang::is_string(methane_slip_fraction_col)) methane_slip_fraction_col else rlang::as_name(rlang::enquo(methane_slip_fraction_col))
  days_col <- if (rlang::is_string(days_col)) days_col else rlang::as_name(rlang::enquo(days_col))
  auxiliary_fuel_consumption_col <- if (rlang::is_string(auxiliary_fuel_consumption_col)) auxiliary_fuel_consumption_col else rlang::as_name(rlang::enquo(auxiliary_fuel_consumption_col))
  auxiliary_fuel_emission_factor_col <- if (rlang::is_string(auxiliary_fuel_emission_factor_col)) auxiliary_fuel_emission_factor_col else rlang::as_name(rlang::enquo(auxiliary_fuel_emission_factor_col))
  fossil_backup_consumption_col <- if (is.null(fossil_backup_consumption_col) || rlang::is_string(fossil_backup_consumption_col)) fossil_backup_consumption_col else rlang::as_name(rlang::enquo(fossil_backup_consumption_col))
  fossil_backup_ef_col <- if (is.null(fossil_backup_ef_col) || rlang::is_string(fossil_backup_ef_col)) fossil_backup_ef_col else rlang::as_name(rlang::enquo(fossil_backup_ef_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .methane_slip = .data[[treated_biomass_col]] *
        .data[[methane_potential_col]] *
        .data[[methane_slip_fraction_col]] *
        .data[[days_col]] *
        methane_density_t_per_m3 *
        gwp_ch4 / 365,
      .auxiliary = .data[[auxiliary_fuel_consumption_col]] * .data[[auxiliary_fuel_emission_factor_col]],
      .backup = if (!is.null(fossil_backup_consumption_col) && !is.null(fossil_backup_ef_col)) {
        .data[[fossil_backup_consumption_col]] * .data[[fossil_backup_ef_col]]
      } else {
        0
      },
      .project_component = .methane_slip + .auxiliary + .backup
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.project_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.project_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate leakage emissions for AMS-III.E
#'
#' Equation (3) accounts for leakage arising from biomass transport and
#' displacement of alternative biomass uses. The helper multiplies transported
#' biomass by distance-based emission factors and adds emissions associated with
#' diverting biomass from other applications.
#'
#' @param data Tibble containing leakage monitoring data.
#' @param biomass_transported_col Column storing biomass transported (tonnes).
#' @param transport_distance_col Column storing distance travelled (km).
#' @param transport_emission_factor_col Column storing emission factors (tCO2e per
#'   tonne-km).
#' @param alternative_use_fraction_col Column storing the fraction of biomass that
#'   would have been used in alternative applications.
#' @param alternative_use_emission_factor_col Column storing emission factors
#'   associated with the alternative use (tCO2e per tonne).
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column containing leakage emissions.
#' @return Tibble containing grouped leakage emissions.
#' @examples
#' leakage <- tibble(
#'   plant_id = c("A", "B"),
#'   biomass_transported_tonnes = c(500, 420),
#'   transport_distance_km = c(45, 30),
#'   transport_ef_tco2_per_tkm = 0.00012,
#'   alternative_use_fraction = c(0.1, 0.05),
#'   alternative_use_ef_tco2_per_tonne = 0.5
#' )
#' calculate_leakage_emissions_iiie(leakage)
#' @export
calculate_leakage_emissions_iiie <- function(data,
                                             biomass_transported_col = "biomass_transported_tonnes",
                                             transport_distance_col = "transport_distance_km",
                                             transport_emission_factor_col = "transport_ef_tco2_per_tkm",
                                             alternative_use_fraction_col = "alternative_use_fraction",
                                             alternative_use_emission_factor_col = "alternative_use_ef_tco2_per_tonne",
                                             group_cols = NULL,
                                             output_col = "leakage_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  biomass_transported_col <- if (rlang::is_string(biomass_transported_col)) biomass_transported_col else rlang::as_name(rlang::enquo(biomass_transported_col))
  transport_distance_col <- if (rlang::is_string(transport_distance_col)) transport_distance_col else rlang::as_name(rlang::enquo(transport_distance_col))
  transport_emission_factor_col <- if (rlang::is_string(transport_emission_factor_col)) transport_emission_factor_col else rlang::as_name(rlang::enquo(transport_emission_factor_col))
  alternative_use_fraction_col <- if (rlang::is_string(alternative_use_fraction_col)) alternative_use_fraction_col else rlang::as_name(rlang::enquo(alternative_use_fraction_col))
  alternative_use_emission_factor_col <- if (rlang::is_string(alternative_use_emission_factor_col)) alternative_use_emission_factor_col else rlang::as_name(rlang::enquo(alternative_use_emission_factor_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .transport = .data[[biomass_transported_col]] *
        .data[[transport_distance_col]] *
        .data[[transport_emission_factor_col]],
      .alternative = .data[[biomass_transported_col]] *
        .data[[alternative_use_fraction_col]] *
        .data[[alternative_use_emission_factor_col]],
      .leakage_component = .transport + .alternative
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.leakage_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.leakage_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Aggregate monitoring periods for AMS-III.E
#'
#' AMS-III.E projects typically report monitoring results across several
#' operating periods. This helper aggregates period-level baseline, project, and
#' leakage emissions to the grouping level required for verification.
#'
#' @param data Tibble containing period-level emissions output from the equation
#'   helpers.
#' @param group_cols Character vector identifying grouping columns (e.g.
#'   `"plant_id"`).
#' @param monitoring_col Column storing the monitoring period identifier.
#' @return Tibble summarising emissions by group and counting monitoring periods.
#' @examples
#' period_results <- tibble(
#'   plant_id = rep("A", 2),
#'   period = 1:2,
#'   baseline_emissions_tco2e = c(12, 11),
#'   project_emissions_tco2e = c(4, 3.5),
#'   leakage_emissions_tco2e = c(0.4, 0.3)
#' )
#' aggregate_monitoring_periods_iiie(period_results, group_cols = "plant_id", monitoring_col = "period")
#' @export
aggregate_monitoring_periods_iiie <- function(data,
                                              group_cols,
                                              monitoring_col = "monitoring_period") {
  if (missing(group_cols) || length(group_cols) == 0) {
    stop("`group_cols` must be supplied to aggregate monitoring periods.")
  }
  data_tbl <- tibble::as_tibble(data)
  monitoring_col <- if (rlang::is_string(monitoring_col)) monitoring_col else rlang::as_name(rlang::enquo(monitoring_col))

  data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      monitoring_periods = dplyr::n_distinct(.data[[monitoring_col]]),
      baseline_emissions_tco2e = sum(.data$baseline_emissions_tco2e, na.rm = TRUE),
      project_emissions_tco2e = sum(.data$project_emissions_tco2e, na.rm = TRUE),
      leakage_emissions_tco2e = sum(.data$leakage_emissions_tco2e, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate emission reductions for AMS-III.E
#'
#' This helper combines baseline emissions, project emissions, and leakage to
#' estimate net emission reductions consistent with AMS-III.E reporting.
#'
#' @param baseline_emissions Tibble produced by
#'   `calculate_baseline_methane_emissions_iiie`.
#' @param project_emissions Tibble produced by
#'   `calculate_project_emissions_iiie`.
#' @param leakage_emissions Optional tibble produced by
#'   `calculate_leakage_emissions_iiie`.
#' @param group_cols Optional character vector identifying grouping columns.
#' @return Tibble containing baseline, project, leakage, and net reductions.
#' @examples
#' baseline <- tibble::tibble(baseline_emissions_tco2e = 20)
#' project <- tibble::tibble(project_emissions_tco2e = 5)
#' calculate_emission_reductions_iiie(baseline, project)
#' @export
calculate_emission_reductions_iiie <- function(baseline_emissions,
                                               project_emissions,
                                               leakage_emissions = NULL,
                                               group_cols = NULL) {
  baseline_tbl <- tibble::as_tibble(baseline_emissions)
  project_tbl <- tibble::as_tibble(project_emissions)
  join_cols <- if (is.null(group_cols)) character() else group_cols

  if (length(join_cols) > 0) {
    baseline_tbl <- baseline_tbl |>
      dplyr::select(dplyr::all_of(c(join_cols, "baseline_emissions_tco2e")))
    project_tbl <- project_tbl |>
      dplyr::select(dplyr::all_of(c(join_cols, "project_emissions_tco2e")))
  }

  combined <- if (length(join_cols) == 0) {
    dplyr::bind_cols(baseline_tbl, project_tbl)
  } else {
    baseline_tbl |>
      dplyr::left_join(project_tbl, by = join_cols)
  }

  leakage_tbl <- if (is.null(leakage_emissions)) {
    if (length(join_cols) == 0) {
      tibble::tibble(leakage_emissions_tco2e = 0)
    } else {
      combined |>
        dplyr::select(dplyr::all_of(join_cols)) |>
        dplyr::distinct() |>
        dplyr::mutate(leakage_emissions_tco2e = 0)
    }
  } else {
    leakage_tbl <- tibble::as_tibble(leakage_emissions)
    if (length(join_cols) > 0) {
      leakage_tbl <- leakage_tbl |>
        dplyr::select(dplyr::all_of(c(join_cols, "leakage_emissions_tco2e")))
    } else {
      leakage_tbl <- leakage_tbl |>
        dplyr::select("leakage_emissions_tco2e")
    }
    leakage_tbl
  }

  combined <- if (length(join_cols) == 0) {
    dplyr::bind_cols(combined, leakage_tbl)
  } else {
    combined |>
      dplyr::left_join(leakage_tbl, by = join_cols)
  }

  combined |>
    dplyr::mutate(
      net_emission_reductions_tco2e = .data$baseline_emissions_tco2e -
        .data$project_emissions_tco2e -
        .data$leakage_emissions_tco2e
    )
}
