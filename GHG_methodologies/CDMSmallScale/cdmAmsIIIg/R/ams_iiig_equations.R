#' Calculate baseline methane emissions for AMS-III.G projects
#'
#' Baseline emissions from landfill sites under AMS-III.G are determined by the
#' methane that would have been released without the recovery system. The helper
#' estimates methane generation from disposed waste and applies optional
#' baseline recovery and oxidation fractions before converting to tCO2e using the
#' methane density and global warming potential of methane.
#'
#' @param data Tibble containing baseline landfill monitoring data.
#' @param waste_disposed_col Column storing waste disposed in tonnes per period.
#' @param methane_generation_potential_col Column storing the methane generation
#'   potential in cubic metres per tonne of waste.
#' @param baseline_collection_efficiency_col Optional column storing the
#'   fraction of methane recovered in the baseline scenario. Defaults to
#'   `"baseline_collection_efficiency_fraction"`.
#' @param oxidation_fraction_col Optional column storing the oxidation fraction
#'   of methane passing through the landfill cover. Defaults to
#'   `"oxidation_fraction"`.
#' @param days_col Optional column storing the number of days represented by
#'   each record. When supplied the generated methane is scaled by the number of
#'   days.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column for baseline emissions in tCO2e.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#'   Defaults to 0.00067 t CH4 per m3.
#' @param gwp_ch4 Global warming potential of methane. Defaults to 28.
#' @return Tibble containing grouped baseline methane emissions in tCO2e.
#' @examples
#' library(tibble)
#' baseline <- tibble(
#'   site_id = c("LF1", "LF2"),
#'   waste_disposed_tonnes = c(12000, 9000),
#'   methane_generation_potential_m3_per_tonne = c(90, 85),
#'   oxidation_fraction = c(0.1, 0.05)
#' )
#' calculate_baseline_methane_emissions_iiig(baseline, group_cols = "site_id")
#' @export
calculate_baseline_methane_emissions_iiig <- function(data,
                                                      waste_disposed_col = "waste_disposed_tonnes",
                                                      methane_generation_potential_col = "methane_generation_potential_m3_per_tonne",
                                                      baseline_collection_efficiency_col = "baseline_collection_efficiency_fraction",
                                                      oxidation_fraction_col = "oxidation_fraction",
                                                      days_col = NULL,
                                                      group_cols = NULL,
                                                      output_col = "baseline_emissions_tco2e",
                                                      methane_density_t_per_m3 = 0.00067,
                                                      gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  waste_disposed_col <- resolve_column(waste_disposed_col)
  methane_generation_potential_col <- resolve_column(methane_generation_potential_col)
  baseline_collection_efficiency_col <- resolve_optional_column(baseline_collection_efficiency_col)
  oxidation_fraction_col <- resolve_optional_column(oxidation_fraction_col)
  days_col <- resolve_optional_column(days_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .days = if (!is.null(days_col)) .data[[days_col]] else 1,
      .generated_m3 = .data[[waste_disposed_col]] *
        .data[[methane_generation_potential_col]] *
        .days,
      .captured_m3 = if (!is.null(baseline_collection_efficiency_col)) {
        .generated_m3 * .data[[baseline_collection_efficiency_col]]
      } else {
        0
      },
      .oxidised_m3 = if (!is.null(oxidation_fraction_col)) {
        (.generated_m3 - .captured_m3) * .data[[oxidation_fraction_col]]
      } else {
        0
      },
      .emitted_m3 = pmax(.generated_m3 - .captured_m3 - .oxidised_m3, 0),
      .baseline_component = .emitted_m3 * methane_density_t_per_m3 * gwp_ch4
    )

  summarise_emission_component(emissions, groups, output_col, ".baseline_component")
}

#' Calculate project methane emissions for AMS-III.G projects
#'
#' Project emissions account for residual methane that escapes collection and
#' destruction under the project as well as onsite energy used to operate the
#' recovery system. The helper converts methane balances and optional energy
#' inputs into tCO2e.
#'
#' @inheritParams calculate_baseline_methane_emissions_iiig
#' @param project_collection_efficiency_col Column storing the fraction of
#'   methane collected under the project scenario.
#' @param destruction_efficiency_col Column storing the destruction efficiency of
#'   the flare or energy utilisation equipment.
#' @param project_oxidation_fraction_col Optional column storing the oxidation
#'   fraction for uncaptured methane under the project.
#' @param electricity_consumption_col Optional column storing electricity use in
#'   MWh for operating the landfill gas system.
#' @param electricity_emission_factor_col Optional column storing grid emission
#'   factors in tCO2 per MWh.
#' @param diesel_consumption_col Optional column storing diesel consumption in
#'   litres for onsite equipment.
#' @param diesel_emission_factor_col Optional column storing emission factors in
#'   tCO2 per litre of diesel.
#' @param output_col Name of the output column for project emissions in tCO2e.
#' @return Tibble containing grouped project emissions in tCO2e.
#' @examples
#' project <- tibble::tibble(
#'   site_id = c("LF1", "LF2"),
#'   waste_disposed_tonnes = c(12000, 9000),
#'   methane_generation_potential_m3_per_tonne = c(90, 85),
#'   project_collection_efficiency_fraction = c(0.65, 0.6),
#'   destruction_efficiency_fraction = c(0.98, 0.96),
#'   oxidation_fraction = c(0.1, 0.08)
#' )
#' calculate_project_emissions_iiig(project, group_cols = "site_id")
#' @export
calculate_project_emissions_iiig <- function(data,
                                             waste_disposed_col = "waste_disposed_tonnes",
                                             methane_generation_potential_col = "methane_generation_potential_m3_per_tonne",
                                             project_collection_efficiency_col = "project_collection_efficiency_fraction",
                                             destruction_efficiency_col = "destruction_efficiency_fraction",
                                             project_oxidation_fraction_col = "oxidation_fraction",
                                             days_col = NULL,
                                             electricity_consumption_col = "electricity_consumption_mwh",
                                             electricity_emission_factor_col = "electricity_ef_tco2_per_mwh",
                                             diesel_consumption_col = "diesel_consumption_litres",
                                             diesel_emission_factor_col = "diesel_ef_tco2_per_litre",
                                             group_cols = NULL,
                                             output_col = "project_emissions_tco2e",
                                             methane_density_t_per_m3 = 0.00067,
                                             gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  waste_disposed_col <- resolve_column(waste_disposed_col)
  methane_generation_potential_col <- resolve_column(methane_generation_potential_col)
  project_collection_efficiency_col <- resolve_column(project_collection_efficiency_col)
  destruction_efficiency_col <- resolve_column(destruction_efficiency_col)
  project_oxidation_fraction_col <- resolve_optional_column(project_oxidation_fraction_col)
  days_col <- resolve_optional_column(days_col)
  electricity_consumption_col <- resolve_optional_column(electricity_consumption_col)
  electricity_emission_factor_col <- resolve_optional_column(electricity_emission_factor_col)
  diesel_consumption_col <- resolve_optional_column(diesel_consumption_col)
  diesel_emission_factor_col <- resolve_optional_column(diesel_emission_factor_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .days = if (!is.null(days_col)) .data[[days_col]] else 1,
      .generated_m3 = .data[[waste_disposed_col]] *
        .data[[methane_generation_potential_col]] *
        .days,
      .captured_m3 = .generated_m3 * .data[[project_collection_efficiency_col]],
      .destroyed_m3 = .captured_m3 * .data[[destruction_efficiency_col]],
      .oxidised_m3 = if (!is.null(project_oxidation_fraction_col)) {
        (.generated_m3 - .captured_m3) * .data[[project_oxidation_fraction_col]]
      } else {
        0
      },
      .residual_m3 = pmax(.generated_m3 - .destroyed_m3 - .oxidised_m3, 0),
      .methane_component = .residual_m3 * methane_density_t_per_m3 * gwp_ch4,
      .electricity_component = if (!is.null(electricity_consumption_col) && !is.null(electricity_emission_factor_col)) {
        .data[[electricity_consumption_col]] * .data[[electricity_emission_factor_col]]
      } else {
        0
      },
      .diesel_component = if (!is.null(diesel_consumption_col) && !is.null(diesel_emission_factor_col)) {
        .data[[diesel_consumption_col]] * .data[[diesel_emission_factor_col]]
      } else {
        0
      },
      .project_component = .methane_component + .electricity_component + .diesel_component
    )

  summarise_emission_component(emissions, groups, output_col, ".project_component")
}

#' Calculate leakage emissions for AMS-III.G projects
#'
#' Leakage emissions in AMS-III.G typically arise from transporting residual
#' waste, handling recovered gas off-site, or displacing fossil-based energy.
#' This helper aggregates transport, residual waste treatment, and optional
#' displacement credits to produce a net leakage estimate.
#'
#' @param data Tibble containing leakage monitoring data.
#' @param waste_transported_col Column storing transported waste in tonnes.
#' @param transport_distance_col Column storing the average transport distance in
#'   kilometres.
#' @param transport_emission_factor_col Column storing emission factors in tCO2
#'   per tonne-kilometre.
#' @param residual_waste_col Optional column storing residual waste mass in
#'   tonnes that is treated outside the project boundary.
#' @param residual_waste_emission_factor_col Optional column storing emission
#'   factors for residual waste treatment in tCO2 per tonne.
#' @param displaced_electricity_col Optional column storing electricity exported
#'   or displaced in MWh.
#' @param displaced_electricity_emission_factor_col Optional column storing the
#'   corresponding emission factor in tCO2 per MWh (credited as negative
#'   leakage).
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column for leakage emissions in tCO2e.
#' @return Tibble containing grouped leakage emissions in tCO2e.
#' @examples
#' leakage <- tibble::tibble(
#'   site_id = c("LF1", "LF2"),
#'   waste_transported_tonnes = c(500, 450),
#'   transport_distance_km = c(20, 35),
#'   transport_ef_tco2_per_tkm = c(0.00012, 0.0001)
#' )
#' calculate_leakage_emissions_iiig(leakage, group_cols = "site_id")
#' @export
calculate_leakage_emissions_iiig <- function(data,
                                             waste_transported_col = "waste_transported_tonnes",
                                             transport_distance_col = "transport_distance_km",
                                             transport_emission_factor_col = "transport_ef_tco2_per_tkm",
                                             residual_waste_col = "residual_waste_tonnes",
                                             residual_waste_emission_factor_col = "residual_waste_ef_tco2_per_tonne",
                                             displaced_electricity_col = "displaced_electricity_mwh",
                                             displaced_electricity_emission_factor_col = "displaced_electricity_ef_tco2_per_mwh",
                                             group_cols = NULL,
                                             output_col = "leakage_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  waste_transported_col <- resolve_column(waste_transported_col)
  transport_distance_col <- resolve_column(transport_distance_col)
  transport_emission_factor_col <- resolve_column(transport_emission_factor_col)
  residual_waste_col <- resolve_optional_column(residual_waste_col)
  residual_waste_emission_factor_col <- resolve_optional_column(residual_waste_emission_factor_col)
  displaced_electricity_col <- resolve_optional_column(displaced_electricity_col)
  displaced_electricity_emission_factor_col <- resolve_optional_column(displaced_electricity_emission_factor_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .transport_component = .data[[waste_transported_col]] *
        .data[[transport_distance_col]] *
        .data[[transport_emission_factor_col]],
      .residual_component = if (!is.null(residual_waste_col) && !is.null(residual_waste_emission_factor_col)) {
        .data[[residual_waste_col]] * .data[[residual_waste_emission_factor_col]]
      } else {
        0
      },
      .displacement_component = if (!is.null(displaced_electricity_col) && !is.null(displaced_electricity_emission_factor_col)) {
        .data[[displaced_electricity_col]] * .data[[displaced_electricity_emission_factor_col]]
      } else {
        0
      },
      .leakage_component = .transport_component + .residual_component - .displacement_component
    )

  summarise_emission_component(emissions, groups, output_col, ".leakage_component")
}

#' Aggregate monitoring periods for AMS-III.G
#'
#' Aggregates period-level emissions outputs generated by the equation helpers
#' while counting distinct monitoring periods per grouping combination.
#'
#' @param data Tibble containing period-level emissions.
#' @param group_cols Character vector identifying grouping columns.
#' @param monitoring_col Column storing the monitoring period identifier.
#' @return Tibble summarising emissions by group with monitoring period counts.
#' @examples
#' period_results <- tibble::tibble(
#'   site_id = rep("LF1", 2),
#'   period = 1:2,
#'   baseline_emissions_tco2e = c(1500, 1480),
#'   project_emissions_tco2e = c(350, 340),
#'   leakage_emissions_tco2e = c(20, 25)
#' )
#' aggregate_monitoring_periods_iiig(period_results, group_cols = "site_id", monitoring_col = "period")
#' @export
aggregate_monitoring_periods_iiig <- function(data,
                                              group_cols,
                                              monitoring_col = "monitoring_period") {
  if (missing(group_cols) || length(group_cols) == 0) {
    stop("`group_cols` must be supplied to aggregate monitoring periods.")
  }
  data_tbl <- tibble::as_tibble(data)
  monitoring_col <- resolve_column(monitoring_col)

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

#' Calculate emission reductions for AMS-III.G
#'
#' Combines baseline, project, and optional leakage emissions to estimate net
#' emission reductions. Grouping columns are preserved when provided.
#'
#' @param baseline_emissions Tibble produced by
#'   `calculate_baseline_methane_emissions_iiig`.
#' @param project_emissions Tibble produced by
#'   `calculate_project_emissions_iiig`.
#' @param leakage_emissions Optional tibble produced by
#'   `calculate_leakage_emissions_iiig`.
#' @param group_cols Optional character vector identifying grouping columns.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' baseline <- tibble::tibble(baseline_emissions_tco2e = 1800)
#' project <- tibble::tibble(project_emissions_tco2e = 420)
#' calculate_emission_reductions_iiig(baseline, project)
#' @export
calculate_emission_reductions_iiig <- function(baseline_emissions,
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

# Helper utilities ---------------------------------------------------------

resolve_column <- function(column) {
  if (rlang::is_string(column)) {
    column
  } else {
    rlang::as_name(rlang::enquo(column))
  }
}

resolve_optional_column <- function(column) {
  if (is.null(column)) {
    NULL
  } else if (rlang::is_string(column)) {
    column
  } else {
    rlang::as_name(rlang::enquo(column))
  }
}

summarise_emission_component <- function(emissions, groups, output_col, component_col) {
  target <- rlang::sym(output_col)
  if (length(groups) == 0) {
    tibble::tibble(!!target := sum(emissions[[component_col]], na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!target := sum(.data[[component_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }
}
