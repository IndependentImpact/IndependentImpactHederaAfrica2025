#' Calculate baseline emissions for AMS-III.AJ projects
#'
#' Baseline emissions for recycling activities are based on the virgin material
#' production that would have occurred without the project and the disposal of
#' recyclable material in the baseline scenario. The helper converts processed
#' material tonnage into avoided emissions using virgin material and disposal
#' emission factors. Optional residual fractions and operating days can be
#' supplied to reflect monitoring data granularity.
#'
#' @param data Tibble containing baseline monitoring data.
#' @param material_processed_col Column storing recycled material processed in
#'   tonnes per record. Defaults to `"material_processed_tonnes"`.
#' @param virgin_emission_factor_col Column storing virgin material emission
#'   factors in tCO2 per tonne. Defaults to
#'   `"virgin_material_ef_tco2_per_tonne"`.
#' @param disposal_emission_factor_col Column storing baseline disposal
#'   emission factors in tCO2 per tonne. Defaults to
#'   `"baseline_disposal_ef_tco2_per_tonne"`.
#' @param residual_fraction_col Optional column storing the fraction of
#'   processed material that would remain as residual waste in the baseline.
#' @param days_col Optional column storing the number of days represented by
#'   each record. When supplied, tonnage values are scaled by the number of
#'   days.
#' @param group_cols Optional character vector with grouping columns.
#' @param output_col Name of the output column for baseline emissions in tCO2e.
#' @return Tibble containing grouped baseline emissions in tCO2e.
#' @examples
#' library(tibble)
#' baseline <- tibble(
#'   facility_id = c("F1", "F1", "F2"),
#'   material_processed_tonnes = c(40, 42, 36),
#'   virgin_material_ef_tco2_per_tonne = c(1.8, 1.8, 1.6),
#'   baseline_disposal_ef_tco2_per_tonne = c(0.3, 0.3, 0.28),
#'   residual_fraction = c(0.05, 0.05, 0.06)
#' )
#' calculate_baseline_emissions_iiiaj(baseline, group_cols = "facility_id")
#' @export
calculate_baseline_emissions_iiiaj <- function(data,
                                              material_processed_col = "material_processed_tonnes",
                                              virgin_emission_factor_col = "virgin_material_ef_tco2_per_tonne",
                                              disposal_emission_factor_col = "baseline_disposal_ef_tco2_per_tonne",
                                              residual_fraction_col = "residual_fraction",
                                              days_col = NULL,
                                              group_cols = NULL,
                                              output_col = "baseline_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  material_processed_col <- resolve_column(material_processed_col)
  virgin_emission_factor_col <- resolve_column(virgin_emission_factor_col)
  disposal_emission_factor_col <- resolve_column(disposal_emission_factor_col)
  residual_fraction_col <- resolve_optional_column(residual_fraction_col)
  days_col <- resolve_optional_column(days_col)
  groups <- ensure_group_cols(group_cols)

  emissions <- data_tbl |>
    dplyr::mutate(
      .days = if (!is.null(days_col)) .data[[days_col]] else 1,
      .processed_tonnes = .data[[material_processed_col]] * .days,
      .virgin_component = .processed_tonnes * .data[[virgin_emission_factor_col]],
      .residual_component = if (!is.null(residual_fraction_col)) {
        .processed_tonnes * .data[[residual_fraction_col]] * .data[[disposal_emission_factor_col]]
      } else {
        0
      },
      .baseline_component = .virgin_component + .residual_component
    )

  summarise_emission_component(emissions, groups, output_col, ".baseline_component")
}

#' Calculate project emissions for AMS-III.AJ projects
#'
#' Project emissions account for the energy used by recycling facilities to
#' process collected waste, including electricity, thermal fuels, and optional
#' supplementary material inputs. The helper aggregates energy consumption into
#' tCO2e using supplied emission factors.
#'
#' @inheritParams calculate_baseline_emissions_iiiaj
#' @param electricity_consumption_col Column storing electricity consumption in
#'   MWh. Defaults to `"electricity_consumption_mwh"`.
#' @param electricity_emission_factor_col Column storing electricity emission
#'   factors in tCO2 per MWh. Defaults to
#'   `"electricity_ef_tco2_per_mwh"`.
#' @param thermal_fuel_consumption_col Optional column storing thermal fuel
#'   consumption in GJ. Defaults to `"thermal_fuel_consumption_gj"`.
#' @param thermal_fuel_emission_factor_col Optional column storing emission
#'   factors for the thermal fuel in tCO2 per GJ. Defaults to
#'   `"thermal_fuel_ef_tco2_per_gj"`.
#' @param supplementary_material_col Optional column storing additive or
#'   supplementary material in tonnes. Defaults to
#'   `"supplementary_material_tonnes"`.
#' @param supplementary_material_ef_col Optional column storing emission
#'   factors for supplementary material in tCO2 per tonne. Defaults to
#'   `"supplementary_material_ef_tco2_per_tonne"`.
#' @param output_col Name of the output column for project emissions.
#' @return Tibble containing grouped project emissions in tCO2e.
#' @examples
#' project <- tibble::tibble(
#'   facility_id = c("F1", "F1", "F2"),
#'   electricity_consumption_mwh = c(12, 13, 11),
#'   electricity_ef_tco2_per_mwh = c(0.45, 0.45, 0.48),
#'   thermal_fuel_consumption_gj = c(5, 6, 4.5),
#'   thermal_fuel_ef_tco2_per_gj = c(0.07, 0.07, 0.068)
#' )
#' calculate_project_emissions_iiiaj(project, group_cols = "facility_id")
#' @export
calculate_project_emissions_iiiaj <- function(data,
                                              electricity_consumption_col = "electricity_consumption_mwh",
                                              electricity_emission_factor_col = "electricity_ef_tco2_per_mwh",
                                              thermal_fuel_consumption_col = "thermal_fuel_consumption_gj",
                                              thermal_fuel_emission_factor_col = "thermal_fuel_ef_tco2_per_gj",
                                              supplementary_material_col = "supplementary_material_tonnes",
                                              supplementary_material_ef_col = "supplementary_material_ef_tco2_per_tonne",
                                              group_cols = NULL,
                                              output_col = "project_emissions_tco2e",
                                              days_col = NULL) {
  data_tbl <- tibble::as_tibble(data)
  electricity_consumption_col <- resolve_column(electricity_consumption_col)
  electricity_emission_factor_col <- resolve_column(electricity_emission_factor_col)
  thermal_fuel_consumption_col <- resolve_optional_column(thermal_fuel_consumption_col)
  thermal_fuel_emission_factor_col <- resolve_optional_column(thermal_fuel_emission_factor_col)
  supplementary_material_col <- resolve_optional_column(supplementary_material_col)
  supplementary_material_ef_col <- resolve_optional_column(supplementary_material_ef_col)
  days_col <- resolve_optional_column(days_col)
  groups <- ensure_group_cols(group_cols)

  emissions <- data_tbl |>
    dplyr::mutate(
      .days = if (!is.null(days_col)) .data[[days_col]] else 1,
      .electricity_component = .data[[electricity_consumption_col]] *
        .data[[electricity_emission_factor_col]] * .days,
      .thermal_component = if (!is.null(thermal_fuel_consumption_col) &&
          !is.null(thermal_fuel_emission_factor_col)) {
        .data[[thermal_fuel_consumption_col]] * .data[[thermal_fuel_emission_factor_col]] * .days
      } else {
        0
      },
      .supplementary_component = if (!is.null(supplementary_material_col) &&
          !is.null(supplementary_material_ef_col)) {
        .data[[supplementary_material_col]] * .data[[supplementary_material_ef_col]] * .days
      } else {
        0
      },
      .project_component = .electricity_component + .thermal_component + .supplementary_component
    )

  summarise_emission_component(emissions, groups, output_col, ".project_component")
}

#' Calculate leakage emissions for AMS-III.AJ projects
#'
#' Leakage emissions capture logistics emissions, residual disposal from the
#' project, and potential market effects where recycled materials displace other
#' recycling facilities. Positive values increase leakage while negative market
#' adjustments can reduce it.
#'
#' @param data Tibble containing leakage-related data.
#' @param collected_material_col Column storing collected material transported in
#'   tonnes. Defaults to `"material_collected_tonnes"`.
#' @param transport_distance_col Column storing average one-way transport
#'   distances in kilometres. Defaults to `"transport_distance_km"`.
#' @param transport_ef_col Column storing transport emission factors in tCO2 per
#'   tonne-kilometre. Defaults to `"transport_ef_tco2_per_tkm"`.
#' @param residual_disposal_col Column storing residual waste requiring disposal
#'   in tonnes. Defaults to `"residual_disposal_tonnes"`.
#' @param residual_disposal_ef_col Column storing residual disposal emission
#'   factors in tCO2 per tonne. Defaults to
#'   `"residual_disposal_ef_tco2_per_tonne"`.
#' @param market_leakage_fraction_col Optional column storing the fraction of
#'   recycled output causing market leakage. Defaults to
#'   `"market_leakage_fraction"`.
#' @param market_emission_factor_col Optional column storing emission factors for
#'   displaced recycling in tCO2 per tonne. Defaults to
#'   `"market_emission_factor_tco2_per_tonne"`.
#' @param group_cols Optional character vector with grouping columns.
#' @param output_col Name of the output column for leakage emissions.
#' @return Tibble containing grouped leakage emissions in tCO2e.
#' @examples
#' leakage <- tibble::tibble(
#'   facility_id = c("F1", "F2"),
#'   material_collected_tonnes = c(100, 80),
#'   transport_distance_km = c(25, 18),
#'   transport_ef_tco2_per_tkm = c(0.00012, 0.00011),
#'   residual_disposal_tonnes = c(8, 6),
#'   residual_disposal_ef_tco2_per_tonne = c(0.32, 0.3)
#' )
#' calculate_leakage_emissions_iiiaj(leakage)
#' @export
calculate_leakage_emissions_iiiaj <- function(data,
                                              collected_material_col = "material_collected_tonnes",
                                              transport_distance_col = "transport_distance_km",
                                              transport_ef_col = "transport_ef_tco2_per_tkm",
                                              residual_disposal_col = "residual_disposal_tonnes",
                                              residual_disposal_ef_col = "residual_disposal_ef_tco2_per_tonne",
                                              market_leakage_fraction_col = "market_leakage_fraction",
                                              market_emission_factor_col = "market_emission_factor_tco2_per_tonne",
                                              group_cols = NULL,
                                              output_col = "leakage_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  collected_material_col <- resolve_column(collected_material_col)
  transport_distance_col <- resolve_column(transport_distance_col)
  transport_ef_col <- resolve_column(transport_ef_col)
  residual_disposal_col <- resolve_column(residual_disposal_col)
  residual_disposal_ef_col <- resolve_column(residual_disposal_ef_col)
  market_leakage_fraction_col <- resolve_optional_column(market_leakage_fraction_col)
  market_emission_factor_col <- resolve_optional_column(market_emission_factor_col)
  groups <- ensure_group_cols(group_cols)

  emissions <- data_tbl |>
    dplyr::mutate(
      .transport_component = .data[[collected_material_col]] *
        .data[[transport_distance_col]] *
        .data[[transport_ef_col]],
      .residual_component = .data[[residual_disposal_col]] *
        .data[[residual_disposal_ef_col]],
      .market_component = if (!is.null(market_leakage_fraction_col) &&
          !is.null(market_emission_factor_col)) {
        .data[[collected_material_col]] *
          .data[[market_leakage_fraction_col]] *
          .data[[market_emission_factor_col]]
      } else {
        0
      },
      .leakage_component = .transport_component + .residual_component + .market_component
    )

  summarise_emission_component(emissions, groups, output_col, ".leakage_component")
}

#' Calculate net emission reductions for AMS-III.AJ projects
#'
#' The helper merges baseline, project, and leakage emission tables using the
#' supplied grouping columns and computes net emission reductions.
#'
#' @param baseline_emissions Tibble returned by
#'   [calculate_baseline_emissions_iiiaj()].
#' @param project_emissions Tibble returned by
#'   [calculate_project_emissions_iiiaj()].
#' @param leakage_emissions Tibble returned by
#'   [calculate_leakage_emissions_iiiaj()].
#' @param group_cols Optional character vector with grouping columns.
#' @param output_col Name of the output column for net emission reductions.
#' @return Tibble with net emission reductions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(facility_id = "F1", baseline_emissions_tco2e = 100)
#' project <- tibble::tibble(facility_id = "F1", project_emissions_tco2e = 35)
#' leakage <- tibble::tibble(facility_id = "F1", leakage_emissions_tco2e = 5)
#' calculate_emission_reductions_iiiaj(baseline, project, leakage, group_cols = "facility_id")
#' @export
calculate_emission_reductions_iiiaj <- function(baseline_emissions,
                                               project_emissions,
                                               leakage_emissions,
                                               group_cols = NULL,
                                               output_col = "net_emission_reductions_tco2e") {
  groups <- ensure_group_cols(group_cols)
  baseline_tbl <- tibble::as_tibble(baseline_emissions)
  project_tbl <- tibble::as_tibble(project_emissions)
  leakage_tbl <- tibble::as_tibble(leakage_emissions)

  output_sym <- rlang::sym(output_col)
  reductions <- baseline_tbl |>
    dplyr::left_join(project_tbl, by = groups) |>
    dplyr::left_join(leakage_tbl, by = groups) |>
    dplyr::mutate(
      !!output_sym := .data[["baseline_emissions_tco2e"]] -
        .data[["project_emissions_tco2e"]] -
        .data[["leakage_emissions_tco2e"]]
    )

  reductions
}

#' Aggregate monitoring periods for AMS-III.AJ data
#'
#' Aggregates monitoring data across reporting periods while preserving the
#' specified grouping columns. Numeric measurement columns are summed while
#' non-numeric columns are kept as distinct identifiers.
#'
#' @param data Tibble containing monitoring-period level data.
#' @param group_cols Character vector specifying grouping columns.
#' @param sum_cols Optional character vector of columns to sum. When `NULL`, all
#'   numeric columns except grouping variables are summed.
#' @return Tibble aggregated across monitoring periods.
#' @examples
#' monitoring <- tibble::tibble(
#'   facility_id = c("F1", "F1", "F2"),
#'   period = c(1, 2, 1),
#'   material_processed_tonnes = c(40, 42, 38),
#'   electricity_consumption_mwh = c(12, 13, 11)
#' )
#' aggregate_monitoring_periods_iiiaj(monitoring, group_cols = "facility_id")
#' @export
aggregate_monitoring_periods_iiiaj <- function(data,
                                              group_cols,
                                              sum_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  groups <- ensure_group_cols(group_cols)
  if (is.null(sum_cols)) {
    sum_cols <- setdiff(colnames(data_tbl), groups)
    sum_cols <- sum_cols[vapply(data_tbl[sum_cols], is.numeric, logical(1))]
  }

  aggregated <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(sum_cols), sum, na.rm = TRUE), .groups = "drop")

  aggregated
}

summarise_emission_component <- function(data, group_cols, output_col, component_col) {
  output_sym <- rlang::sym(output_col)
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      !!output_sym := sum(.data[[component_col]], na.rm = TRUE),
      .groups = "drop"
    )
}

resolve_column <- function(column) {
  if (rlang::is_string(column)) {
    column
  } else {
    rlang::as_name(rlang::enquo(column))
  }
}

resolve_optional_column <- function(column) {
  if (is.null(column)) {
    return(NULL)
  }
  resolve_column(column)
}

enforce_named_list <- function(args) {
  if (is.null(args)) {
    return(list())
  }
  if (!rlang::is_named(args)) {
    stop("All additional arguments must be named", call. = FALSE)
  }
  args
}

ensure_group_cols <- function(group_cols) {
  if (is.null(group_cols)) {
    character()
  } else if (is.character(group_cols)) {
    group_cols
  } else {
    stop("`group_cols` must be NULL or a character vector", call. = FALSE)
  }
}
