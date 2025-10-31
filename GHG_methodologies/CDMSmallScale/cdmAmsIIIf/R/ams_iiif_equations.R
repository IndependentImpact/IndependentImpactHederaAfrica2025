#' Calculate baseline methane emissions for compostable waste
#'
#' Equation (1) of AMS-III.F determines baseline methane generation from solid
#' waste that would have decayed anaerobically in unmanaged disposal sites. The
#' helper multiplies waste quantity, degradable organic carbon parameters, and a
#' methane correction factor before applying the methane fraction and global
#' warming potential to obtain baseline emissions in tonnes of CO2 equivalent.
#'
#' @param data Tibble containing baseline waste characterisation data.
#' @param waste_quantity_col Column storing the quantity of waste subject to the
#'   project activity (tonnes).
#' @param doc_fraction_col Column storing the degradable organic carbon (DOC)
#'   fraction of the waste (tonne C per tonne of waste).
#' @param docf_col Column storing the fraction of DOC that actually degrades
#'   (dimensionless).
#' @param methane_correction_factor_col Column storing the methane correction
#'   factor (MCF) for the baseline disposal site (dimensionless).
#' @param methane_fraction Methane fraction of landfill gas. Defaults to 0.5.
#' @param oxidation_factor_col Optional column containing the oxidation factor
#'   (OX). Set to `NULL` to ignore oxidation.
#' @param group_cols Optional character vector specifying grouping columns (for
#'   example the composting site identifier).
#' @param output_col Name of the output column with baseline emissions in tCO2e.
#'   Defaults to `"baseline_emissions_tco2e"`.
#' @param gwp_ch4 Global warming potential for methane. Defaults to 28.
#' @return Tibble with grouped baseline methane emissions in tCO2e.
#' @examples
#' library(tibble)
#' baseline <- tibble(
#'   site_id = c("A", "B"),
#'   waste_tonnes = c(1000, 850),
#'   doc_fraction = c(0.15, 0.16),
#'   docf_fraction = 0.5,
#'   baseline_mcf_fraction = c(0.8, 0.7)
#' )
#' calculate_baseline_methane_emissions_iiif(baseline)
#' @export
calculate_baseline_methane_emissions_iiif <- function(data,
                                                     waste_quantity_col = "waste_tonnes",
                                                     doc_fraction_col = "doc_fraction",
                                                     docf_col = "docf_fraction",
                                                     methane_correction_factor_col = "baseline_mcf_fraction",
                                                     methane_fraction = 0.5,
                                                     oxidation_factor_col = NULL,
                                                     group_cols = NULL,
                                                     output_col = "baseline_emissions_tco2e",
                                                     gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  waste_quantity_col <- if (rlang::is_string(waste_quantity_col)) waste_quantity_col else rlang::as_name(rlang::enquo(waste_quantity_col))
  doc_fraction_col <- if (rlang::is_string(doc_fraction_col)) doc_fraction_col else rlang::as_name(rlang::enquo(doc_fraction_col))
  docf_col <- if (rlang::is_string(docf_col)) docf_col else rlang::as_name(rlang::enquo(docf_col))
  methane_correction_factor_col <- if (rlang::is_string(methane_correction_factor_col)) methane_correction_factor_col else rlang::as_name(rlang::enquo(methane_correction_factor_col))
  oxidation_factor_col <- if (is.null(oxidation_factor_col) || rlang::is_string(oxidation_factor_col)) oxidation_factor_col else rlang::as_name(rlang::enquo(oxidation_factor_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .oxidation = if (!is.null(oxidation_factor_col)) .data[[oxidation_factor_col]] else 0,
      .baseline_ch4_t = .data[[waste_quantity_col]] *
        .data[[doc_fraction_col]] *
        .data[[docf_col]] *
        .data[[methane_correction_factor_col]] *
        methane_fraction *
        (16 / 12) *
        (1 - .oxidation),
      .baseline_component = .baseline_ch4_t * gwp_ch4
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

#' Calculate project emissions for aerobic composting systems
#'
#' Equation (2) of AMS-III.F represents project emissions as residual methane
#' released from the composting process plus fossil fuel or electricity used to
#' operate the facility. The helper accepts tidy monitoring data and returns
#' grouped project emissions in tCO2e.
#'
#' @inheritParams calculate_baseline_methane_emissions_iiif
#' @param composted_waste_col Column storing the amount of waste composted
#'   (tonnes).
#' @param compost_mcf_col Column storing the methane correction factor achieved
#'   by the controlled composting system (dimensionless).
#' @param compost_oxidation_col Optional column storing the oxidation factor for
#'   the compost matrix. Set to `NULL` to omit.
#' @param electricity_consumption_col Optional column storing electricity use
#'   (MWh) for aeration, turning, or leachate management.
#' @param electricity_emission_factor_col Optional column storing the grid
#'   emission factor associated with electricity (tCO2e per MWh).
#' @param fossil_fuel_consumption_col Optional column storing fossil fuel use
#'   (for example diesel in litres or energy units).
#' @param fossil_fuel_emission_factor_col Optional column storing the emission
#'   factor for the fossil fuel input (tCO2e per unit of consumption).
#' @param output_col Name of the output column with project emissions in tCO2e.
#'   Defaults to `"project_emissions_tco2e"`.
#' @return Tibble with grouped project emissions.
#' @examples
#' project <- tibble(
#'   site_id = c("A", "B"),
#'   composted_waste_tonnes = c(950, 800),
#'   doc_fraction = c(0.15, 0.16),
#'   docf_fraction = 0.5,
#'   compost_mcf_fraction = c(0.1, 0.12),
#'   electricity_mwh = c(40, 32),
#'   grid_ef_tco2_per_mwh = 0.6
#' )
#' calculate_project_emissions_iiif(project)
#' @export
calculate_project_emissions_iiif <- function(data,
                                             composted_waste_col = "composted_waste_tonnes",
                                             doc_fraction_col = "doc_fraction",
                                             docf_col = "docf_fraction",
                                             compost_mcf_col = "compost_mcf_fraction",
                                             methane_fraction = 0.5,
                                             compost_oxidation_col = NULL,
                                             electricity_consumption_col = "electricity_mwh",
                                             electricity_emission_factor_col = "grid_ef_tco2_per_mwh",
                                             fossil_fuel_consumption_col = "diesel_litres",
                                             fossil_fuel_emission_factor_col = "diesel_ef_tco2_per_litre",
                                             group_cols = NULL,
                                             output_col = "project_emissions_tco2e",
                                             gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  composted_waste_col <- if (rlang::is_string(composted_waste_col)) composted_waste_col else rlang::as_name(rlang::enquo(composted_waste_col))
  doc_fraction_col <- if (rlang::is_string(doc_fraction_col)) doc_fraction_col else rlang::as_name(rlang::enquo(doc_fraction_col))
  docf_col <- if (rlang::is_string(docf_col)) docf_col else rlang::as_name(rlang::enquo(docf_col))
  compost_mcf_col <- if (rlang::is_string(compost_mcf_col)) compost_mcf_col else rlang::as_name(rlang::enquo(compost_mcf_col))
  compost_oxidation_col <- if (is.null(compost_oxidation_col) || rlang::is_string(compost_oxidation_col)) compost_oxidation_col else rlang::as_name(rlang::enquo(compost_oxidation_col))
  electricity_consumption_col <- if (is.null(electricity_consumption_col) || rlang::is_string(electricity_consumption_col)) electricity_consumption_col else rlang::as_name(rlang::enquo(electricity_consumption_col))
  electricity_emission_factor_col <- if (is.null(electricity_emission_factor_col) || rlang::is_string(electricity_emission_factor_col)) electricity_emission_factor_col else rlang::as_name(rlang::enquo(electricity_emission_factor_col))
  fossil_fuel_consumption_col <- if (is.null(fossil_fuel_consumption_col) || rlang::is_string(fossil_fuel_consumption_col)) fossil_fuel_consumption_col else rlang::as_name(rlang::enquo(fossil_fuel_consumption_col))
  fossil_fuel_emission_factor_col <- if (is.null(fossil_fuel_emission_factor_col) || rlang::is_string(fossil_fuel_emission_factor_col)) fossil_fuel_emission_factor_col else rlang::as_name(rlang::enquo(fossil_fuel_emission_factor_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .oxidation = if (!is.null(compost_oxidation_col)) .data[[compost_oxidation_col]] else 0,
      .residual_ch4_t = .data[[composted_waste_col]] *
        .data[[doc_fraction_col]] *
        .data[[docf_col]] *
        .data[[compost_mcf_col]] *
        methane_fraction *
        (16 / 12) *
        (1 - .oxidation),
      .residual_component = .residual_ch4_t * gwp_ch4,
      .electricity_component = if (!is_null_dual(electricity_consumption_col, electricity_emission_factor_col)) {
        .data[[electricity_consumption_col]] * .data[[electricity_emission_factor_col]]
      } else {
        0
      },
      .fuel_component = if (!is_null_dual(fossil_fuel_consumption_col, fossil_fuel_emission_factor_col)) {
        .data[[fossil_fuel_consumption_col]] * .data[[fossil_fuel_emission_factor_col]]
      } else {
        0
      },
      .project_component = .residual_component + .electricity_component + .fuel_component
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

#' Calculate leakage emissions associated with compost distribution
#'
#' Equation (3) of AMS-III.F covers leakage from transporting compost and
#' managing residual waste streams that require alternative treatment. The helper
#' also allows crediting avoided emissions if the produced compost displaces
#' synthetic fertiliser. Positive values represent emissions whereas avoided
#' emissions are handled as negatives.
#'
#' @param data Tibble containing leakage monitoring data.
#' @param compost_transported_col Column storing the mass of compost or waste
#'   transported (tonnes).
#' @param transport_distance_col Column storing transport distance (km).
#' @param transport_emission_factor_col Column storing transport emission factors
#'   (tCO2e per tonne-km).
#' @param residual_waste_col Optional column storing residual waste requiring
#'   disposal (tonnes).
#' @param residual_waste_emission_factor_col Optional column storing emission
#'   factors for residual waste treatment (tCO2e per tonne).
#' @param displaced_fertiliser_col Optional column storing the amount of
#'   synthetic fertiliser displaced (tonnes or appropriate units).
#' @param displaced_fertiliser_emission_factor_col Optional column storing the
#'   emission factor for the displaced fertiliser (tCO2e per unit). The term is
#'   subtracted from leakage when provided.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column for leakage emissions in tCO2e.
#' @return Tibble containing grouped leakage emissions.
#' @examples
#' leakage <- tibble(
#'   site_id = c("A", "B"),
#'   compost_transported_tonnes = c(900, 760),
#'   transport_distance_km = c(25, 30),
#'   transport_ef_tco2_per_tkm = 0.0001,
#'   displaced_fertiliser_tonnes = c(50, 45),
#'   fertiliser_ef_tco2_per_tonne = 0.4
#' )
#' calculate_leakage_emissions_iiif(leakage)
#' @export
calculate_leakage_emissions_iiif <- function(data,
                                             compost_transported_col = "compost_transported_tonnes",
                                             transport_distance_col = "transport_distance_km",
                                             transport_emission_factor_col = "transport_ef_tco2_per_tkm",
                                             residual_waste_col = "residual_waste_tonnes",
                                             residual_waste_emission_factor_col = "residual_waste_ef_tco2_per_tonne",
                                             displaced_fertiliser_col = "displaced_fertiliser_tonnes",
                                             displaced_fertiliser_emission_factor_col = "fertiliser_ef_tco2_per_tonne",
                                             group_cols = NULL,
                                             output_col = "leakage_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  compost_transported_col <- if (rlang::is_string(compost_transported_col)) compost_transported_col else rlang::as_name(rlang::enquo(compost_transported_col))
  transport_distance_col <- if (rlang::is_string(transport_distance_col)) transport_distance_col else rlang::as_name(rlang::enquo(transport_distance_col))
  transport_emission_factor_col <- if (rlang::is_string(transport_emission_factor_col)) transport_emission_factor_col else rlang::as_name(rlang::enquo(transport_emission_factor_col))
  residual_waste_col <- if (is.null(residual_waste_col) || rlang::is_string(residual_waste_col)) residual_waste_col else rlang::as_name(rlang::enquo(residual_waste_col))
  residual_waste_emission_factor_col <- if (is_null_dual(residual_waste_col, residual_waste_emission_factor_col)) {
    residual_waste_emission_factor_col
  } else if (rlang::is_string(residual_waste_emission_factor_col)) {
    residual_waste_emission_factor_col
  } else {
    rlang::as_name(rlang::enquo(residual_waste_emission_factor_col))
  }
  displaced_fertiliser_col <- if (is.null(displaced_fertiliser_col) || rlang::is_string(displaced_fertiliser_col)) displaced_fertiliser_col else rlang::as_name(rlang::enquo(displaced_fertiliser_col))
  displaced_fertiliser_emission_factor_col <- if (is_null_dual(displaced_fertiliser_col, displaced_fertiliser_emission_factor_col)) {
    displaced_fertiliser_emission_factor_col
  } else if (rlang::is_string(displaced_fertiliser_emission_factor_col)) {
    displaced_fertiliser_emission_factor_col
  } else {
    rlang::as_name(rlang::enquo(displaced_fertiliser_emission_factor_col))
  }
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .transport_component = .data[[compost_transported_col]] *
        .data[[transport_distance_col]] *
        .data[[transport_emission_factor_col]],
      .residual_component = if (!is.null(residual_waste_col) && !is.null(residual_waste_emission_factor_col)) {
        .data[[residual_waste_col]] * .data[[residual_waste_emission_factor_col]]
      } else {
        0
      },
      .displacement_component = if (!is_null_dual(displaced_fertiliser_col, displaced_fertiliser_emission_factor_col)) {
        .data[[displaced_fertiliser_col]] * .data[[displaced_fertiliser_emission_factor_col]]
      } else {
        0
      },
      .leakage_component = .transport_component + .residual_component - .displacement_component
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

# Helper used internally to check paired optional arguments
is_null_dual <- function(value_col, ef_col) {
  is.null(value_col) || is.null(ef_col)
}

#' Aggregate monitoring periods for AMS-III.F
#'
#' Composting projects often report monitoring results for multiple periods. The
#' helper aggregates period-level outputs produced by the equation helpers and
#' reports totals alongside the number of monitoring periods available for each
#' grouping combination.
#'
#' @param data Tibble containing period-level emissions output.
#' @param group_cols Character vector identifying grouping columns (e.g.
#'   `"site_id"`).
#' @param monitoring_col Column storing the monitoring period identifier.
#' @return Tibble summarising emissions by group with a monitoring period count.
#' @examples
#' period_results <- tibble::tibble(
#'   site_id = rep("A", 2),
#'   period = 1:2,
#'   baseline_emissions_tco2e = c(20, 19),
#'   project_emissions_tco2e = c(5, 4.5),
#'   leakage_emissions_tco2e = c(0.5, 0.4)
#' )
#' aggregate_monitoring_periods_iiif(period_results, group_cols = "site_id", monitoring_col = "period")
#' @export
aggregate_monitoring_periods_iiif <- function(data,
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

#' Calculate emission reductions for AMS-III.F
#'
#' Combines baseline, project, and leakage emissions to estimate net emission
#' reductions. All inputs must include the columns output by the corresponding
#' helpers. Grouping columns are preserved when provided.
#'
#' @param baseline_emissions Tibble produced by
#'   `calculate_baseline_methane_emissions_iiif`.
#' @param project_emissions Tibble produced by
#'   `calculate_project_emissions_iiif`.
#' @param leakage_emissions Optional tibble produced by
#'   `calculate_leakage_emissions_iiif`.
#' @param group_cols Optional character vector identifying grouping columns.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' baseline <- tibble::tibble(baseline_emissions_tco2e = 25)
#' project <- tibble::tibble(project_emissions_tco2e = 6)
#' calculate_emission_reductions_iiif(baseline, project)
#' @export
calculate_emission_reductions_iiif <- function(baseline_emissions,
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
