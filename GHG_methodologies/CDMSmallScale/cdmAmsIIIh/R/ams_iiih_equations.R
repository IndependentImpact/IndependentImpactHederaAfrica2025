#' Calculate baseline methane emissions for AMS-III.H projects
#'
#' Baseline emissions from wastewater treatment under AMS-III.H are driven by
#' the methane that would be emitted from uncovered anaerobic lagoons or other
#' systems without recovery. This helper estimates methane formation from COD
#' loads and applies optional baseline capture or oxidation fractions before
#' converting to tCO2e.
#'
#' @param data Tibble containing baseline wastewater monitoring data.
#' @param influent_flow_col Column storing influent wastewater flow in m3 per day.
#' @param cod_concentration_col Column storing influent COD in mg/L.
#' @param degradable_cod_fraction_col Optional column storing the fraction of COD
#'   that is biodegradable.
#' @param baseline_capture_efficiency_col Optional column storing methane capture
#'   efficiency in the baseline scenario.
#' @param oxidation_fraction_col Optional column storing the oxidation fraction
#'   of residual methane.
#' @param days_col Optional column storing the number of days represented by each
#'   record. When supplied, COD loads are scaled by the number of days.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column for baseline emissions in tCO2e.
#' @param methane_conversion_factor_m3_per_kg_cod Methane generation potential in
#'   m3 CH4 per kg COD. Defaults to 0.35.
#' @param methane_density_t_per_m3 Density of methane in tonnes per cubic metre.
#'   Defaults to 0.00067 t CH4 per m3.
#' @param gwp_ch4 Global warming potential of methane. Defaults to 28.
#' @return Tibble containing grouped baseline methane emissions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(
#'   site_id = c("WW1", "WW2"),
#'   influent_flow_m3_per_day = c(5500, 3200),
#'   cod_mg_l = c(3200, 1800),
#'   biodegradable_fraction = c(0.8, 0.65)
#' )
#' calculate_baseline_methane_emissions_iiih(baseline, group_cols = "site_id")
#' @export
calculate_baseline_methane_emissions_iiih <- function(data,
                                                      influent_flow_col = "influent_flow_m3_per_day",
                                                      cod_concentration_col = "cod_mg_l",
                                                      degradable_cod_fraction_col = "biodegradable_fraction",
                                                      baseline_capture_efficiency_col = "baseline_capture_efficiency_fraction",
                                                      oxidation_fraction_col = "baseline_oxidation_fraction",
                                                      days_col = "days_in_period",
                                                      group_cols = NULL,
                                                      output_col = "baseline_emissions_tco2e",
                                                      methane_conversion_factor_m3_per_kg_cod = 0.35,
                                                      methane_density_t_per_m3 = 0.00067,
                                                      gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  influent_flow_col <- resolve_column(influent_flow_col)
  cod_concentration_col <- resolve_column(cod_concentration_col)
  degradable_cod_fraction_col <- resolve_optional_column(degradable_cod_fraction_col)
  baseline_capture_efficiency_col <- resolve_optional_column(baseline_capture_efficiency_col)
  oxidation_fraction_col <- resolve_optional_column(oxidation_fraction_col)
  days_col <- resolve_optional_column(days_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .days = if (!is.null(days_col)) .data[[days_col]] else 1,
      .cod_load_kg = .data[[influent_flow_col]] * .data[[cod_concentration_col]] / 1000,
      .cod_load_kg = if (!is.null(degradable_cod_fraction_col)) {
        .cod_load_kg * .data[[degradable_cod_fraction_col]]
      } else {
        .cod_load_kg
      },
      .cod_load_kg = .cod_load_kg * .days,
      .baseline_capture = if (!is.null(baseline_capture_efficiency_col)) {
        .data[[baseline_capture_efficiency_col]]
      } else {
        0
      },
      .oxidation = if (!is.null(oxidation_fraction_col)) {
        .data[[oxidation_fraction_col]]
      } else {
        0
      },
      .generated_m3 = .cod_load_kg * methane_conversion_factor_m3_per_kg_cod,
      .captured_m3 = .generated_m3 * .baseline_capture,
      .oxidised_m3 = (.generated_m3 - .captured_m3) * .oxidation,
      .emitted_m3 = pmax(.generated_m3 - .captured_m3 - .oxidised_m3, 0),
      .baseline_component = .emitted_m3 * methane_density_t_per_m3 * gwp_ch4
    )

  summarise_emission_component(emissions, groups, output_col, ".baseline_component")
}

#' Calculate project emissions for AMS-III.H projects
#'
#' Project emissions account for residual methane that escapes the recovery
#' system as well as onsite energy used to operate digesters, blowers, or
#' utilisation equipment. The helper converts methane balances and optional
#' energy inputs into tCO2e.
#'
#' @inheritParams calculate_baseline_methane_emissions_iiih
#' @param project_capture_efficiency_col Column storing methane capture efficiency
#'   under the project scenario.
#' @param destruction_efficiency_col Column storing the destruction efficiency of
#'   the flare or utilisation equipment.
#' @param project_oxidation_fraction_col Optional column storing oxidation
#'   fractions for uncaptured methane.
#' @param fugitive_leakage_fraction_col Optional column storing additional
#'   leakage fraction from handling losses.
#' @param electricity_consumption_col Optional column storing electricity use in
#'   MWh for running pumps or blowers.
#' @param electricity_emission_factor_col Optional column storing emission factor
#'   in tCO2 per MWh.
#' @param thermal_energy_consumption_col Optional column storing supplemental fuel
#'   energy use in GJ.
#' @param thermal_energy_emission_factor_col Optional column storing emission
#'   factor in tCO2 per GJ.
#' @param output_col Name of the output column for project emissions in tCO2e.
#' @return Tibble containing grouped project emissions in tCO2e.
#' @examples
#' project <- tibble::tibble(
#'   site_id = c("WW1", "WW2"),
#'   influent_flow_m3_per_day = c(5500, 3200),
#'   cod_mg_l = c(3200, 1800),
#'   project_capture_efficiency_fraction = c(0.75, 0.65),
#'   destruction_efficiency_fraction = c(0.985, 0.97)
#' )
#' calculate_project_emissions_iiih(project, group_cols = "site_id")
#' @export
calculate_project_emissions_iiih <- function(data,
                                             influent_flow_col = "influent_flow_m3_per_day",
                                             cod_concentration_col = "cod_mg_l",
                                             degradable_cod_fraction_col = "biodegradable_fraction",
                                             project_capture_efficiency_col = "project_capture_efficiency_fraction",
                                             destruction_efficiency_col = "destruction_efficiency_fraction",
                                             project_oxidation_fraction_col = "project_oxidation_fraction",
                                             fugitive_leakage_fraction_col = "fugitive_leakage_fraction",
                                             days_col = "days_in_period",
                                             electricity_consumption_col = "electricity_consumption_mwh",
                                             electricity_emission_factor_col = "electricity_ef_tco2_per_mwh",
                                             thermal_energy_consumption_col = "thermal_energy_consumption_gj",
                                             thermal_energy_emission_factor_col = "thermal_energy_ef_tco2_per_gj",
                                             group_cols = NULL,
                                             output_col = "project_emissions_tco2e",
                                             methane_conversion_factor_m3_per_kg_cod = 0.35,
                                             methane_density_t_per_m3 = 0.00067,
                                             gwp_ch4 = 28) {
  data_tbl <- tibble::as_tibble(data)
  influent_flow_col <- resolve_column(influent_flow_col)
  cod_concentration_col <- resolve_column(cod_concentration_col)
  degradable_cod_fraction_col <- resolve_optional_column(degradable_cod_fraction_col)
  project_capture_efficiency_col <- resolve_column(project_capture_efficiency_col)
  destruction_efficiency_col <- resolve_column(destruction_efficiency_col)
  project_oxidation_fraction_col <- resolve_optional_column(project_oxidation_fraction_col)
  fugitive_leakage_fraction_col <- resolve_optional_column(fugitive_leakage_fraction_col)
  days_col <- resolve_optional_column(days_col)
  electricity_consumption_col <- resolve_optional_column(electricity_consumption_col)
  electricity_emission_factor_col <- resolve_optional_column(electricity_emission_factor_col)
  thermal_energy_consumption_col <- resolve_optional_column(thermal_energy_consumption_col)
  thermal_energy_emission_factor_col <- resolve_optional_column(thermal_energy_emission_factor_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .days = if (!is.null(days_col)) .data[[days_col]] else 1,
      .cod_load_kg = .data[[influent_flow_col]] * .data[[cod_concentration_col]] / 1000,
      .cod_load_kg = if (!is.null(degradable_cod_fraction_col)) {
        .cod_load_kg * .data[[degradable_cod_fraction_col]]
      } else {
        .cod_load_kg
      },
      .cod_load_kg = .cod_load_kg * .days,
      .generated_m3 = .cod_load_kg * methane_conversion_factor_m3_per_kg_cod,
      .captured_m3 = .generated_m3 * .data[[project_capture_efficiency_col]],
      .destroyed_m3 = .captured_m3 * .data[[destruction_efficiency_col]],
      .oxidised_m3 = if (!is.null(project_oxidation_fraction_col)) {
        (.generated_m3 - .captured_m3) * .data[[project_oxidation_fraction_col]]
      } else {
        0
      },
      .fugitive_m3 = if (!is.null(fugitive_leakage_fraction_col)) {
        .captured_m3 * .data[[fugitive_leakage_fraction_col]]
      } else {
        0
      },
      .residual_m3 = pmax(.generated_m3 - .destroyed_m3 - .oxidised_m3 + .fugitive_m3, 0),
      .methane_component = .residual_m3 * methane_density_t_per_m3 * gwp_ch4,
      .electricity_component = if (!is.null(electricity_consumption_col) && !is.null(electricity_emission_factor_col)) {
        .data[[electricity_consumption_col]] * .data[[electricity_emission_factor_col]]
      } else {
        0
      },
      .thermal_component = if (!is.null(thermal_energy_consumption_col) && !is.null(thermal_energy_emission_factor_col)) {
        .data[[thermal_energy_consumption_col]] * .data[[thermal_energy_emission_factor_col]]
      } else {
        0
      },
      .project_component = .methane_component + .electricity_component + .thermal_component
    )

  summarise_emission_component(emissions, groups, output_col, ".project_component")
}

#' Calculate leakage emissions for AMS-III.H projects
#'
#' Leakage emissions typically arise from transporting and treating residual
#' sludge, importing auxiliary chemicals, or exporting excess biogas for
#' off-site use. This helper aggregates transport, treatment, and optional
#' displacement credits into a net leakage estimate.
#'
#' @param data Tibble containing leakage monitoring data.
#' @param sludge_mass_col Column storing sludge transported off-site in tonnes.
#' @param transport_distance_col Column storing the transport distance in
#'   kilometres.
#' @param transport_emission_factor_col Column storing emission factors in tCO2
#'   per tonne-kilometre.
#' @param sludge_treatment_emission_factor_col Optional column storing emission
#'   factors for sludge treatment in tCO2 per tonne.
#' @param chemical_dosage_col Optional column storing chemical usage in tonnes.
#' @param chemical_emission_factor_col Optional column storing emission factors in
#'   tCO2 per tonne of chemical.
#' @param displaced_fossil_fuel_col Optional column storing fossil fuel displaced
#'   by biogas utilisation in GJ.
#' @param displaced_fossil_fuel_emission_factor_col Optional column storing the
#'   emission factor for the displaced fuel in tCO2 per GJ (credited as negative
#'   leakage).
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the output column for leakage emissions in tCO2e.
#' @return Tibble containing grouped leakage emissions in tCO2e.
#' @examples
#' leakage <- tibble::tibble(
#'   site_id = c("WW1", "WW2"),
#'   sludge_tonnes = c(450, 320),
#'   transport_distance_km = c(25, 40),
#'   transport_ef_tco2_per_tkm = c(0.0001, 0.00012)
#' )
#' calculate_leakage_emissions_iiih(leakage, group_cols = "site_id")
#' @export
calculate_leakage_emissions_iiih <- function(data,
                                             sludge_mass_col = "sludge_tonnes",
                                             transport_distance_col = "transport_distance_km",
                                             transport_emission_factor_col = "transport_ef_tco2_per_tkm",
                                             sludge_treatment_emission_factor_col = "sludge_treatment_ef_tco2_per_tonne",
                                             chemical_dosage_col = "chemical_usage_tonnes",
                                             chemical_emission_factor_col = "chemical_ef_tco2_per_tonne",
                                             displaced_fossil_fuel_col = "displaced_fossil_fuel_gj",
                                             displaced_fossil_fuel_emission_factor_col = "displaced_fossil_fuel_ef_tco2_per_gj",
                                             group_cols = NULL,
                                             output_col = "leakage_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  sludge_mass_col <- resolve_column(sludge_mass_col)
  transport_distance_col <- resolve_column(transport_distance_col)
  transport_emission_factor_col <- resolve_column(transport_emission_factor_col)
  sludge_treatment_emission_factor_col <- resolve_optional_column(sludge_treatment_emission_factor_col)
  chemical_dosage_col <- resolve_optional_column(chemical_dosage_col)
  chemical_emission_factor_col <- resolve_optional_column(chemical_emission_factor_col)
  displaced_fossil_fuel_col <- resolve_optional_column(displaced_fossil_fuel_col)
  displaced_fossil_fuel_emission_factor_col <- resolve_optional_column(displaced_fossil_fuel_emission_factor_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .transport_component = .data[[sludge_mass_col]] *
        .data[[transport_distance_col]] *
        .data[[transport_emission_factor_col]],
      .treatment_component = if (!is.null(sludge_treatment_emission_factor_col)) {
        .data[[sludge_mass_col]] * .data[[sludge_treatment_emission_factor_col]]
      } else {
        0
      },
      .chemical_component = if (!is.null(chemical_dosage_col) && !is.null(chemical_emission_factor_col)) {
        .data[[chemical_dosage_col]] * .data[[chemical_emission_factor_col]]
      } else {
        0
      },
      .displacement_component = if (!is.null(displaced_fossil_fuel_col) &&
        !is.null(displaced_fossil_fuel_emission_factor_col)) {
        .data[[displaced_fossil_fuel_col]] * .data[[displaced_fossil_fuel_emission_factor_col]]
      } else {
        0
      },
      .leakage_component = .transport_component +
        .treatment_component +
        .chemical_component -
        .displacement_component
    )

  summarise_emission_component(emissions, groups, output_col, ".leakage_component")
}

#' Aggregate monitoring periods for AMS-III.H
#'
#' Aggregates period-level emissions outputs produced by the equation helpers and
#' counts distinct monitoring periods per grouping combination.
#'
#' @param data Tibble containing period-level emissions.
#' @param group_cols Character vector identifying grouping columns.
#' @param monitoring_col Column storing the monitoring period identifier.
#' @return Tibble summarising emissions by group with monitoring period counts.
#' @examples
#' period_results <- tibble::tibble(
#'   site_id = rep("WW1", 2),
#'   period = 1:2,
#'   baseline_emissions_tco2e = c(1500, 1480),
#'   project_emissions_tco2e = c(420, 410),
#'   leakage_emissions_tco2e = c(30, 28)
#' )
#' aggregate_monitoring_periods_iiih(period_results, group_cols = "site_id", monitoring_col = "period")
#' @export
aggregate_monitoring_periods_iiih <- function(data,
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

#' Calculate emission reductions for AMS-III.H
#'
#' Combines baseline, project, and optional leakage emissions to estimate net
#' emission reductions. Grouping columns are preserved when provided.
#'
#' @param baseline_emissions Tibble produced by
#'   `calculate_baseline_methane_emissions_iiih`.
#' @param project_emissions Tibble produced by
#'   `calculate_project_emissions_iiih`.
#' @param leakage_emissions Optional tibble produced by
#'   `calculate_leakage_emissions_iiih`.
#' @param group_cols Optional character vector identifying grouping columns.
#' @return Tibble containing baseline, project, leakage, and net emission
#'   reductions.
#' @examples
#' baseline <- tibble::tibble(baseline_emissions_tco2e = 1800)
#' project <- tibble::tibble(project_emissions_tco2e = 420)
#' calculate_emission_reductions_iiih(baseline, project)
#' @export
calculate_emission_reductions_iiih <- function(baseline_emissions,
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
