#' Calculate baseline non-renewable biomass (Equation 1)
#'
#' Implements Equation (1) of AMS-II.G by applying the fraction of non-renewable
#' biomass to monitored baseline biomass consumption. Results are aggregated
#' across the requested grouping structure to produce total displaced
#' non-renewable biomass.
#'
#' @param baseline_data Tibble containing baseline biomass monitoring data.
#' @param consumption_col Column storing baseline biomass consumption (e.g.
#'   tonnes).
#' @param fraction_col Column storing the fraction of biomass that is
#'   non-renewable (dimensionless).
#' @param group_cols Optional character vector of columns used for aggregation
#'   (e.g. household or community identifiers).
#' @param output_col Name of the output column storing non-renewable biomass in
#'   the same units as `consumption_col`.
#' @return A tibble grouped by `group_cols` (when supplied) with one column named
#'   `output_col` containing non-renewable biomass.
#' @examples
#' baseline <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   baseline_biomass_consumption_tonnes = c(12, 14),
#'   baseline_non_renewable_fraction = c(0.82, 0.88)
#' )
#' calculate_baseline_non_renewable_biomass_iig(
#'   baseline,
#'   group_cols = "site_id"
#' )
#' @export
calculate_baseline_non_renewable_biomass_iig <- function(baseline_data,
                                                        consumption_col = "baseline_biomass_consumption_tonnes",
                                                        fraction_col = "baseline_non_renewable_fraction",
                                                        group_cols = NULL,
                                                        output_col = "baseline_non_renewable_biomass_tonnes") {
  data_tbl <- dplyr::as_tibble(baseline_data)

  if (!consumption_col %in% names(data_tbl)) {
    stop("`consumption_col` must exist in `baseline_data`.", call. = FALSE)
  }
  if (!fraction_col %in% names(data_tbl)) {
    stop("`fraction_col` must exist in `baseline_data`.", call. = FALSE)
  }

  consumption_sym <- rlang::sym(consumption_col)
  fraction_sym <- rlang::sym(fraction_col)
  output_sym <- rlang::sym(output_col)

  summarise_tbl <- function(tbl) {
    tbl |>
      dplyr::summarise(
        !!output_sym := sum(
          dplyr::coalesce(!!consumption_sym, 0) * dplyr::coalesce(!!fraction_sym, 0),
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_tbl(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_tbl()
  }
}

#' Calculate project non-renewable biomass (Equation 1A)
#'
#' Applies the fraction of non-renewable biomass observed under the project to
#' the monitored project biomass consumption. The helper mirrors
#' [calculate_baseline_non_renewable_biomass_iig()] but uses project-specific
#' column defaults.
#'
#' @inheritParams calculate_baseline_non_renewable_biomass_iig
#' @param project_data Tibble containing project biomass monitoring data.
#' @param project_consumption_col Column storing project biomass consumption.
#' @param project_fraction_col Column storing the fraction of project biomass
#'   that is non-renewable.
#' @param output_col Name of the column storing project non-renewable biomass.
#' @examples
#' project <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   project_biomass_consumption_tonnes = c(7.5, 8.1),
#'   project_non_renewable_fraction = c(0.4, 0.38)
#' )
#' calculate_project_non_renewable_biomass_iig(project, group_cols = "site_id")
#' @export
calculate_project_non_renewable_biomass_iig <- function(project_data,
                                                        project_consumption_col = "project_biomass_consumption_tonnes",
                                                        project_fraction_col = "project_non_renewable_fraction",
                                                        group_cols = NULL,
                                                        output_col = "project_non_renewable_biomass_tonnes") {
  calculate_baseline_non_renewable_biomass_iig(
    baseline_data = project_data,
    consumption_col = project_consumption_col,
    fraction_col = project_fraction_col,
    group_cols = group_cols,
    output_col = output_col
  )
}

#' Calculate baseline thermal energy (Equation 2)
#'
#' Converts the baseline non-renewable biomass quantities from Equation (1) into
#' useful thermal energy using the net calorific value (NCV) of the baseline
#' biomass feedstock. The NCV is summarised from the supplied baseline dataset
#' and assumed to be constant within each grouping structure.
#'
#' @param non_renewable_biomass Tibble returned by
#'   [calculate_baseline_non_renewable_biomass_iig()].
#' @param biomass_col Column storing non-renewable biomass values.
#' @param baseline_data Tibble containing baseline NCV observations.
#' @param ncv_col Column containing the baseline NCV in MJ per unit of biomass.
#' @param group_cols Optional grouping columns to preserve in the output.
#' @param output_col Name of the resulting column storing baseline thermal energy
#'   in MJ.
#' @return A tibble with baseline thermal energy in MJ.
#' @examples
#' non_renewable <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   baseline_non_renewable_biomass_tonnes = c(9.84, 12.32)
#' )
#' baseline <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   baseline_net_calorific_value_mj_per_tonne = c(15.2, 14.8)
#' )
#' calculate_baseline_thermal_energy_iig(
#'   non_renewable,
#'   baseline_data = baseline,
#'   group_cols = "site_id"
#' )
#' @export
calculate_baseline_thermal_energy_iig <- function(non_renewable_biomass,
                                                  biomass_col = "baseline_non_renewable_biomass_tonnes",
                                                  baseline_data,
                                                  ncv_col = "baseline_net_calorific_value_mj_per_tonne",
                                                  group_cols = NULL,
                                                  output_col = "baseline_thermal_energy_mj") {
  calculate_thermal_energy_iig(
    non_renewable_biomass = non_renewable_biomass,
    biomass_col = biomass_col,
    energy_data = baseline_data,
    ncv_col = ncv_col,
    group_cols = group_cols,
    output_col = output_col
  )
}

#' Calculate project thermal energy (Equation 3)
#'
#' Converts the project non-renewable biomass quantities into thermal energy
#' using the project net calorific value. Mirrors
#' [calculate_baseline_thermal_energy_iig()] with project-specific defaults.
#'
#' @inheritParams calculate_baseline_thermal_energy_iig
#' @param project_data Tibble containing project NCV observations.
#' @param ncv_col Column containing the project NCV values in MJ per unit.
#' @examples
#' project_nrb <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   project_non_renewable_biomass_tonnes = c(3.2, 3.1)
#' )
#' project_inputs <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   project_net_calorific_value_mj_per_tonne = c(15.4, 15.1)
#' )
#' calculate_project_thermal_energy_iig(
#'   project_nrb,
#'   project_data = project_inputs,
#'   group_cols = "site_id"
#' )
#' @export
calculate_project_thermal_energy_iig <- function(non_renewable_biomass,
                                                 biomass_col = "project_non_renewable_biomass_tonnes",
                                                 project_data,
                                                 ncv_col = "project_net_calorific_value_mj_per_tonne",
                                                 group_cols = NULL,
                                                 output_col = "project_thermal_energy_mj") {
  calculate_thermal_energy_iig(
    non_renewable_biomass = non_renewable_biomass,
    biomass_col = biomass_col,
    energy_data = project_data,
    ncv_col = ncv_col,
    group_cols = group_cols,
    output_col = output_col
  )
}

#' Internal helper to convert non-renewable biomass into thermal energy
#'
#' @keywords internal
calculate_thermal_energy_iig <- function(non_renewable_biomass,
                                         biomass_col,
                                         energy_data,
                                         ncv_col,
                                         group_cols,
                                         output_col) {
  biomass_tbl <- dplyr::as_tibble(non_renewable_biomass)
  ncv_tbl <- dplyr::as_tibble(energy_data)

  if (!ncv_col %in% names(ncv_tbl)) {
    stop("`ncv_col` must exist in the supplied energy dataset.", call. = FALSE)
  }
  if (!biomass_col %in% names(biomass_tbl)) {
    stop("`biomass_col` must exist in `non_renewable_biomass`.", call. = FALSE)
  }

  biomass_sym <- rlang::sym(biomass_col)
  output_sym <- rlang::sym(output_col)
  single_value <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(0)
    }
    if (dplyr::n_distinct(values) > 1) {
      stop("`ncv_col` must have a single value within each grouping structure.", call. = FALSE)
    }
    values[[1]]
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    ncv_value <- single_value(ncv_tbl[[ncv_col]])
    biomass_tbl |>
      dplyr::mutate(!!output_sym := dplyr::coalesce(!!biomass_sym, 0) * ncv_value) |>
      dplyr::select(!!output_sym)
  } else {
    ncv_summary <- ncv_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        .ncv = single_value(dplyr::cur_data_all()[[ncv_col]]),
        .groups = "drop"
      )

    biomass_tbl |>
      dplyr::left_join(ncv_summary, by = group_cols) |>
      dplyr::mutate(
        !!output_sym := dplyr::coalesce(!!biomass_sym, 0) * dplyr::coalesce(.ncv, 0)
      ) |>
      dplyr::select(dplyr::all_of(group_cols), !!output_sym)
  }
}

#' Calculate emissions from thermal energy (Equations 4 & 5)
#'
#' Multiplies thermal energy demand by the appropriate emission factor to obtain
#' baseline or project emissions under AMS-II.G. Emission factors are assumed to
#' be constant within the requested grouping structure.
#'
#' @param energy_data Tibble containing thermal energy results (baseline or
#'   project).
#' @param energy_col Column storing thermal energy in MJ.
#' @param factor_data Tibble containing emission factors.
#' @param emission_factor_col Column storing emission factors in tCO2e/MJ.
#' @param group_cols Optional grouping columns.
#' @param output_col Name of the resulting emissions column.
#' @return Tibble with emissions aggregated by the grouping structure.
#' @examples
#' energy <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   baseline_thermal_energy_mj = c(1500, 1820)
#' )
#' factors <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   baseline_emission_factor_tco2_per_mj = c(0.00009, 0.000095)
#' )
#' calculate_emissions_from_energy_iig(
#'   energy,
#'   factor_data = factors,
#'   group_cols = "site_id"
#' )
#' @export
calculate_emissions_from_energy_iig <- function(energy_data,
                                                energy_col = "baseline_thermal_energy_mj",
                                                factor_data,
                                                emission_factor_col = "baseline_emission_factor_tco2_per_mj",
                                                group_cols = NULL,
                                                output_col = "baseline_emissions_tco2e") {
  energy_tbl <- dplyr::as_tibble(energy_data)
  factor_tbl <- dplyr::as_tibble(factor_data)

  if (!energy_col %in% names(energy_tbl)) {
    stop("`energy_col` must exist in `energy_data`.", call. = FALSE)
  }
  if (!emission_factor_col %in% names(factor_tbl)) {
    stop("`emission_factor_col` must exist in `factor_data`.", call. = FALSE)
  }

  energy_sym <- rlang::sym(energy_col)
  factor_sym <- rlang::sym(emission_factor_col)
  output_sym <- rlang::sym(output_col)

  summarise_factor <- function(tbl) {
    values <- tbl[[emission_factor_col]]
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(0)
    }
    if (dplyr::n_distinct(values) > 1) {
      stop("`emission_factor_col` must have a single value within each grouping structure.", call. = FALSE)
    }
    values[[1]]
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    factor_value <- summarise_factor(factor_tbl)
    energy_tbl |>
      dplyr::summarise(
        !!output_sym := sum(dplyr::coalesce(!!energy_sym, 0) * factor_value, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    factor_summary <- factor_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        .factor = summarise_factor(dplyr::cur_data_all()),
        .groups = "drop"
      )

    energy_tbl |>
      dplyr::left_join(factor_summary, by = group_cols) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        !!output_sym := sum(dplyr::coalesce(!!energy_sym, 0) * dplyr::coalesce(.factor, 0), na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate leakage emissions for AMS-II.G (Equation 6)
#'
#' Aggregates leakage emissions associated with biomass production or transport.
#' When leakage data are absent the helper returns zeros matching the grouping
#' structure so that downstream calculations remain robust.
#'
#' @param leakage_data Optional tibble containing leakage estimates.
#' @param leakage_col Column storing leakage emissions in tCO2e.
#' @param group_cols Optional grouping columns.
#' @param output_col Name of the leakage column in the output.
#' @return Tibble with leakage totals by group (or a single zero row when no
#'   leakage data are supplied).
#' @examples
#' leakage <- tibble::tibble(
#'   site_id = c("cookstove-1", "cookstove-2"),
#'   leakage_emissions_tco2e = c(0.12, 0.08)
#' )
#' calculate_leakage_emissions_iig(leakage, group_cols = "site_id")
#' @export
calculate_leakage_emissions_iig <- function(leakage_data = NULL,
                                            leakage_col = "leakage_emissions_tco2e",
                                            group_cols = NULL,
                                            output_col = "leakage_emissions_tco2e") {
  if (is.null(leakage_data)) {
    if (is.null(group_cols) || length(group_cols) == 0) {
      return(tibble::tibble(!!rlang::sym(output_col) := 0))
    }
    empty_groups <- tibble::tibble()
    for (col in group_cols) {
      empty_groups[[col]] <- character()
    }
    return(dplyr::as_tibble(empty_groups))
  }

  leakage_tbl <- dplyr::as_tibble(leakage_data)

  if (!leakage_col %in% names(leakage_tbl)) {
    stop("`leakage_col` must exist in `leakage_data` when provided.", call. = FALSE)
  }

  leakage_sym <- rlang::sym(leakage_col)
  output_sym <- rlang::sym(output_col)

  summarise_tbl <- function(tbl) {
    tbl |>
      dplyr::summarise(!!output_sym := sum(dplyr::coalesce(!!leakage_sym, 0), na.rm = TRUE), .groups = "drop")
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    summarise_tbl(leakage_tbl)
  } else {
    leakage_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      summarise_tbl()
  }
}

#' Estimate emission reductions for AMS-II.G (Equation 7)
#'
#' Combines baseline, project, and leakage emissions to obtain net emission
#' reductions in accordance with Equation (7) of AMS-II.G. When leakage
#' information is absent it is treated as zero.
#'
#' @param baseline_emissions Tibble containing baseline emission totals.
#' @param project_emissions Tibble containing project emission totals.
#' @param leakage_emissions Optional tibble containing leakage totals.
#' @param group_cols Optional grouping columns.
#' @param output_col Name of the emission reduction column in the output.
#' @return Tibble with emission reductions by group.
#' @examples
#' baseline <- tibble::tibble(site_id = "cookstove-1", baseline_emissions_tco2e = 12.5)
#' project <- tibble::tibble(site_id = "cookstove-1", project_emissions_tco2e = 4.2)
#' calculate_emission_reductions_iig(baseline, project, group_cols = "site_id")
#' @export
calculate_emission_reductions_iig <- function(baseline_emissions,
                                              project_emissions,
                                              leakage_emissions = NULL,
                                              group_cols = NULL,
                                              output_col = "emission_reductions_tco2e") {
  baseline_tbl <- dplyr::as_tibble(baseline_emissions)
  project_tbl <- dplyr::as_tibble(project_emissions)

  if (!"baseline_emissions_tco2e" %in% names(baseline_tbl)) {
    stop("`baseline_emissions` must include `baseline_emissions_tco2e`.", call. = FALSE)
  }
  if (!"project_emissions_tco2e" %in% names(project_tbl)) {
    stop("`project_emissions` must include `project_emissions_tco2e`.", call. = FALSE)
  }

  reduction_sym <- rlang::sym(output_col)

  join_by_cols <- group_cols
  if (is.null(join_by_cols)) {
    join_by_cols <- character()
  }

  combined <- baseline_tbl |>
    dplyr::left_join(project_tbl, by = join_by_cols)

  if (!is.null(leakage_emissions)) {
    leakage_tbl <- dplyr::as_tibble(leakage_emissions)
    if (!"leakage_emissions_tco2e" %in% names(leakage_tbl)) {
      stop("`leakage_emissions` must include `leakage_emissions_tco2e`.", call. = FALSE)
    }
    combined <- combined |>
      dplyr::left_join(leakage_tbl, by = join_by_cols)
  }

  if (!"leakage_emissions_tco2e" %in% names(combined)) {
    combined$leakage_emissions_tco2e <- 0
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    combined |>
      dplyr::summarise(
        !!reduction_sym := sum(
          dplyr::coalesce(baseline_emissions_tco2e, 0) -
            dplyr::coalesce(project_emissions_tco2e, 0) -
            dplyr::coalesce(leakage_emissions_tco2e, 0),
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  } else {
    combined |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        !!reduction_sym := sum(
          dplyr::coalesce(baseline_emissions_tco2e, 0) -
            dplyr::coalesce(project_emissions_tco2e, 0) -
            dplyr::coalesce(leakage_emissions_tco2e, 0),
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }
}
