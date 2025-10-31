#' Calculate non-renewable biomass consumption (Equation 1)
#'
#' Implements Equation (1) of AMS-I.E by applying the fraction of non-renewable
#' biomass to the observed biomass consumption and aggregating across the
#' specified grouping variables.
#'
#' @param biomass_data A tibble containing biomass consumption observations.
#' @param consumption_col Column name storing biomass consumption (e.g. tonnes).
#' @param fraction Fraction of the biomass that is non-renewable. Supply either a
#'   single numeric value or a vector matching the number of rows in
#'   `biomass_data` when `fraction_col` is `NULL`.
#' @param fraction_col Optional column containing the non-renewable biomass
#'   fraction for each observation. When provided it overrides `fraction`.
#' @param group_cols Optional character vector of grouping columns used to
#'   aggregate the non-renewable biomass consumption.
#' @param output_col Name of the resulting column containing non-renewable
#'   biomass in the same units as `consumption_col`.
#' @return A tibble with grouping columns (if supplied) and a column named
#'   `output_col` storing non-renewable biomass consumption.
#' @examples
#' data <- tibble::tibble(
#'   user_id = c("cookstove-1", "cookstove-2"),
#'   biomass_tonnes = c(15, 18),
#'   non_renewable_fraction = c(0.85, 0.9)
#' )
#' calculate_non_renewable_biomass(
#'   data,
#'   consumption_col = "biomass_tonnes",
#'   fraction_col = "non_renewable_fraction"
#' )
#' @export
calculate_non_renewable_biomass <- function(biomass_data,
                                            consumption_col = "biomass_consumption_tonnes",
                                            fraction = 1,
                                            fraction_col = "non_renewable_fraction",
                                            group_cols = NULL,
                                            output_col = "non_renewable_biomass_tonnes") {
  biomass_tbl <- tibble::as_tibble(biomass_data)

  consumption_sym <- if (is.character(consumption_col)) {
    if (length(consumption_col) != 1) {
      stop("`consumption_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(consumption_col)
  } else {
    rlang::ensym(consumption_col)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  consumption_col_name <- rlang::as_string(consumption_sym)
  if (!consumption_col_name %in% names(biomass_tbl)) {
    stop("`consumption_col` must exist in `biomass_data`.", call. = FALSE)
  }

  effective_fraction <- if (!is.null(fraction_col) && fraction_col %in% names(biomass_tbl)) {
    frac_values <- biomass_tbl[[fraction_col]]
    if (!is.numeric(frac_values)) {
      stop("`fraction_col` must contain numeric values.", call. = FALSE)
    }
    frac_values
  } else {
    if (length(fraction) != 1 && length(fraction) != nrow(biomass_tbl)) {
      stop(
        "`fraction` must be length 1 or match the number of rows in `biomass_data` when `fraction_col` is NULL.",
        call. = FALSE
      )
    }
    if (!is.numeric(fraction)) {
      stop("`fraction` must be numeric when `fraction_col` is NULL.", call. = FALSE)
    }
    if (length(fraction) == 1) rep(fraction, nrow(biomass_tbl)) else fraction
  }

  temp <- biomass_tbl |>
    dplyr::mutate(`__non_renewable` = !!consumption_sym * effective_fraction)

  if (is.null(group_cols) || length(group_cols) == 0) {
    temp |>
      dplyr::summarise(!!output_sym := sum(`__non_renewable`, na.rm = TRUE), .groups = "drop")
  } else {
    temp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(!!output_sym := sum(`__non_renewable`, na.rm = TRUE), .groups = "drop")
  }
}

#' Calculate baseline thermal energy content (Equation 2)
#'
#' Converts the non-renewable biomass consumption calculated under Equation (1)
#' into useful thermal energy by applying the net calorific value (NCV).
#'
#' @param non_renewable_biomass Tibble produced by
#'   [calculate_non_renewable_biomass()].
#' @param biomass_col Column containing non-renewable biomass quantity.
#' @param ncv Net calorific value in MJ per unit of biomass. Supply either a
#'   single value applied to all rows or a vector matching the number of rows in
#'   `non_renewable_biomass`.
#' @param output_col Name of the resulting column containing baseline energy in
#'   MJ.
#' @return Tibble with baseline thermal energy in MJ.
#' @examples
#' biomass <- tibble::tibble(non_renewable_biomass_tonnes = c(10, 12))
#' calculate_baseline_energy_content(biomass, ncv = 15)
#' @export
calculate_baseline_energy_content <- function(non_renewable_biomass,
                                              biomass_col = "non_renewable_biomass_tonnes",
                                              ncv,
                                              output_col = "baseline_energy_mj") {
  biomass_tbl <- tibble::as_tibble(non_renewable_biomass)
  biomass_sym <- if (is.character(biomass_col)) {
    if (length(biomass_col) != 1) {
      stop("`biomass_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(biomass_col)
  } else {
    rlang::ensym(biomass_col)
  }
  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (!rlang::as_string(biomass_sym) %in% names(biomass_tbl)) {
    stop("`biomass_col` must be present in `non_renewable_biomass`.", call. = FALSE)
  }
  if (!is.numeric(ncv)) {
    stop("`ncv` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(biomass_tbl)
  if (!length(ncv) %in% c(1L, n_rows)) {
    stop("`ncv` must be length 1 or match the number of rows in `non_renewable_biomass`.", call. = FALSE)
  }
  ncv_values <- if (length(ncv) == 1L) rep(ncv, n_rows) else ncv

  biomass_tbl |>
    dplyr::mutate(!!output_sym := !!biomass_sym * ncv_values)
}

#' Calculate baseline emissions (Equation 3)
#'
#' Applies the baseline emission factor representing displaced non-renewable
#' biomass combustion to the baseline energy content estimated in Equation (2).
#'
#' @param energy_data Tibble containing baseline thermal energy, typically the
#'   output of [calculate_baseline_energy_content()].
#' @param energy_col Column name storing baseline energy in MJ.
#' @param emission_factor Baseline emission factor in tCO2e/MJ.
#' @param output_col Name of the resulting column containing baseline emissions
#'   in tCO2e.
#' @return Tibble with baseline emissions in tCO2e.
#' @examples
#' energy <- tibble::tibble(baseline_energy_mj = c(150000, 180000))
#' calculate_baseline_emissions(energy, emission_factor = 0.0001)
#' @export
calculate_baseline_emissions <- function(energy_data,
                                         energy_col = "baseline_energy_mj",
                                         emission_factor,
                                         output_col = "baseline_emissions_tco2e") {
  energy_tbl <- tibble::as_tibble(energy_data)
  energy_sym <- if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(energy_col)
  } else {
    rlang::ensym(energy_col)
  }
  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (!rlang::as_string(energy_sym) %in% names(energy_tbl)) {
    stop("`energy_col` must be present in `energy_data`.", call. = FALSE)
  }
  if (!is.numeric(emission_factor)) {
    stop("`emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(energy_tbl)
  if (!length(emission_factor) %in% c(1L, n_rows)) {
    stop(
      "`emission_factor` must be length 1 or match the number of rows in `energy_data`.",
      call. = FALSE
    )
  }

  factors <- if (length(emission_factor) == 1L) rep(emission_factor, n_rows) else emission_factor

  energy_tbl |>
    dplyr::mutate(!!output_sym := !!energy_sym * factors)
}

#' Calculate project emissions (Equation 4)
#'
#' Converts residual fossil energy consumption under the project scenario into
#' project emissions using the supplied emission factor.
#'
#' @param project_energy Tibble containing project fossil energy consumption.
#' @param energy_col Column with project energy in MJ.
#' @param project_emission_factor Project emission factor in tCO2e/MJ (default 0).
#' @param output_col Name of the resulting column containing project emissions in
#'   tCO2e.
#' @return Tibble with project emissions in tCO2e.
#' @examples
#' project <- tibble::tibble(project_energy_mj = c(1000, 2000))
#' calculate_project_emissions(project, project_emission_factor = 0.0001)
#' @export
calculate_project_emissions <- function(project_energy,
                                        energy_col = "project_energy_mj",
                                        project_emission_factor = 0,
                                        output_col = "project_emissions_tco2e") {
  project_tbl <- tibble::as_tibble(project_energy)
  energy_sym <- if (is.character(energy_col)) {
    if (length(energy_col) != 1) {
      stop("`energy_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(energy_col)
  } else {
    rlang::ensym(energy_col)
  }
  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  if (!rlang::as_string(energy_sym) %in% names(project_tbl)) {
    stop("`energy_col` must be present in `project_energy`.", call. = FALSE)
  }
  if (!is.numeric(project_emission_factor)) {
    stop("`project_emission_factor` must be numeric.", call. = FALSE)
  }

  n_rows <- nrow(project_tbl)
  if (!length(project_emission_factor) %in% c(1L, n_rows)) {
    stop("`project_emission_factor` must be length 1 or match the number of rows in `project_energy`.", call. = FALSE)
  }

  factors <- if (length(project_emission_factor) == 1L) {
    rep(project_emission_factor, n_rows)
  } else {
    project_emission_factor
  }

  project_tbl |>
    dplyr::mutate(!!output_sym := !!energy_sym * factors)
}

#' Calculate emission reductions (Equation 5)
#'
#' Implements Equation (5) of AMS-I.E by subtracting project emissions from
#' baseline emissions.
#'
#' @inheritParams calculate_project_emissions
#' @param baseline_emissions Tibble containing baseline emissions, typically the
#'   output of [calculate_baseline_emissions()].
#' @param project_emissions Tibble containing project emissions, typically the
#'   output of [calculate_project_emissions()].
#' @param baseline_col Name of the baseline emission column.
#' @param project_col Name of the project emission column.
#' @param output_col Name of the resulting emission reduction column.
#' @return Tibble containing emission reductions in tCO2e.
#' @examples
#' baseline <- tibble::tibble(baseline_emissions_tco2e = c(120, 150))
#' project <- tibble::tibble(project_emissions_tco2e = c(5, 6))
#' calculate_emission_reductions(baseline, project)
#' @export
calculate_emission_reductions <- function(baseline_emissions,
                                          project_emissions,
                                          baseline_col = "baseline_emissions_tco2e",
                                          project_col = "project_emissions_tco2e",
                                          output_col = "emission_reductions_tco2e") {
  baseline_sym <- if (is.character(baseline_col)) {
    if (length(baseline_col) != 1) {
      stop("`baseline_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(baseline_col)
  } else {
    rlang::ensym(baseline_col)
  }

  project_sym <- if (is.character(project_col)) {
    if (length(project_col) != 1) {
      stop("`project_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(project_col)
  } else {
    rlang::ensym(project_col)
  }

  output_sym <- if (is.character(output_col)) {
    if (length(output_col) != 1) {
      stop("`output_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(output_col)
  } else {
    rlang::ensym(output_col)
  }

  baseline_tbl <- dplyr::as_tibble(baseline_emissions)
  project_tbl <- dplyr::as_tibble(project_emissions)

  baseline_col_name <- rlang::as_string(baseline_sym)
  project_col_name <- rlang::as_string(project_sym)

  if (!baseline_col_name %in% names(baseline_tbl)) {
    stop("`baseline_col` must exist in `baseline_emissions`.", call. = FALSE)
  }
  if (!project_col_name %in% names(project_tbl)) {
    stop("`project_col` must exist in `project_emissions`.", call. = FALSE)
  }

  baseline_keys <- setdiff(names(baseline_tbl), baseline_col_name)
  project_keys <- setdiff(names(project_tbl), project_col_name)
  join_cols <- intersect(baseline_keys, project_keys)

  row_id_col <- NULL

  if (length(join_cols) == 0) {
    if (nrow(baseline_tbl) != nrow(project_tbl)) {
      stop("Unable to align baseline and project emissions without shared keys.", call. = FALSE)
    }
    row_id_col <- ".row_id"
    baseline_tbl[[row_id_col]] <- seq_len(nrow(baseline_tbl))
    project_tbl[[row_id_col]] <- seq_len(nrow(project_tbl))
    join_cols <- row_id_col
  }

  joined <- dplyr::left_join(
    baseline_tbl,
    project_tbl,
    by = join_cols
  )

  if (any(is.na(joined[[project_col_name]]))) {
    stop("`project_emissions` must contain values for each baseline record.", call. = FALSE)
  }

  result <- joined |>
    dplyr::mutate(!!output_sym := !!baseline_sym - !!project_sym)

  if (!is.null(row_id_col) && row_id_col %in% names(result)) {
    result <- dplyr::select(result, -dplyr::all_of(row_id_col))
  }

  result
}
