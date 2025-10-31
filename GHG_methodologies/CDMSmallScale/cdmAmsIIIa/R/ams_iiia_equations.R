#' Calculate baseline emissions from synthetic nitrogen fertilizer use
#'
#' Equation 1 of AMS-III.A multiplies the quantity of synthetic nitrogen applied
#' to leguminous crops by upstream production and field emission factors. The
#' helper accepts tidy data with fertilizer application quantities and returns a
#' tibble containing total baseline emissions in tCO2e.
#'
#' @param data Tibble containing baseline fertilizer monitoring data.
#' @param fertilizer_use_col Column storing the mass of nitrogen fertilizer
#'   applied (kg N).
#' @param production_emission_factor_col Column storing the upstream emission
#'   factor for synthetic fertilizer production (tCO2e per kg N).
#' @param field_emission_factor_col Column storing the direct soil emission factor
#'   (tCO2e per kg N).
#' @param group_cols Optional character vector identifying grouping columns.
#' @param output_col Name of the output column for baseline emissions. Defaults
#'   to `"baseline_emissions_tco2e"`.
#' @return Tibble with grouping columns (if supplied) and baseline emissions.
#' @examples
#' baseline <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   synthetic_n_applied_kg = c(120, 80),
#'   production_ef_tco2_per_kg = 0.004,
#'   field_ef_tco2_per_kg = 0.01
#' )
#' calculate_baseline_fertilizer_emissions_iiia(baseline)
#' @export
calculate_baseline_fertilizer_emissions_iiia <- function(data,
                                                        fertilizer_use_col = "synthetic_n_applied_kg",
                                                        production_emission_factor_col = "production_ef_tco2_per_kg",
                                                        field_emission_factor_col = "field_ef_tco2_per_kg",
                                                        group_cols = NULL,
                                                        output_col = "baseline_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  fertilizer_use_col <- if (rlang::is_string(fertilizer_use_col)) fertilizer_use_col else rlang::as_name(rlang::enquo(fertilizer_use_col))
  production_emission_factor_col <- if (rlang::is_string(production_emission_factor_col)) production_emission_factor_col else rlang::as_name(rlang::enquo(production_emission_factor_col))
  field_emission_factor_col <- if (rlang::is_string(field_emission_factor_col)) field_emission_factor_col else rlang::as_name(rlang::enquo(field_emission_factor_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .baseline_component = .data[[fertilizer_use_col]] * (
        .data[[production_emission_factor_col]] + .data[[field_emission_factor_col]]
      )
    )

  summary_tbl <- if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.baseline_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.baseline_component, na.rm = TRUE),
        .groups = "drop"
      )
  }

  summary_tbl
}

#' Calculate project emissions from residual fertilizer use
#'
#' Some AMS-III.A projects partially replace synthetic fertilizers rather than
#' fully displacing them. This helper mirrors the baseline calculation but
#' applies to the remaining synthetic nitrogen applied during the project.
#'
#' @inheritParams calculate_baseline_fertilizer_emissions_iiia
#' @param output_col Name of the output column for project fertilizer emissions.
#' @return Tibble with grouping columns (if supplied) and project fertilizer emissions.
#' @examples
#' project <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   synthetic_n_applied_kg = c(20, 10),
#'   production_ef_tco2_per_kg = 0.004,
#'   field_ef_tco2_per_kg = 0.01
#' )
#' calculate_project_residual_fertilizer_emissions_iiia(project)
#' @export
calculate_project_residual_fertilizer_emissions_iiia <- function(data,
                                                                 fertilizer_use_col = "synthetic_n_applied_kg",
                                                                 production_emission_factor_col = "production_ef_tco2_per_kg",
                                                                 field_emission_factor_col = "field_ef_tco2_per_kg",
                                                                 group_cols = NULL,
                                                                 output_col = "project_fertilizer_emissions_tco2e") {
  calculate_baseline_fertilizer_emissions_iiia(
    data = data,
    fertilizer_use_col = fertilizer_use_col,
    production_emission_factor_col = production_emission_factor_col,
    field_emission_factor_col = field_emission_factor_col,
    group_cols = group_cols,
    output_col = output_col
  )
}

#' Calculate emissions from inoculant production and application
#'
#' Equation 2 covers emissions attributable to producing, transporting, and
#' applying the biological inoculant. Users provide the inoculant application rate
#' per hectare, the planted legume area, and an aggregate emission factor.
#'
#' @param data Tibble containing project monitoring data.
#' @param inoculant_rate_col Column storing inoculant application rate per hectare
#'   (kg/ha).
#' @param area_planted_col Column storing the planted area (ha).
#' @param inoculant_emission_factor_col Column storing emission factors for the
#'   inoculant lifecycle (tCO2e per kg).
#' @param group_cols Optional character vector identifying grouping columns.
#' @param output_col Name of the output column. Defaults to
#'   `"project_inoculant_emissions_tco2e"`.
#' @return Tibble with inoculant emissions aggregated by group (if supplied).
#' @examples
#' project <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   inoculant_rate_kg_per_ha = c(0.5, 0.4),
#'   legume_area_ha = c(30, 25),
#'   inoculant_ef_tco2_per_kg = 0.002
#' )
#' calculate_project_inoculant_emissions_iiia(project)
#' @export
calculate_project_inoculant_emissions_iiia <- function(data,
                                                       inoculant_rate_col = "inoculant_rate_kg_per_ha",
                                                       area_planted_col = "legume_area_ha",
                                                       inoculant_emission_factor_col = "inoculant_ef_tco2_per_kg",
                                                       group_cols = NULL,
                                                       output_col = "project_inoculant_emissions_tco2e") {
  data_tbl <- tibble::as_tibble(data)
  inoculant_rate_col <- if (rlang::is_string(inoculant_rate_col)) inoculant_rate_col else rlang::as_name(rlang::enquo(inoculant_rate_col))
  area_planted_col <- if (rlang::is_string(area_planted_col)) area_planted_col else rlang::as_name(rlang::enquo(area_planted_col))
  inoculant_emission_factor_col <- if (rlang::is_string(inoculant_emission_factor_col)) inoculant_emission_factor_col else rlang::as_name(rlang::enquo(inoculant_emission_factor_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  emissions <- data_tbl |>
    dplyr::mutate(
      .inoculant_component = .data[[inoculant_rate_col]] * .data[[area_planted_col]] * .data[[inoculant_emission_factor_col]]
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := sum(emissions$.inoculant_component, na.rm = TRUE))
  } else {
    emissions |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := sum(.data$.inoculant_component, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate leakage emissions for AMS-III.A
#'
#' Leakage may arise when displaced fertilizer is used elsewhere or when inoculant
#' production triggers upstream emissions outside the project boundary. Provide a
#' tibble containing leakage emissions in tCO2e.
#'
#' @param data Tibble containing leakage observations.
#' @param leakage_col Column storing leakage emissions in tCO2e.
#' @param group_cols Optional character vector identifying grouping columns.
#' @return Tibble with leakage emissions aggregated by group (if supplied).
#' @examples
#' leakage <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   leakage_emissions_tco2e = c(0.5, 0.1)
#' )
#' calculate_leakage_emissions_iiia(leakage)
#' @export
calculate_leakage_emissions_iiia <- function(data,
                                             leakage_col = "leakage_emissions_tco2e",
                                             group_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  leakage_col <- if (rlang::is_string(leakage_col)) leakage_col else rlang::as_name(rlang::enquo(leakage_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  if (length(groups) == 0) {
    tibble::tibble(leakage_emissions_tco2e = sum(data_tbl[[leakage_col]], na.rm = TRUE))
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        leakage_emissions_tco2e = sum(.data[[leakage_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Aggregate monitoring periods for AMS-III.A
#'
#' The methodology reports emission reductions over monitoring periods such as
#' agricultural seasons. This helper aggregates period-level results produced by
#' the equation helpers to annual or campaign totals.
#'
#' @param data Tibble containing period-level emissions with columns matching the
#'   outputs from `calculate_baseline_fertilizer_emissions_iiia`,
#'   `calculate_project_residual_fertilizer_emissions_iiia`, and
#'   `calculate_project_inoculant_emissions_iiia`.
#' @param group_cols Character vector identifying group columns (e.g. farm ID).
#' @param monitoring_col Column storing the monitoring period identifier.
#' @return Tibble aggregated to the grouping columns with summed emissions.
#' @examples
#' results <- tibble::tibble(
#'   farm_id = rep("A", 2),
#'   season = 1:2,
#'   baseline_emissions_tco2e = c(3, 2.5),
#'   project_fertilizer_emissions_tco2e = c(0.4, 0.5),
#'   project_inoculant_emissions_tco2e = c(0.2, 0.2)
#' )
#' aggregate_monitoring_periods_iiia(results, group_cols = "farm_id", monitoring_col = "season")
#' @export
aggregate_monitoring_periods_iiia <- function(data,
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
      project_fertilizer_emissions_tco2e = sum(.data$project_fertilizer_emissions_tco2e, na.rm = TRUE),
      project_inoculant_emissions_tco2e = sum(.data$project_inoculant_emissions_tco2e, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate emission reductions for AMS-III.A
#'
#' This helper combines baseline emissions, project emissions, and leakage to
#' estimate net emission reductions in line with AMS-III.A.
#'
#' @param baseline_emissions Tibble produced by
#'   `calculate_baseline_fertilizer_emissions_iiia`.
#' @param project_emissions Tibble containing project fertilizer and inoculant
#'   emissions. Typically generated by binding the outputs from
#'   `calculate_project_residual_fertilizer_emissions_iiia` and
#'   `calculate_project_inoculant_emissions_iiia`.
#' @param leakage_emissions Optional tibble produced by
#'   `calculate_leakage_emissions_iiia`.
#' @param group_cols Optional character vector identifying grouping columns.
#' @return Tibble containing baseline emissions, project emissions, leakage, and
#'   net emission reductions.
#' @examples
#' baseline <- tibble::tibble(baseline_emissions_tco2e = 5)
#' project <- tibble::tibble(project_fertilizer_emissions_tco2e = 0.5,
#'   project_inoculant_emissions_tco2e = 0.2)
#' calculate_emission_reductions_iiia(baseline, project)
#' @export
calculate_emission_reductions_iiia <- function(baseline_emissions,
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
      dplyr::select(dplyr::all_of(c(join_cols,
        "project_fertilizer_emissions_tco2e",
        "project_inoculant_emissions_tco2e")))
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
      project_emissions_tco2e = dplyr::coalesce(.data$project_fertilizer_emissions_tco2e, 0) +
        dplyr::coalesce(.data$project_inoculant_emissions_tco2e, 0),
      emission_reductions_tco2e = .data$baseline_emissions_tco2e -
        .data$project_emissions_tco2e - dplyr::coalesce(.data$leakage_emissions_tco2e, 0)
    )
}
