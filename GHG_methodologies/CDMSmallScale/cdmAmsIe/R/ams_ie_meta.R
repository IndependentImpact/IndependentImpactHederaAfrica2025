#' Estimate emission reductions under AMS-I.E
#'
#' Chains the AMS-I.E equation helpers to translate monitoring data describing
#' biomass consumption, non-renewable fractions, and project fossil fuel use
#' into emission reductions.
#'
#' @param biomass_data Tibble containing monitoring observations.
#' @param consumption_col Column with biomass consumption (e.g. tonnes).
#' @param fraction_col Column with the fraction of biomass that is non-renewable.
#'   If `NULL`, the scalar `fraction` is applied to all observations.
#' @param fraction Scalar fraction of biomass that is non-renewable when
#'   `fraction_col` is `NULL`.
#' @param ncv Net calorific value in MJ per unit of biomass. Supply either a
#'   single value or a vector matching the number of rows after grouping.
#' @param group_cols Optional character vector specifying grouping columns (e.g.
#'   site identifiers).
#' @param emission_factor Baseline emission factor in tCO2e/MJ.
#' @param project_energy_col Optional column with project fossil energy in MJ.
#' @param project_emission_factor Project emission factor in tCO2e/MJ (default 0).
#' @return Tibble containing non-renewable biomass, baseline energy, baseline
#'   emissions, project emissions, and emission reductions.
#' @examples
#' monitoring <- tibble::tibble(
#'   site_id = c("A", "A", "B"),
#'   biomass_tonnes = c(10, 8, 12),
#'   non_renewable_fraction = c(0.85, 0.8, 0.9)
#' )
#' estimate_emission_reductions_ams_ie(
#'   monitoring,
#'   consumption_col = "biomass_tonnes",
#'   fraction_col = "non_renewable_fraction",
#'   ncv = 15,
#'   emission_factor = 0.0001,
#'   group_cols = "site_id"
#' )
#' @export
estimate_emission_reductions_ams_ie <- function(biomass_data,
                                                consumption_col = "biomass_consumption_tonnes",
                                                fraction_col = "non_renewable_fraction",
                                                fraction = 1,
                                                ncv,
                                                group_cols = NULL,
                                                emission_factor,
                                                project_energy_col = NULL,
                                                project_emission_factor = 0) {
  keys <- if (is.null(group_cols) || length(group_cols) == 0) character() else unique(group_cols)

  non_renewable <- calculate_non_renewable_biomass(
    biomass_data = biomass_data,
    consumption_col = consumption_col,
    fraction = fraction,
    fraction_col = fraction_col,
    group_cols = keys
  )

  baseline_energy <- calculate_baseline_energy_content(
    non_renewable_biomass = non_renewable,
    ncv = ncv
  )

  baseline_emissions <- calculate_baseline_emissions(
    energy_data = baseline_energy,
    emission_factor = emission_factor
  )

  project_sym <- if (!is.null(project_energy_col)) rlang::sym(project_energy_col) else NULL

  if (!is.null(project_energy_col)) {
    project_tbl <- dplyr::as_tibble(biomass_data)
    if (!project_energy_col %in% names(project_tbl)) {
      stop("`project_energy_col` must exist in `biomass_data` when supplied.", call. = FALSE)
    }
    select_cols <- unique(c(keys, project_energy_col))
    project_tbl <- project_tbl |>
      dplyr::select(dplyr::all_of(select_cols))
    if (length(keys) > 0) {
      project_tbl <- project_tbl |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
        dplyr::summarise(project_energy_mj = sum(!!project_sym, na.rm = TRUE), .groups = "drop")
    } else {
      project_tbl <- dplyr::summarise(project_tbl, project_energy_mj = sum(!!project_sym, na.rm = TRUE))
    }
  } else {
    if (length(keys) > 0) {
      project_tbl <- baseline_energy |>
        dplyr::select(dplyr::all_of(keys)) |>
        dplyr::mutate(project_energy_mj = 0)
    } else {
      project_tbl <- tibble::tibble(project_energy_mj = 0)
    }
  }

  project_emissions <- calculate_project_emissions(
    project_energy = project_tbl,
    project_emission_factor = project_emission_factor
  )

  if (length(keys) == 0) {
    baseline_emissions$.__all__ <- 1
    project_emissions$.__all__ <- 1
  }

  result <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )

  if (length(keys) == 0) {
    result$.__all__ <- NULL
  } else {
    result <- result |>
      dplyr::relocate(dplyr::all_of(keys), .before = dplyr::everything()) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  }

  result
}

#' Aggregate monitoring periods for AMS-I.E
#'
#' Summarises monitoring data across reporting periods, combining baseline
#' biomass consumption, non-renewable fractions, and project fossil energy to
#' return period-level emission reductions.
#'
#' @param biomass_data Tibble containing monitoring observations.
#' @param monitoring_cols Character vector specifying the columns that define a
#'   monitoring period (e.g. year, quarter).
#' @param group_cols Character vector specifying entity-level identifiers (e.g.
#'   household, community, or site IDs).
#' @param consumption_col Name of the column with biomass consumption.
#' @param fraction_col Name of the column with non-renewable biomass fraction.
#' @param ncv_col Name of the column with net calorific value in MJ per unit of
#'   biomass.
#' @param emission_factor_col Column containing the baseline emission factor in
#'   tCO2e/MJ.
#' @param project_energy_col Column containing project fossil energy in MJ.
#' @param project_emission_factor_col Optional column containing the project
#'   emission factor in tCO2e/MJ. Defaults to using `project_emission_factor`
#'   when `NULL`.
#' @param project_emission_factor Default project emission factor in tCO2e/MJ
#'   when `project_emission_factor_col` is `NULL` (default 0).
#' @return Tibble aggregated by entity and monitoring period with columns for
#'   non-renewable biomass, baseline energy, baseline emissions, project
#'   emissions, and emission reductions.
#' @examples
#' data <- simulate_ams_ie_dataset(n_users = 2, n_periods = 3)
#' aggregate_monitoring_periods(data)
#' @export
aggregate_monitoring_periods <- function(biomass_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "user_id",
                                         consumption_col = "biomass_consumption_tonnes",
                                         fraction_col = "non_renewable_fraction",
                                         ncv_col = "net_calorific_value",
                                         emission_factor_col = "emission_factor",
                                         project_energy_col = "project_energy_mj",
                                         project_emission_factor_col = NULL,
                                         project_emission_factor = 0) {
  data_tbl <- tibble::as_tibble(biomass_data)

  required_cols <- unique(c(
    group_cols,
    monitoring_cols,
    consumption_col,
    fraction_col,
    ncv_col,
    emission_factor_col,
    project_energy_col,
    project_emission_factor_col
  ))
  required_cols <- required_cols[!is.na(required_cols) & nzchar(required_cols)]

  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`biomass_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  keys <- unique(c(group_cols, monitoring_cols))

  emission_factor_sym <- rlang::sym(emission_factor_col)
  project_emission_factor_sym <- if (!is.null(project_emission_factor_col)) {
    rlang::sym(project_emission_factor_col)
  } else {
    NULL
  }

  project_sym <- rlang::sym(project_energy_col)

  fraction_values <- data_tbl[[fraction_col]]
  if (!is.numeric(fraction_values)) {
    stop("`fraction_col` must contain numeric values.", call. = FALSE)
  }

  energy_expr <- data_tbl[[consumption_col]] * fraction_values * data_tbl[[ncv_col]]

  data_tbl$`__non_renewable_biomass` <- data_tbl[[consumption_col]] * fraction_values
  data_tbl$`__baseline_energy` <- energy_expr

  summary_tbl <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      non_renewable_biomass_tonnes = sum(`__non_renewable_biomass`, na.rm = TRUE),
      baseline_energy_mj = sum(`__baseline_energy`, na.rm = TRUE),
      emission_factor = dplyr::first(!!emission_factor_sym),
      project_energy_mj = sum(!!project_sym, na.rm = TRUE),
      project_emission_factor = if (!is.null(project_emission_factor_col)) {
        dplyr::first(!!project_emission_factor_sym)
      } else {
        project_emission_factor
      },
      .groups = "drop"
    )

  baseline_emissions <- calculate_baseline_emissions(
    energy_data = summary_tbl,
    emission_factor = summary_tbl$emission_factor
  )

  project_emissions <- calculate_project_emissions(
    project_energy = summary_tbl,
    project_emission_factor = summary_tbl$project_emission_factor
  )

  reductions <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )

  reductions |>
    dplyr::select(
      dplyr::all_of(keys),
      non_renewable_biomass_tonnes,
      baseline_energy_mj,
      baseline_emissions_tco2e,
      project_energy_mj,
      project_emissions_tco2e,
      emission_reductions_tco2e
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
}
