#' Estimate emission reductions under AMS-I.J
#'
#' Composes the equation-level helpers to compute emission reductions for
#' datasets describing useful thermal output from solar water heating systems and
#' the associated baseline and auxiliary emission factors.
#'
#' @param thermal_data Tibble containing useful solar thermal output and optional
#'   auxiliary energy in MWhth.
#' @param baseline_emission_factor Baseline emission factor in tCO2e/MWhth.
#' @param auxiliary_emission_factor Auxiliary system emission factor in
#'   tCO2e/MWhth.
#' @param group_cols Optional character vector specifying grouping columns in
#'   `thermal_data`.
#' @param useful_energy_col Column name storing useful thermal output (default
#'   `"useful_heat_mwh"`).
#' @param auxiliary_energy_col Column name storing auxiliary energy consumption
#'   (default `"auxiliary_energy_mwh"`). Set to `NULL` when auxiliary energy is
#'   not recorded.
#' @return Tibble with useful thermal output, baseline emissions, project
#'   emissions, and emission reductions.
#' @examples
#' data <- tibble::tibble(
#'   site_id = c("A", "B"),
#'   useful_heat_mwh = c(500, 420),
#'   auxiliary_energy_mwh = c(40, 25)
#' )
#' estimate_emission_reductions_ams_ij(
#'   data,
#'   baseline_emission_factor = 0.22,
#'   auxiliary_emission_factor = 0.19,
#'   group_cols = "site_id"
#' )
#' @export
estimate_emission_reductions_ams_ij <- function(thermal_data,
                                                baseline_emission_factor,
                                                auxiliary_emission_factor = 0,
                                                group_cols = NULL,
                                                useful_energy_col = "useful_heat_mwh",
                                                auxiliary_energy_col = "auxiliary_energy_mwh") {
  data_tbl <- tibble::as_tibble(thermal_data)

  internal_group_cols <- group_cols
  added_group <- FALSE
  if (is.null(internal_group_cols) || length(internal_group_cols) == 0) {
    new_col <- ".group_id"
    while (new_col %in% names(data_tbl)) {
      new_col <- paste0(new_col, "_x")
    }
    data_tbl[[new_col]] <- 1L
    internal_group_cols <- new_col
    added_group <- TRUE
  }

  useful_output <- calculate_useful_thermal_output(
    thermal_data = data_tbl,
    energy_col = useful_energy_col,
    group_cols = internal_group_cols
  )

  baseline_emissions <- calculate_baseline_emissions(
    useful_output = useful_output,
    baseline_emission_factor = baseline_emission_factor
  )

  if (is.null(auxiliary_energy_col)) {
    project_emissions <- useful_output |>
      dplyr::mutate(project_emissions_tco2e = 0)
  } else {
    project_emissions <- calculate_project_emissions_auxiliary(
      auxiliary_data = data_tbl,
      energy_col = auxiliary_energy_col,
      emission_factor = auxiliary_emission_factor,
      group_cols = internal_group_cols
    )
  }

  reductions <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions
  )

  if (added_group) {
    reductions <- reductions |>
      dplyr::select(-dplyr::all_of(internal_group_cols))
  }

  reductions
}

#' Aggregate AMS-I.J monitoring results across periods
#'
#' Summarises useful thermal output and auxiliary energy for each monitoring
#' period, returning baseline emissions, project emissions, and emission
#' reductions. The helper leverages the equation-level functions to maintain
#' consistency with AMS-I.J calculations.
#'
#' @param thermal_data Tibble containing monitoring observations, including the
#'   columns listed in `group_cols` and `monitoring_cols` plus useful thermal
#'   output and auxiliary energy information.
#' @param monitoring_cols Character vector specifying the columns that define a
#'   monitoring period.
#' @param group_cols Character vector specifying entity-level identifiers (e.g.
#'   `site_id`).
#' @param useful_energy_col Name of the column with useful thermal output for
#'   each observation in MWhth.
#' @param auxiliary_energy_col Name of the column storing auxiliary energy in
#'   MWhth. Set to `NULL` when auxiliary energy is not measured.
#' @param baseline_factor_col Name of the column storing the baseline emission
#'   factor.
#' @param auxiliary_factor_col Name of the column storing the auxiliary emission
#'   factor.
#' @return Tibble aggregated by entity and monitoring period with columns for
#'   useful thermal output, baseline emissions, project emissions, and emission
#'   reductions.
#' @examples
#' data <- simulate_ams_ij_dataset(n_sites = 2, n_periods = 3)
#' aggregate_monitoring_periods(
#'   data,
#'   monitoring_cols = c("year", "month"),
#'   group_cols = "site_id"
#' )
#' @export
aggregate_monitoring_periods <- function(thermal_data,
                                         monitoring_cols = c("year", "month"),
                                         group_cols = "site_id",
                                         useful_energy_col = "useful_heat_mwh",
                                         auxiliary_energy_col = "auxiliary_energy_mwh",
                                         baseline_factor_col = "baseline_emission_factor",
                                         auxiliary_factor_col = "auxiliary_emission_factor") {
  data_tbl <- tibble::as_tibble(thermal_data)

  useful_sym <- if (is.character(useful_energy_col)) {
    if (length(useful_energy_col) != 1) {
      stop("`useful_energy_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(useful_energy_col)
  } else {
    rlang::ensym(useful_energy_col)
  }

  if (!rlang::as_string(useful_sym) %in% names(data_tbl)) {
    stop("`useful_energy_col` must be present in `thermal_data`.", call. = FALSE)
  }

  auxiliary_sym <- NULL
  if (!is.null(auxiliary_energy_col)) {
    auxiliary_sym <- if (is.character(auxiliary_energy_col)) {
      if (length(auxiliary_energy_col) != 1) {
        stop("`auxiliary_energy_col` must be a single column name.", call. = FALSE)
      }
      rlang::sym(auxiliary_energy_col)
    } else {
      rlang::ensym(auxiliary_energy_col)
    }

    if (!rlang::as_string(auxiliary_sym) %in% names(data_tbl)) {
      stop("`auxiliary_energy_col` must be present in `thermal_data`.", call. = FALSE)
    }
  }

  baseline_factor_sym <- if (is.character(baseline_factor_col)) {
    if (length(baseline_factor_col) != 1) {
      stop("`baseline_factor_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(baseline_factor_col)
  } else {
    rlang::ensym(baseline_factor_col)
  }

  auxiliary_factor_sym <- if (is.null(auxiliary_factor_col)) {
    NULL
  } else if (is.character(auxiliary_factor_col)) {
    if (length(auxiliary_factor_col) != 1) {
      stop("`auxiliary_factor_col` must be a single column name.", call. = FALSE)
    }
    rlang::sym(auxiliary_factor_col)
  } else {
    rlang::ensym(auxiliary_factor_col)
  }

  keys <- unique(c(group_cols, monitoring_cols))
  required_cols <- unique(c(keys, rlang::as_string(useful_sym), rlang::as_string(baseline_factor_sym)))
  if (!is.null(auxiliary_sym)) {
    required_cols <- unique(c(required_cols, rlang::as_string(auxiliary_sym)))
  }
  if (!is.null(auxiliary_factor_sym)) {
    required_cols <- unique(c(required_cols, rlang::as_string(auxiliary_factor_sym)))
  }

  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`thermal_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  useful_output <- calculate_useful_thermal_output(
    thermal_data = data_tbl,
    energy_col = useful_energy_col,
    group_cols = keys
  )

  baseline_factor_name <- rlang::as_string(baseline_factor_sym)
  auxiliary_factor_name <- if (!is.null(auxiliary_factor_sym)) {
    rlang::as_string(auxiliary_factor_sym)
  } else {
    "auxiliary_factor"
  }

  factor_summary <- data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      baseline_factor = dplyr::first(!!baseline_factor_sym),
      auxiliary_factor = if (!is.null(auxiliary_factor_sym)) {
        dplyr::first(!!auxiliary_factor_sym)
      } else {
        0
      },
      .groups = "drop"
    ) |>
    dplyr::rename(
      !!baseline_factor_name := baseline_factor,
      !!auxiliary_factor_name := auxiliary_factor
    )

  auxiliary_factor_sym_output <- rlang::sym(auxiliary_factor_name)

  baseline_with_factors <- dplyr::left_join(
    useful_output,
    factor_summary |>
      dplyr::select(dplyr::all_of(keys), dplyr::all_of(baseline_factor_name)),
    by = keys
  )

  baseline_emissions <- calculate_baseline_emissions(
    useful_output = baseline_with_factors,
    baseline_emission_factor = baseline_with_factors[[baseline_factor_name]],
    output_col = "baseline_emissions_tco2e"
  )

  if (is.null(auxiliary_sym)) {
    project_emissions <- baseline_with_factors |>
      dplyr::select(dplyr::all_of(keys)) |>
      dplyr::mutate(project_emissions_tco2e = 0)
  } else {
    auxiliary_summary <- data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(
        auxiliary_energy_mwh = sum(!!auxiliary_sym, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::left_join(
        factor_summary |>
          dplyr::select(dplyr::all_of(keys), dplyr::all_of(auxiliary_factor_name)),
        by = keys
      ) |>
      dplyr::mutate(project_emissions_tco2e = auxiliary_energy_mwh * !!auxiliary_factor_sym_output)

    project_emissions <- auxiliary_summary |>
      dplyr::select(dplyr::all_of(keys), project_emissions_tco2e)
  }

  emission_reductions <- calculate_emission_reductions(
    baseline_emissions = baseline_emissions |>
      dplyr::select(dplyr::all_of(keys), baseline_emissions_tco2e),
    project_emissions = project_emissions,
    baseline_col = "baseline_emissions_tco2e",
    project_col = "project_emissions_tco2e",
    output_col = "emission_reductions_tco2e"
  )

  emission_reductions |>
    dplyr::left_join(
      baseline_with_factors |>
        dplyr::select(dplyr::all_of(keys), useful_thermal_output_mwh, dplyr::all_of(baseline_factor_name)),
      by = keys
    ) |>
    dplyr::left_join(
      factor_summary |>
        dplyr::select(dplyr::all_of(keys), dplyr::all_of(auxiliary_factor_name)),
      by = keys
    ) |>
    dplyr::relocate(dplyr::all_of(keys)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
}
