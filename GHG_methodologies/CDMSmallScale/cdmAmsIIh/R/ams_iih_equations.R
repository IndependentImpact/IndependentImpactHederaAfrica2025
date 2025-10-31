#' Calculate baseline decentralized utility emissions (Equation 1)
#'
#' Equation (1) of AMS-II.H consolidates the greenhouse gas emissions that
#' would occur when industrial utilities such as steam, hot water, or chilled
#' water are generated in a decentralized manner. The function multiplies
#' baseline fuel use by its emission factor and optionally reports the specific
#' energy consumption of the decentralized units for diagnostic purposes.
#'
#' @param baseline_data A tibble describing counterfactual decentralized
#'   operation.
#' @param fuel_consumption_col Column storing baseline fuel use (in GJ).
#' @param emission_factor_col Column storing baseline emission factors
#'   (tCO2e/GJ).
#' @param useful_output_col Optional column storing useful thermal output from
#'   the decentralized units (GJ). When supplied, the function also returns the
#'   baseline specific energy consumption (fuel per unit of useful output).
#' @param group_cols Optional character vector of grouping columns to retain.
#' @param output_col Name of the emissions column in the returned tibble.
#'   Defaults to `"baseline_emissions_tco2e"`.
#' @param specific_energy_col Name of the optional specific energy column that
#'   is returned when `useful_output_col` is provided. Defaults to
#'   `"baseline_specific_energy_gj_per_gj"`.
#' @return A tibble containing grouped baseline emissions in tCO2e and, when
#'   requested, the baseline specific energy consumption in GJ/GJ.
#' @examples
#' baseline <- tibble::tibble(
#'   line = c("Boiler_A", "Boiler_B"),
#'   baseline_fuel_use_gj = c(4200, 3150),
#'   baseline_emission_factor_tco2_per_gj = c(0.071, 0.071),
#'   baseline_useful_output_gj = c(3600, 2700)
#' )
#' calculate_baseline_decentralized_emissions_iih(
#'   baseline,
#'   group_cols = "line"
#' )
#' @export
calculate_baseline_decentralized_emissions_iih <- function(baseline_data,
                                                           fuel_consumption_col = "baseline_fuel_use_gj",
                                                           emission_factor_col = "baseline_emission_factor_tco2_per_gj",
                                                           useful_output_col = "baseline_useful_output_gj",
                                                           group_cols = NULL,
                                                           output_col = "baseline_emissions_tco2e",
                                                           specific_energy_col = "baseline_specific_energy_gj_per_gj") {
  data_tbl <- tibble::as_tibble(baseline_data)
  fuel_sym <- rlang::ensym(fuel_consumption_col)
  factor_sym <- rlang::ensym(emission_factor_col)
  output_sym <- rlang::ensym(output_col)

  missing_cols <- setdiff(c(rlang::as_string(fuel_sym), rlang::as_string(factor_sym)), names(data_tbl))
  if (length(missing_cols) > 0) {
    stop("Specified fuel and emission factor columns must exist in the data frame: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  optional_output_sym <- if (!is.null(useful_output_col)) rlang::ensym(useful_output_col) else NULL
  if (!is.null(optional_output_sym)) {
    optional_missing <- setdiff(rlang::as_string(optional_output_sym), names(data_tbl))
    if (length(optional_missing) > 0) {
      stop("Specified useful output column must exist in the data frame: ",
        paste(optional_missing, collapse = ", "), call. = FALSE
      )
    }
  }
  specific_sym <- if (!is.null(optional_output_sym)) rlang::ensym(specific_energy_col) else NULL

  compute_group <- function(tbl) {
    result <- tbl |>
      dplyr::mutate(
        .fuel_use = dplyr::coalesce(!!fuel_sym, 0),
        .emissions = .fuel_use * dplyr::coalesce(!!factor_sym, 0)
      ) |>
      dplyr::summarise(
        !!output_sym := sum(.emissions, na.rm = TRUE),
        .fuel_total = sum(.fuel_use, na.rm = TRUE),
        .output_total = if (!is.null(optional_output_sym)) sum(dplyr::coalesce(!!optional_output_sym, 0), na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    if (!is.null(optional_output_sym)) {
      result[[rlang::as_string(specific_sym)]] <- result$.fuel_total / result$.output_total
      result[[rlang::as_string(specific_sym)]] <- ifelse(is.finite(result[[rlang::as_string(specific_sym)]]),
        result[[rlang::as_string(specific_sym)]],
        NA_real_
      )
    }

    dplyr::select(result, -dplyr::any_of(c(".fuel_total", ".output_total")))
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    compute_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      compute_group()
  }
}

#' Calculate project centralized utility emissions (Equation 2)
#'
#' Equation (2) of AMS-II.H quantifies greenhouse gas emissions from the
#' centralized utility system implemented by the project. The function multiplies
#' fuel use by emission factors and, when a useful output column is supplied,
#' returns the project specific energy consumption for comparison with the
#' baseline.
#'
#' @param project_data A tibble describing monitored project operation.
#' @param fuel_consumption_col Column storing project fuel use (GJ).
#' @param emission_factor_col Column storing emission factors (tCO2e/GJ).
#' @param useful_output_col Optional column storing useful thermal output (GJ).
#' @param group_cols Optional character vector of grouping variables to retain.
#' @param output_col Name of the emissions column in the returned tibble.
#'   Defaults to `"project_central_emissions_tco2e"`.
#' @param specific_energy_col Name of the optional specific energy column that is
#'   returned when `useful_output_col` is provided. Defaults to
#'   `"project_specific_energy_gj_per_gj"`.
#' @return A tibble containing grouped project emissions and optional specific
#'   energy consumption values.
#' @examples
#' project <- tibble::tibble(
#'   line = c("Header_1", "Header_2"),
#'   project_fuel_use_gj = c(3000, 2250),
#'   project_emission_factor_tco2_per_gj = c(0.068, 0.068),
#'   project_useful_output_gj = c(2880, 2160)
#' )
#' calculate_project_central_emissions_iih(project, group_cols = "line")
#' @export
calculate_project_central_emissions_iih <- function(project_data,
                                                    fuel_consumption_col = "project_fuel_use_gj",
                                                    emission_factor_col = "project_emission_factor_tco2_per_gj",
                                                    useful_output_col = "project_useful_output_gj",
                                                    group_cols = NULL,
                                                    output_col = "project_central_emissions_tco2e",
                                                    specific_energy_col = "project_specific_energy_gj_per_gj") {
  data_tbl <- tibble::as_tibble(project_data)
  fuel_sym <- rlang::ensym(fuel_consumption_col)
  factor_sym <- rlang::ensym(emission_factor_col)
  output_sym <- rlang::ensym(output_col)

  missing_cols <- setdiff(c(rlang::as_string(fuel_sym), rlang::as_string(factor_sym)), names(data_tbl))
  if (length(missing_cols) > 0) {
    stop("Specified fuel and emission factor columns must exist in the data frame: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  optional_output_sym <- if (!is.null(useful_output_col)) rlang::ensym(useful_output_col) else NULL
  if (!is.null(optional_output_sym)) {
    optional_missing <- setdiff(rlang::as_string(optional_output_sym), names(data_tbl))
    if (length(optional_missing) > 0) {
      stop("Specified useful output column must exist in the data frame: ",
        paste(optional_missing, collapse = ", "), call. = FALSE
      )
    }
  }
  specific_sym <- if (!is.null(optional_output_sym)) rlang::ensym(specific_energy_col) else NULL

  compute_group <- function(tbl) {
    result <- tbl |>
      dplyr::mutate(
        .fuel_use = dplyr::coalesce(!!fuel_sym, 0),
        .emissions = .fuel_use * dplyr::coalesce(!!factor_sym, 0)
      ) |>
      dplyr::summarise(
        !!output_sym := sum(.emissions, na.rm = TRUE),
        .fuel_total = sum(.fuel_use, na.rm = TRUE),
        .output_total = if (!is.null(optional_output_sym)) sum(dplyr::coalesce(!!optional_output_sym, 0), na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    if (!is.null(optional_output_sym)) {
      result[[rlang::as_string(specific_sym)]] <- result$.fuel_total / result$.output_total
      result[[rlang::as_string(specific_sym)]] <- ifelse(is.finite(result[[rlang::as_string(specific_sym)]]),
        result[[rlang::as_string(specific_sym)]],
        NA_real_
      )
    }

    dplyr::select(result, -dplyr::any_of(c(".fuel_total", ".output_total")))
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    compute_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      compute_group()
  }
}

#' Calculate project auxiliary electricity emissions (Equation 3)
#'
#' The centralized utility system may require auxiliary electricity for pumps,
#' controls, or distribution equipment. Equation (3) of AMS-II.H converts the
#' monitored electricity use into emissions using grid emission factors. When no
#' auxiliary electricity is consumed the function returns zeros.
#'
#' @param project_data Tibble containing monitored auxiliary electricity use.
#' @param electricity_consumption_col Column storing auxiliary electricity
#'   consumption (MWh). Set to `NULL` to return zeros.
#' @param emission_factor_col Column storing grid emission factors (tCO2e/MWh).
#'   Required when `electricity_consumption_col` is provided.
#' @param group_cols Optional character vector of grouping variables.
#' @param output_col Name of the resulting emissions column. Defaults to
#'   `"project_auxiliary_emissions_tco2e"`.
#' @return A tibble with grouped auxiliary electricity emissions in tCO2e.
#' @examples
#' monitoring <- tibble::tibble(
#'   facility = c("Plant_A", "Plant_B"),
#'   project_auxiliary_electricity_mwh = c(240, 180),
#'   project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.55)
#' )
#' calculate_project_auxiliary_electricity_iih(monitoring, group_cols = "facility")
#' @export
calculate_project_auxiliary_electricity_iih <- function(project_data,
                                                        electricity_consumption_col = "project_auxiliary_electricity_mwh",
                                                        emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
                                                        group_cols = NULL,
                                                        output_col = "project_auxiliary_emissions_tco2e") {
  output_sym <- rlang::ensym(output_col)

  if (is.null(electricity_consumption_col)) {
    base_tbl <- tibble::as_tibble(project_data)
    if (!is.null(group_cols) && length(group_cols) > 0) {
      base_tbl <- base_tbl |>
        dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))
      base_tbl[[rlang::as_string(output_sym)]] <- 0
      base_tbl
    } else {
      tibble::tibble(!!output_sym := 0)
    }
  } else {
    data_tbl <- tibble::as_tibble(project_data)
    electricity_sym <- rlang::ensym(electricity_consumption_col)
    factor_sym <- rlang::ensym(emission_factor_col)

    missing_cols <- setdiff(c(rlang::as_string(electricity_sym), rlang::as_string(factor_sym)), names(data_tbl))
    if (length(missing_cols) > 0) {
      stop("Specified electricity columns must exist in the data frame: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    compute_group <- function(tbl) {
      tbl |>
        dplyr::mutate(
          .electricity = dplyr::coalesce(!!electricity_sym, 0),
          .emissions = .electricity * dplyr::coalesce(!!factor_sym, 0)
        ) |>
        dplyr::summarise(!!output_sym := sum(.emissions, na.rm = TRUE), .groups = "drop")
    }

    if (is.null(group_cols) || length(group_cols) == 0) {
      compute_group(data_tbl)
    } else {
      data_tbl |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        compute_group()
    }
  }
}

#' Aggregate emission reductions for AMS-II.H (Equation 4)
#'
#' Equation (4) of AMS-II.H determines emission reductions by subtracting
#' centralized system emissions and leakage from the baseline decentralized
#' emissions. This helper accepts pre-computed emissions components and applies
#' the subtraction consistently across grouped datasets.
#'
#' @param baseline_emissions Tibble containing baseline emissions with one row
#'   per group.
#' @param project_emissions Tibble containing total project emissions (central
#'   plus auxiliary) with one row per group.
#' @param leakage_emissions Tibble containing leakage emissions in tCO2e.
#' @param group_cols Optional character vector of grouping columns shared across
#'   the inputs.
#' @param output_col Name of the resulting emission reduction column. Defaults to
#'   `"emission_reductions_tco2e"`.
#' @return A tibble containing net emission reductions.
#' @examples
#' baseline <- tibble::tibble(line = c("A", "B"), baseline_emissions_tco2e = c(12, 9))
#' project <- tibble::tibble(line = c("A", "B"), project_emissions_tco2e = c(6.4, 5.1))
#' leakage <- tibble::tibble(line = c("A", "B"), leakage_emissions_tco2e = c(0.2, 0.1))
#' calculate_emission_reductions_iih(baseline, project, leakage, group_cols = "line")
#' @export
calculate_emission_reductions_iih <- function(baseline_emissions,
                                              project_emissions,
                                              leakage_emissions = NULL,
                                              group_cols = NULL,
                                              output_col = "emission_reductions_tco2e") {
  base_tbl <- tibble::as_tibble(baseline_emissions)
  project_tbl <- tibble::as_tibble(project_emissions)
  leakage_tbl <- if (is.null(leakage_emissions)) {
    if (!is.null(group_cols) && length(group_cols) > 0) {
      project_tbl |>
        dplyr::select(dplyr::all_of(group_cols)) |>
        dplyr::distinct() |>
        dplyr::mutate(leakage_emissions_tco2e = 0)
    } else {
      tibble::tibble(leakage_emissions_tco2e = 0)
    }
  } else {
    tibble::as_tibble(leakage_emissions)
  }

  join_cols <- if (is.null(group_cols) || length(group_cols) == 0) character() else group_cols

  combined <- if (length(join_cols) == 0) {
    dplyr::bind_cols(base_tbl, project_tbl, leakage_tbl)
  } else {
    base_tbl |>
      dplyr::left_join(project_tbl, by = join_cols) |>
      dplyr::left_join(leakage_tbl, by = join_cols)
  }

  output_sym <- rlang::ensym(output_col)
  baseline_sym <- rlang::sym("baseline_emissions_tco2e")
  project_sym <- rlang::sym("project_emissions_tco2e")
  leakage_sym <- rlang::sym("leakage_emissions_tco2e")

  combined |>
    dplyr::mutate(
      !!output_sym := dplyr::coalesce(!!baseline_sym, 0) - dplyr::coalesce(!!project_sym, 0) - dplyr::coalesce(!!leakage_sym, 0)
    )
}
