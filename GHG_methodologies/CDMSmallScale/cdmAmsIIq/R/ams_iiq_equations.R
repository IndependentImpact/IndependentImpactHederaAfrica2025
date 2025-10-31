#' Calculate baseline building emissions (Equation 1)
#'
#' Equation (1) of AMS-II.Q estimates greenhouse gas emissions that would occur
#' in the absence of the energy efficiency or onsite supply project. Baseline
#' energy use from grid or fossil sources is multiplied by appropriate emission
#' factors. When building service output (e.g. conditioned floor area or thermal
#' delivery) is provided the helper also returns the baseline energy intensity to
#' support applicability and diagnostic checks.
#'
#' @param baseline_data A tibble describing counterfactual building operation.
#' @param energy_consumption_col Column storing baseline energy use (in MWh or GJ).
#' @param emission_factor_col Column storing baseline emission factors (tCO2e per
#'   unit of energy).
#' @param service_output_col Optional column storing the delivered service such as
#'   conditioned floor area or thermal output. When supplied the function also
#'   reports baseline energy intensity.
#' @param group_cols Optional character vector of grouping columns to retain.
#' @param output_col Name of the emissions column in the returned tibble.
#'   Defaults to `"baseline_emissions_tco2e"`.
#' @param intensity_col Name of the optional intensity column returned when
#'   `service_output_col` is supplied. Defaults to
#'   `"baseline_energy_intensity"`.
#' @return A tibble containing grouped baseline emissions in tCO2e and, when
#'   requested, the baseline energy intensity.
#' @examples
#' baseline <- tibble::tibble(
#'   building_id = c("A", "A", "B"),
#'   baseline_energy_use_mwh = c(420, 380, 610),
#'   baseline_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.61),
#'   baseline_service_output_mwh = c(400, 360, 560)
#' )
#' calculate_baseline_building_emissions_iiq(
#'   baseline,
#'   group_cols = "building_id"
#' )
#' @export
calculate_baseline_building_emissions_iiq <- function(baseline_data,
                                                     energy_consumption_col = "baseline_energy_use_mwh",
                                                     emission_factor_col = "baseline_emission_factor_tco2_per_mwh",
                                                     service_output_col = "baseline_service_output_mwh",
                                                     group_cols = NULL,
                                                     output_col = "baseline_emissions_tco2e",
                                                     intensity_col = "baseline_energy_intensity") {
  data_tbl <- tibble::as_tibble(baseline_data)
  energy_col <- if (rlang::is_string(energy_consumption_col)) energy_consumption_col else rlang::as_name(rlang::enquo(energy_consumption_col))
  factor_col <- if (rlang::is_string(emission_factor_col)) emission_factor_col else rlang::as_name(rlang::enquo(emission_factor_col))
  missing_cols <- setdiff(c(energy_col, factor_col), names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      "Specified energy use and emission factor columns must exist in the data frame: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  service_col <- if (is.null(service_output_col)) NULL else if (rlang::is_string(service_output_col)) service_output_col else rlang::as_name(rlang::enquo(service_output_col))
  if (!is.null(service_col)) {
    optional_missing <- setdiff(service_col, names(data_tbl))
    if (length(optional_missing) > 0) {
      stop(
        "Specified service output column must exist in the data frame: ",
        paste(optional_missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  intensity_name <- if (is.null(service_col)) NULL else if (rlang::is_string(intensity_col)) intensity_col else rlang::as_name(rlang::enquo(intensity_col))

  compute_group <- function(tbl) {
    summary <- tbl |>
      dplyr::mutate(
        .energy_use = dplyr::coalesce(.data[[energy_col]], 0),
        .emissions = .energy_use * dplyr::coalesce(.data[[factor_col]], 0)
      ) |>
      dplyr::summarise(
        .emissions_total = sum(.emissions, na.rm = TRUE),
        .energy_total = sum(.energy_use, na.rm = TRUE),
        .service_total = if (!is.null(service_col)) sum(dplyr::coalesce(.data[[service_col]], 0), na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    summary[[output_col]] <- summary$.emissions_total
    if (!is.null(service_col)) {
      summary[[intensity_name]] <- summary$.energy_total / summary$.service_total
      summary[[intensity_name]] <- ifelse(is.finite(summary[[intensity_name]]), summary[[intensity_name]], NA_real_)
    }

    summary |>
      dplyr::select(-dplyr::any_of(c(".emissions_total", ".energy_total", ".service_total")))
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    compute_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      compute_group()
  }
}

#' Calculate project building emissions (Equation 2)
#'
#' Equation (2) of AMS-II.Q converts post-retrofit energy consumption into
#' project emissions. The helper accepts grid or fossil energy use and optional
#' service output to provide project energy intensity that can be compared to the
#' baseline intensity.
#'
#' @param project_data A tibble describing monitored project operation.
#' @param energy_consumption_col Column storing project energy use.
#' @param emission_factor_col Column storing project emission factors (tCO2e per
#'   unit of energy).
#' @param service_output_col Optional column storing the delivered service (e.g.
#'   chilled water output or delivered lighting service) to compute project
#'   energy intensity.
#' @param group_cols Optional character vector of grouping variables to retain.
#' @param output_col Name of the emissions column in the returned tibble.
#'   Defaults to `"project_emissions_tco2e"`.
#' @param intensity_col Name of the optional intensity column returned when
#'   `service_output_col` is provided. Defaults to
#'   `"project_energy_intensity"`.
#' @return A tibble containing grouped project emissions and optional energy
#'   intensity values.
#' @examples
#' project <- tibble::tibble(
#'   building_id = c("A", "A", "B"),
#'   project_energy_use_mwh = c(320, 290, 430),
#'   project_emission_factor_tco2_per_mwh = c(0.58, 0.58, 0.57),
#'   project_service_output_mwh = c(400, 360, 560)
#' )
#' calculate_project_building_emissions_iiq(project, group_cols = "building_id")
#' @export
calculate_project_building_emissions_iiq <- function(project_data,
                                                    energy_consumption_col = "project_energy_use_mwh",
                                                    emission_factor_col = "project_emission_factor_tco2_per_mwh",
                                                    service_output_col = "project_service_output_mwh",
                                                    group_cols = NULL,
                                                    output_col = "project_emissions_tco2e",
                                                    intensity_col = "project_energy_intensity") {
  data_tbl <- tibble::as_tibble(project_data)
  energy_col <- if (rlang::is_string(energy_consumption_col)) energy_consumption_col else rlang::as_name(rlang::enquo(energy_consumption_col))
  factor_col <- if (rlang::is_string(emission_factor_col)) emission_factor_col else rlang::as_name(rlang::enquo(emission_factor_col))
  missing_cols <- setdiff(c(energy_col, factor_col), names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      "Specified energy use and emission factor columns must exist in the data frame: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  service_col <- if (is.null(service_output_col)) NULL else if (rlang::is_string(service_output_col)) service_output_col else rlang::as_name(rlang::enquo(service_output_col))
  if (!is.null(service_col)) {
    optional_missing <- setdiff(service_col, names(data_tbl))
    if (length(optional_missing) > 0) {
      stop(
        "Specified service output column must exist in the data frame: ",
        paste(optional_missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  intensity_name <- if (is.null(service_col)) NULL else if (rlang::is_string(intensity_col)) intensity_col else rlang::as_name(rlang::enquo(intensity_col))

  compute_group <- function(tbl) {
    summary <- tbl |>
      dplyr::mutate(
        .energy_use = dplyr::coalesce(.data[[energy_col]], 0),
        .emissions = .energy_use * dplyr::coalesce(.data[[factor_col]], 0)
      ) |>
      dplyr::summarise(
        .emissions_total = sum(.emissions, na.rm = TRUE),
        .energy_total = sum(.energy_use, na.rm = TRUE),
        .service_total = if (!is.null(service_col)) sum(dplyr::coalesce(.data[[service_col]], 0), na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    summary[[output_col]] <- summary$.emissions_total
    if (!is.null(service_col)) {
      summary[[intensity_name]] <- summary$.energy_total / summary$.service_total
      summary[[intensity_name]] <- ifelse(is.finite(summary[[intensity_name]]), summary[[intensity_name]], NA_real_)
    }

    summary |>
      dplyr::select(-dplyr::any_of(c(".emissions_total", ".energy_total", ".service_total")))
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    compute_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      compute_group()
  }
}

#' Calculate onsite energy supply emissions (Equation 3)
#'
#' Many AMS-II.Q projects blend efficiency retrofits with onsite energy supply.
#' Equation (3) converts onsite energy use—such as auxiliary boilers or
#' fossil-fired combined heat and power—into emissions that must be added to the
#' project totals. When onsite energy is not used, set `onsite_energy_col = NULL`
#' to obtain zero emissions.
#'
#' @param project_data A tibble describing project monitoring observations.
#' @param onsite_energy_col Column storing onsite energy use (GJ or MWh). Set to
#'   `NULL` when onsite energy is absent.
#' @param emission_factor_col Column storing onsite energy emission factors
#'   (tCO2e per unit of energy). Required when `onsite_energy_col` is supplied.
#' @param group_cols Optional character vector of grouping variables shared with
#'   other monitoring datasets.
#' @param output_col Name of the emissions column returned by the function.
#'   Defaults to `"project_onsite_emissions_tco2e"`.
#' @return Tibble with onsite emissions aggregated by the requested grouping.
#' @examples
#' monitoring <- tibble::tibble(
#'   building_id = c("A", "B"),
#'   project_onsite_energy_gj = c(80, 0),
#'   project_onsite_emission_factor_tco2_per_gj = c(0.055, 0.055)
#' )
#' calculate_project_onsite_energy_emissions_iiq(monitoring, group_cols = "building_id")
#' @export
calculate_project_onsite_energy_emissions_iiq <- function(project_data,
                                                          onsite_energy_col = "project_onsite_energy_gj",
                                                          emission_factor_col = "project_onsite_emission_factor_tco2_per_gj",
                                                          group_cols = NULL,
                                                          output_col = "project_onsite_emissions_tco2e") {
  if (is.null(onsite_energy_col)) {
    project_tbl <- tibble::as_tibble(project_data)
    if (is.null(group_cols) || length(group_cols) == 0) {
      result <- tibble::tibble(value = 0)
      names(result) <- output_col
      return(result)
    }
    result <- project_tbl |>
      dplyr::select(dplyr::all_of(group_cols)) |>
      dplyr::distinct()
    result[[output_col]] <- 0
    return(result)
  }

  data_tbl <- tibble::as_tibble(project_data)
  energy_col <- if (rlang::is_string(onsite_energy_col)) onsite_energy_col else rlang::as_name(rlang::enquo(onsite_energy_col))
  factor_col <- if (rlang::is_string(emission_factor_col)) emission_factor_col else rlang::as_name(rlang::enquo(emission_factor_col))
  missing_cols <- setdiff(c(energy_col, factor_col), names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      "Specified onsite energy and emission factor columns must exist in the data frame: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  compute_group <- function(tbl) {
    summary <- tbl |>
      dplyr::mutate(
        .energy_use = dplyr::coalesce(.data[[energy_col]], 0),
        .emissions = .energy_use * dplyr::coalesce(.data[[factor_col]], 0)
      ) |>
      dplyr::summarise(
        .emissions_total = sum(.emissions, na.rm = TRUE),
        .groups = "drop"
      )
    summary[[output_col]] <- summary$.emissions_total
    dplyr::select(summary, -dplyr::any_of(".emissions_total"))
  }

  if (is.null(group_cols) || length(group_cols) == 0) {
    compute_group(data_tbl)
  } else {
    data_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      compute_group()
  }
}

#' Calculate emission reductions (Equation 4)
#'
#' Equation (4) of AMS-II.Q subtracts monitored project and leakage emissions from
#' the baseline emissions to derive net emission reductions.
#'
#' @param baseline_emissions Tibble containing baseline emissions in tCO2e.
#' @param project_emissions Tibble containing project emissions in tCO2e.
#' @param leakage_emissions Optional tibble containing leakage emissions in tCO2e.
#'   When omitted leakage is assumed to be zero.
#' @param group_cols Optional character vector of grouping columns shared across
#'   the inputs.
#' @param output_col Name of the emission reduction column to return.
#' @return Tibble with emission reductions aggregated by the requested grouping.
#' @examples
#' baseline <- tibble::tibble(building_id = "A", baseline_emissions_tco2e = 12)
#' project <- tibble::tibble(building_id = "A", project_emissions_tco2e = 6)
#' leakage <- tibble::tibble(building_id = "A", leakage_emissions_tco2e = 0.2)
#' calculate_emission_reductions_iiq(baseline, project, leakage, group_cols = "building_id")
#' @export
calculate_emission_reductions_iiq <- function(baseline_emissions,
                                             project_emissions,
                                             leakage_emissions = NULL,
                                             group_cols = NULL,
                                             output_col = "emission_reductions_tco2e") {
  baseline_tbl <- tibble::as_tibble(baseline_emissions)
  project_tbl <- tibble::as_tibble(project_emissions)
  leakage_tbl <- if (is.null(leakage_emissions)) {
    if (is.null(group_cols) || length(group_cols) == 0) {
      tibble::tibble(leakage_emissions_tco2e = 0)
    } else {
      baseline_tbl |>
        dplyr::select(dplyr::all_of(group_cols)) |>
        dplyr::distinct() |>
        dplyr::mutate(leakage_emissions_tco2e = 0)
    }
  } else {
    tibble::as_tibble(leakage_emissions)
  }

  baseline_col <- "baseline_emissions_tco2e"
  project_col <- "project_emissions_tco2e"
  leakage_col <- "leakage_emissions_tco2e"

  if (!baseline_col %in% names(baseline_tbl)) {
    stop("Baseline tibble must contain baseline_emissions_tco2e", call. = FALSE)
  }
  if (!project_col %in% names(project_tbl)) {
    stop("Project tibble must contain project_emissions_tco2e", call. = FALSE)
  }
  if (!leakage_col %in% names(leakage_tbl)) {
    stop("Leakage tibble must contain leakage_emissions_tco2e", call. = FALSE)
  }

  join_cols <- if (is.null(group_cols) || length(group_cols) == 0) character() else group_cols

  combined <- if (length(join_cols) == 0) {
    dplyr::bind_cols(baseline_tbl, project_tbl, leakage_tbl)
  } else {
    baseline_tbl |>
      dplyr::left_join(project_tbl, by = join_cols) |>
      dplyr::left_join(leakage_tbl, by = join_cols)
  }

  result <- combined |>
    dplyr::mutate(
      .reduction = dplyr::coalesce(.data[[baseline_col]], 0) -
        dplyr::coalesce(.data[[project_col]], 0) -
        dplyr::coalesce(.data[[leakage_col]], 0)
    )
  result[[output_col]] <- result$.reduction
  dplyr::select(result, dplyr::all_of(c(join_cols, output_col)))
}
