#' Check that manure management system types are eligible for AMS-III.D
#'
#' Projects must apply the methodology to anaerobic lagoons, uncovered digesters,
#' or other systems capable of generating methane that can be captured. This
#' helper evaluates whether each monitoring group reports an eligible system
#' type.
#'
#' @param data Tibble containing project metadata.
#' @param system_type_col Column storing system types (character).
#' @param allowed_types Character vector of eligible system types. Defaults to
#'   c("anaerobic lagoon", "uncovered digester", "liquid slurry store").
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column.
#' @return Tibble indicating whether each group meets the applicability criterion.
#' @examples
#' metadata <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   system_type = c("anaerobic lagoon", "solid compost")
#' )
#' check_applicability_system_type_iiid(metadata, group_cols = "farm_id")
#' @export
check_applicability_system_type_iiid <- function(data,
                                                system_type_col = "system_type",
                                                allowed_types = c("anaerobic lagoon", "uncovered digester", "liquid slurry store"),
                                                group_cols = NULL,
                                                output_col = "system_type_applicable") {
  data_tbl <- tibble::as_tibble(data)
  system_type_col <- if (rlang::is_string(system_type_col)) system_type_col else rlang::as_name(rlang::enquo(system_type_col))
  groups <- if (is.null(group_cols)) character() else group_cols
  allowed_types_clean <- tolower(trimws(allowed_types))

  assessment <- data_tbl |>
    dplyr::mutate(
      .applicable = tolower(trimws(.data[[system_type_col]])) %in% allowed_types_clean
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := all(assessment$.applicable))
  } else {
    assessment |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := all(.data$.applicable),
        .groups = "drop"
      )
  }
}

#' Check measurement frequency meets AMS-III.D requirements
#'
#' The methodology requires continuous or at least weekly monitoring of methane
#' recovery. This helper ensures the frequency recorded for each monitoring group
#' meets or exceeds the minimum threshold.
#'
#' @param data Tibble containing monitoring system data.
#' @param frequency_col Column storing measurement frequency per month.
#' @param minimum_frequency Minimum acceptable number of measurements per month.
#'   Defaults to 4 (weekly).
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column.
#' @return Tibble indicating whether each group satisfies the frequency
#'   requirement.
#' @examples
#' monitoring <- tibble::tibble(
#'   digester_id = c("D1", "D2"),
#'   measurements_per_month = c(8, 2)
#' )
#' check_applicability_measurement_frequency_iiid(monitoring, group_cols = "digester_id")
#' @export
check_applicability_measurement_frequency_iiid <- function(data,
                                                          frequency_col = "measurements_per_month",
                                                          minimum_frequency = 4,
                                                          group_cols = NULL,
                                                          output_col = "measurement_frequency_applicable") {
  data_tbl <- tibble::as_tibble(data)
  frequency_col <- if (rlang::is_string(frequency_col)) frequency_col else rlang::as_name(rlang::enquo(frequency_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(.applicable = .data[[frequency_col]] >= minimum_frequency)

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := all(assessment$.applicable))
  } else {
    assessment |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := all(.data$.applicable),
        .groups = "drop"
      )
  }
}

#' Check that methane leakage controls are in place
#'
#' AMS-III.D requires demonstration that methane capture and transport systems
#' prevent significant leakage. Use this helper to verify that monitoring groups
#' report positive control measures (e.g. condensate traps, maintenance logs).
#'
#' @param data Tibble containing leakage control records.
#' @param control_indicator_col Column storing logical indicators for leakage
#'   controls.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column.
#' @return Tibble indicating whether leakage controls are documented for each
#'   group.
#' @examples
#' controls <- tibble::tibble(
#'   farm_id = c("A", "B"),
#'   leakage_controls_in_place = c(TRUE, FALSE)
#' )
#' check_applicability_leakage_control_iiid(controls, group_cols = "farm_id")
#' @export
check_applicability_leakage_control_iiid <- function(data,
                                                    control_indicator_col = "leakage_controls_in_place",
                                                    group_cols = NULL,
                                                    output_col = "leakage_controls_applicable") {
  data_tbl <- tibble::as_tibble(data)
  control_indicator_col <- if (rlang::is_string(control_indicator_col)) control_indicator_col else rlang::as_name(rlang::enquo(control_indicator_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(
      .applicable = dplyr::if_else(
        is.na(.data[[control_indicator_col]]),
        FALSE,
        .data[[control_indicator_col]]
      )
    )

  if (length(groups) == 0) {
    tibble::tibble(!!output_col := all(assessment$.applicable))
  } else {
    assessment |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!output_col := all(.data$.applicable),
        .groups = "drop"
      )
  }
}
