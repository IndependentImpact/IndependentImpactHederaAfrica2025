#' Check recycled material quality applicability conditions
#'
#' Verifies that recycled material streams meet AMS-III.AJ quality
#' requirements, including acceptable material categories, contamination
#' thresholds, and compliance with downstream market specifications.
#'
#' @param data Tibble containing material quality monitoring data.
#' @param material_type_col Column storing material categories. Defaults to
#'   `"material_type"`.
#' @param contamination_rate_col Column storing contamination rates (0-1).
#'   Defaults to `"contamination_rate"`.
#' @param specification_met_col Column indicating whether quality standards are
#'   met. Defaults to `"specification_met"`.
#' @param max_contamination Maximum allowable contamination fraction. Defaults to
#'   0.1.
#' @param allowed_materials Character vector of admissible material types.
#'   Defaults to c("paper", "cardboard", "plastics", "metals", "glass").
#' @param group_cols Optional character vector with grouping columns.
#' @return Tibble summarising compliance with material quality conditions.
#' @examples
#' quality <- tibble::tibble(
#'   facility_id = c("F1", "F1", "F2"),
#'   material_type = c("paper", "plastics", "glass"),
#'   contamination_rate = c(0.05, 0.08, 0.12),
#'   specification_met = c(TRUE, TRUE, TRUE)
#' )
#' check_applicability_material_quality_iiiaj(quality, group_cols = "facility_id")
#' @export
check_applicability_material_quality_iiiaj <- function(data,
                                                       material_type_col = "material_type",
                                                       contamination_rate_col = "contamination_rate",
                                                       specification_met_col = "specification_met",
                                                       max_contamination = 0.1,
                                                       allowed_materials = c("paper", "cardboard", "plastics", "metals", "glass"),
                                                       group_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  material_type_col <- resolve_column(material_type_col)
  contamination_rate_col <- resolve_column(contamination_rate_col)
  specification_met_col <- resolve_column(specification_met_col)
  groups <- ensure_group_cols(group_cols)

  assessments <- data_tbl |>
    dplyr::mutate(
      .material_allowed = .data[[material_type_col]] %in% allowed_materials,
      .contamination_compliant = .data[[contamination_rate_col]] <= max_contamination,
      .specification_compliant = .data[[specification_met_col]]
    )

  summarise_applicability(assessments, groups, c(
    "material_types_compliant" = ".material_allowed",
    "contamination_compliant" = ".contamination_compliant",
    "specification_compliant" = ".specification_compliant"
  ))
}

#' Check collection network applicability conditions
#'
#' Evaluates whether the collection system satisfies AMS-III.AJ requirements,
#' including source segregation, coverage of the service area, and logistics
#' readiness for the recycling facility.
#'
#' @param data Tibble containing collection network data.
#' @param segregation_rate_col Column storing the proportion of households or
#'   generators separating recyclables. Defaults to `"segregation_rate"`.
#' @param coverage_rate_col Column storing the share of service area covered by
#'   the collection network. Defaults to `"coverage_rate"`.
#' @param logistics_score_col Column storing a readiness indicator (0-1).
#'   Defaults to `"logistics_score"`.
#' @param min_segregation Minimum acceptable segregation rate. Defaults to 0.6.
#' @param min_coverage Minimum acceptable service coverage. Defaults to 0.7.
#' @param min_logistics_score Minimum acceptable logistics readiness score.
#'   Defaults to 0.7.
#' @param group_cols Optional character vector with grouping columns.
#' @return Tibble summarising collection network compliance.
#' @examples
#' collection <- tibble::tibble(
#'   facility_id = c("F1", "F2"),
#'   segregation_rate = c(0.75, 0.55),
#'   coverage_rate = c(0.82, 0.78),
#'   logistics_score = c(0.8, 0.72)
#' )
#' check_applicability_collection_network_iiiaj(collection, group_cols = "facility_id")
#' @export
check_applicability_collection_network_iiiaj <- function(data,
                                                          segregation_rate_col = "segregation_rate",
                                                          coverage_rate_col = "coverage_rate",
                                                          logistics_score_col = "logistics_score",
                                                          min_segregation = 0.6,
                                                          min_coverage = 0.7,
                                                          min_logistics_score = 0.7,
                                                          group_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  segregation_rate_col <- resolve_column(segregation_rate_col)
  coverage_rate_col <- resolve_column(coverage_rate_col)
  logistics_score_col <- resolve_column(logistics_score_col)
  groups <- ensure_group_cols(group_cols)

  assessments <- data_tbl |>
    dplyr::mutate(
      .segregation_compliant = .data[[segregation_rate_col]] >= min_segregation,
      .coverage_compliant = .data[[coverage_rate_col]] >= min_coverage,
      .logistics_compliant = .data[[logistics_score_col]] >= min_logistics_score
    )

  summarise_applicability(assessments, groups, c(
    "segregation_compliant" = ".segregation_compliant",
    "coverage_compliant" = ".coverage_compliant",
    "logistics_compliant" = ".logistics_compliant"
  ))
}

#' Check monitoring plan applicability conditions
#'
#' Confirms that the monitoring plan records throughput, residual disposal, and
#' calibration activities required by AMS-III.AJ.
#'
#' @param data Tibble containing monitoring plan data.
#' @param throughput_monitoring_col Column indicating whether throughput is
#'   monitored. Defaults to `"throughput_monitoring"`.
#' @param residual_tracking_col Column indicating whether residual disposal is
#'   tracked. Defaults to `"residual_tracking"`.
#' @param calibration_events_col Column storing the number of calibration events
#'   conducted during the monitoring period. Defaults to
#'   `"calibration_events"`.
#' @param min_calibration_events Minimum number of calibration events per period.
#'   Defaults to 1.
#' @param group_cols Optional character vector with grouping columns.
#' @return Tibble summarising monitoring plan compliance.
#' @examples
#' monitoring <- tibble::tibble(
#'   facility_id = c("F1", "F2"),
#'   throughput_monitoring = c(TRUE, TRUE),
#'   residual_tracking = c(TRUE, FALSE),
#'   calibration_events = c(2, 0)
#' )
#' check_applicability_monitoring_plan_iiiaj(monitoring, group_cols = "facility_id")
#' @export
check_applicability_monitoring_plan_iiiaj <- function(data,
                                                       throughput_monitoring_col = "throughput_monitoring",
                                                       residual_tracking_col = "residual_tracking",
                                                       calibration_events_col = "calibration_events",
                                                       min_calibration_events = 1,
                                                       group_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  throughput_monitoring_col <- resolve_column(throughput_monitoring_col)
  residual_tracking_col <- resolve_column(residual_tracking_col)
  calibration_events_col <- resolve_column(calibration_events_col)
  groups <- ensure_group_cols(group_cols)

  assessments <- data_tbl |>
    dplyr::mutate(
      .throughput_compliant = .data[[throughput_monitoring_col]],
      .residual_compliant = .data[[residual_tracking_col]],
      .calibration_compliant = .data[[calibration_events_col]] >= min_calibration_events
    )

  summarise_applicability(assessments, groups, c(
    "throughput_compliant" = ".throughput_compliant",
    "residual_compliant" = ".residual_compliant",
    "calibration_compliant" = ".calibration_compliant"
  ))
}

summarise_applicability <- function(data, group_cols, mapping) {
  summaries <- purrr::imap(mapping, function(column, output_name) {
    column_sym <- rlang::sym(column)
    rlang::expr(all(.data[[!!column_sym]], na.rm = TRUE))
  })
  summaries <- rlang::set_names(summaries, names(mapping))

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(!!!summaries, .groups = "drop")
}
