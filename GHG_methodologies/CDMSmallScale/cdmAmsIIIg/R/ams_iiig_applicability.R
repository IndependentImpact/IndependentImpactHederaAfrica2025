#' Check landfill characteristics for AMS-III.G applicability
#'
#' AMS-III.G applies to landfills with unmanaged methane emissions where gas
#' recovery can be implemented. This helper verifies landfill status, waste
#' composition, and readiness for gas capture based on monitoring metadata.
#'
#' @param data Tibble containing landfill metadata.
#' @param landfill_status_col Column storing the landfill management status
#'   (e.g. "open dump", "controlled landfill").
#' @param eligible_statuses Character vector listing landfill statuses that meet
#'   AMS-III.G requirements.
#' @param biodegradable_fraction_col Column storing the biodegradable fraction of
#'   incoming waste.
#' @param minimum_biodegradable_fraction Minimum biodegradable fraction required
#'   for AMS-III.G applicability. Defaults to 0.4 (40%).
#' @param gas_collection_ready_col Column storing logical indicators confirming
#'   that gas wells or trenches can be installed without interfering with waste
#'   disposal operations.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether each grouping combination satisfies landfill
#'   characteristic requirements.
#' @examples
#' metadata <- tibble::tibble(
#'   site_id = c("LF1", "LF2"),
#'   landfill_status = c("open dump", "sanitary landfill"),
#'   biodegradable_fraction = c(0.55, 0.32),
#'   gas_collection_ready = c(TRUE, FALSE)
#' )
#' check_applicability_landfill_characteristics_iiig(metadata, group_cols = "site_id")
#' @export
check_applicability_landfill_characteristics_iiig <- function(data,
                                                             landfill_status_col = "landfill_status",
                                                             eligible_statuses = c(
                                                               "open dump",
                                                               "controlled landfill",
                                                               "sanitary landfill"
                                                             ),
                                                             biodegradable_fraction_col = "biodegradable_fraction",
                                                             minimum_biodegradable_fraction = 0.4,
                                                             gas_collection_ready_col = "gas_collection_ready",
                                                             group_cols = NULL,
                                                             output_col = "landfill_applicable") {
  data_tbl <- tibble::as_tibble(data)
  landfill_status_col <- resolve_column(landfill_status_col)
  biodegradable_fraction_col <- resolve_column(biodegradable_fraction_col)
  gas_collection_ready_col <- resolve_column(gas_collection_ready_col)
  groups <- if (is.null(group_cols)) character() else group_cols
  eligible_statuses <- tolower(trimws(eligible_statuses))

  assessment <- data_tbl |>
    dplyr::mutate(
      .status_applicable = tolower(trimws(.data[[landfill_status_col]])) %in% eligible_statuses,
      .biodegradable_applicable = .data[[biodegradable_fraction_col]] >= minimum_biodegradable_fraction,
      .collection_ready = .data[[gas_collection_ready_col]] %in% TRUE,
      .applicable = .status_applicable & .biodegradable_applicable & .collection_ready
    )

  summarise_applicability(assessment, groups, output_col)
}

#' Check gas management practices for AMS-III.G applicability
#'
#' AMS-III.G requires that recovered landfill gas is routed to compliant
#' destruction or utilisation technologies with adequate monitoring of operating
#' hours. This helper verifies destruction technology, minimum operating hours,
#' and redundancy of key equipment.
#'
#' @param data Tibble containing gas management metadata.
#' @param destruction_technology_col Column storing the destruction or utilisation
#'   technology description.
#' @param eligible_technologies Character vector listing eligible destruction
#'   technologies (e.g. enclosed flare, electricity generation).
#' @param operating_hours_col Column storing weekly operating hours for the gas
#'   destruction system.
#' @param minimum_operating_hours Minimum weekly operating hours required for
#'   applicability. Defaults to 30 hours.
#' @param redundancy_col Optional column storing logical indicators confirming
#'   the presence of redundant blowers or flares.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether gas management practices satisfy AMS-III.G.
#' @examples
#' gas <- tibble::tibble(
#'   site_id = c("LF1", "LF2"),
#'   destruction_technology = c("enclosed flare", "open flare"),
#'   operating_hours_per_week = c(40, 12),
#'   redundancy_installed = c(TRUE, FALSE)
#' )
#' check_applicability_gas_management_iiig(gas, group_cols = "site_id")
#' @export
check_applicability_gas_management_iiig <- function(data,
                                                    destruction_technology_col = "destruction_technology",
                                                    eligible_technologies = c(
                                                      "enclosed flare",
                                                      "electricity generation",
                                                      "direct thermal use"
                                                    ),
                                                    operating_hours_col = "operating_hours_per_week",
                                                    minimum_operating_hours = 30,
                                                    redundancy_col = "redundancy_installed",
                                                    group_cols = NULL,
                                                    output_col = "gas_management_applicable") {
  data_tbl <- tibble::as_tibble(data)
  destruction_technology_col <- resolve_column(destruction_technology_col)
  operating_hours_col <- resolve_column(operating_hours_col)
  redundancy_col <- resolve_optional_column(redundancy_col)
  groups <- if (is.null(group_cols)) character() else group_cols
  eligible_technologies <- tolower(trimws(eligible_technologies))

  assessment <- data_tbl |>
    dplyr::mutate(
      .technology_applicable = tolower(trimws(.data[[destruction_technology_col]])) %in% eligible_technologies,
      .operating_applicable = .data[[operating_hours_col]] >= minimum_operating_hours,
      .redundancy_applicable = if (!is.null(redundancy_col)) .data[[redundancy_col]] %in% TRUE else TRUE,
      .applicable = .technology_applicable & .operating_applicable & .redundancy_applicable
    )

  summarise_applicability(assessment, groups, output_col)
}

#' Check monitoring framework for AMS-III.G
#'
#' The methodology requires routine monitoring of gas flow, methane fraction, and
#' key instrumentation calibrations. This helper confirms that monitoring plans
#' meet minimum sampling frequencies and calibration intervals.
#'
#' @param data Tibble containing monitoring framework metadata.
#' @param flow_monitoring_col Column storing the number of flow measurements per
#'   week.
#' @param methane_fraction_monitoring_col Column storing the number of methane
#'   fraction measurements per week.
#' @param calibration_events_col Column storing the number of instrument
#'   calibrations per year.
#' @param minimum_flow_measurements Minimum flow measurements per week. Defaults
#'   to 3.
#' @param minimum_methane_measurements Minimum methane fraction measurements per
#'   week. Defaults to 3.
#' @param minimum_calibrations Minimum calibration events per year. Defaults to 4.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether monitoring frameworks satisfy AMS-III.G.
#' @examples
#' monitoring <- tibble::tibble(
#'   site_id = c("LF1", "LF2"),
#'   flow_measurements_per_week = c(7, 2),
#'   methane_measurements_per_week = c(5, 1),
#'   calibration_events_per_year = c(6, 2)
#' )
#' check_applicability_monitoring_framework_iiig(monitoring, group_cols = "site_id")
#' @export
check_applicability_monitoring_framework_iiig <- function(data,
                                                          flow_monitoring_col = "flow_measurements_per_week",
                                                          methane_fraction_monitoring_col = "methane_measurements_per_week",
                                                          calibration_events_col = "calibration_events_per_year",
                                                          minimum_flow_measurements = 3,
                                                          minimum_methane_measurements = 3,
                                                          minimum_calibrations = 4,
                                                          group_cols = NULL,
                                                          output_col = "monitoring_applicable") {
  data_tbl <- tibble::as_tibble(data)
  flow_monitoring_col <- resolve_column(flow_monitoring_col)
  methane_fraction_monitoring_col <- resolve_column(methane_fraction_monitoring_col)
  calibration_events_col <- resolve_column(calibration_events_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(
      .flow_applicable = .data[[flow_monitoring_col]] >= minimum_flow_measurements,
      .methane_applicable = .data[[methane_fraction_monitoring_col]] >= minimum_methane_measurements,
      .calibration_applicable = .data[[calibration_events_col]] >= minimum_calibrations,
      .applicable = .flow_applicable & .methane_applicable & .calibration_applicable
    )

  summarise_applicability(assessment, groups, output_col)
}

summarise_applicability <- function(assessment, groups, output_col) {
  target <- rlang::sym(output_col)
  if (length(groups) == 0) {
    tibble::tibble(!!target := all(assessment$.applicable))
  } else {
    assessment |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        !!target := all(.data$.applicable),
        .groups = "drop"
      )
  }
}
