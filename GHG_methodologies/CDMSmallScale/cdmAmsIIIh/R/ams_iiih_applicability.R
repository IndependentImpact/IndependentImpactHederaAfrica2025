#' Check wastewater characteristics for AMS-III.H applicability
#'
#' AMS-III.H targets anaerobic wastewater treatment systems where methane
#' recovery can displace uncontrolled emissions. This helper confirms that the
#' wastewater characteristics, baseline treatment configuration, and organic
#' loading support meaningful recovery potential.
#'
#' @param data Tibble containing wastewater facility metadata.
#' @param wastewater_type_col Column storing the wastewater classification
#'   (e.g. "industrial", "agro-industrial", "domestic").
#' @param eligible_types Character vector listing wastewater types that fall
#'   within the AMS-III.H scope.
#' @param cod_concentration_col Column storing chemical oxygen demand (COD)
#'   concentration in mg/L.
#' @param minimum_cod_mg_l Minimum COD concentration required for AMS-III.H
#'   applicability. Defaults to 2000 mg/L.
#' @param anaerobic_fraction_col Column storing the fraction of the wastewater
#'   stream routed to anaerobic treatment.
#' @param minimum_anaerobic_fraction Minimum anaerobic fraction required for
#'   applicability. Defaults to 0.5 (50%).
#' @param baseline_system_col Column storing a description of the baseline
#'   treatment system (e.g. open lagoon, uncovered reactor).
#' @param excluded_baseline_systems Character vector listing baseline systems
#'   that already capture or destroy methane (and therefore are not eligible).
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble summarising whether each grouping combination satisfies the
#'   wastewater characteristic requirements.
#' @examples
#' facilities <- tibble::tibble(
#'   site_id = c("WW1", "WW2"),
#'   wastewater_type = c("agro-industrial", "domestic"),
#'   cod_mg_l = c(3200, 1800),
#'   anaerobic_fraction = c(0.7, 0.3),
#'   baseline_system = c("open anaerobic lagoon", "covered anaerobic lagoon")
#' )
#' check_applicability_wastewater_characteristics_iiih(
#'   facilities,
#'   group_cols = "site_id"
#' )
#' @export
check_applicability_wastewater_characteristics_iiih <- function(data,
                                                                wastewater_type_col = "wastewater_type",
                                                                eligible_types = c(
                                                                  "industrial",
                                                                  "agro-industrial",
                                                                  "domestic"
                                                                ),
                                                                cod_concentration_col = "cod_mg_l",
                                                                minimum_cod_mg_l = 2000,
                                                                anaerobic_fraction_col = "anaerobic_fraction",
                                                                minimum_anaerobic_fraction = 0.5,
                                                                baseline_system_col = "baseline_system",
                                                                excluded_baseline_systems = c(
                                                                  "covered anaerobic lagoon",
                                                                  "closed digester",
                                                                  "anaerobic reactor with flare"
                                                                ),
                                                                group_cols = NULL,
                                                                output_col = "wastewater_applicable") {
  data_tbl <- tibble::as_tibble(data)
  wastewater_type_col <- resolve_column(wastewater_type_col)
  cod_concentration_col <- resolve_column(cod_concentration_col)
  anaerobic_fraction_col <- resolve_column(anaerobic_fraction_col)
  baseline_system_col <- resolve_column(baseline_system_col)
  groups <- if (is.null(group_cols)) character() else group_cols
  eligible_types <- tolower(trimws(eligible_types))
  excluded_baseline_systems <- tolower(trimws(excluded_baseline_systems))

  assessment <- data_tbl |>
    dplyr::mutate(
      .type_applicable = tolower(trimws(.data[[wastewater_type_col]])) %in% eligible_types,
      .cod_applicable = .data[[cod_concentration_col]] >= minimum_cod_mg_l,
      .anaerobic_applicable = .data[[anaerobic_fraction_col]] >= minimum_anaerobic_fraction,
      .baseline_applicable = !tolower(trimws(.data[[baseline_system_col]])) %in% excluded_baseline_systems,
      .applicable = .type_applicable & .cod_applicable & .anaerobic_applicable & .baseline_applicable
    )

  summarise_applicability(assessment, groups, output_col)
}

#' Check recovery system design for AMS-III.H applicability
#'
#' AMS-III.H projects must install dedicated methane capture, destruction, or
#' utilisation systems with robust operating schedules. This helper validates
#' the recovery system configuration, destruction technology, and minimum
#' operating hours for each facility.
#'
#' @param data Tibble containing recovery system metadata.
#' @param capture_system_col Column storing whether a gas-tight cover or digester
#'   is installed (logical or yes/no string).
#' @param destruction_technology_col Column storing the destruction or
#'   utilisation technology description.
#' @param eligible_technologies Character vector listing eligible technologies
#'   (e.g. flare, boiler, electricity generation).
#' @param operating_hours_col Column storing weekly operating hours for the
#'   destruction/utilisation equipment.
#' @param minimum_operating_hours Minimum weekly operating hours required for
#'   applicability. Defaults to 25 hours.
#' @param redundancy_col Optional column storing logical indicators confirming
#'   redundant destruction equipment or storage.
#' @param utilisation_documented_col Optional column storing logical indicators
#'   that utilisation routes (e.g. boiler firing) are contractually documented.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether recovery systems satisfy AMS-III.H
#'   requirements.
#' @examples
#' recovery <- tibble::tibble(
#'   site_id = c("WW1", "WW2"),
#'   gas_capture_installed = c(TRUE, FALSE),
#'   destruction_technology = c("enclosed flare", "flare"),
#'   operating_hours_per_week = c(45, 12)
#' )
#' check_applicability_recovery_system_iiih(recovery, group_cols = "site_id")
#' @export
check_applicability_recovery_system_iiih <- function(data,
                                                      capture_system_col = "gas_capture_installed",
                                                      destruction_technology_col = "destruction_technology",
                                                      eligible_technologies = c(
                                                        "enclosed flare",
                                                        "boiler firing",
                                                        "electricity generation",
                                                        "biogas upgrading"
                                                      ),
                                                      operating_hours_col = "operating_hours_per_week",
                                                      minimum_operating_hours = 25,
                                                      redundancy_col = "redundancy_installed",
                                                      utilisation_documented_col = "utilisation_documented",
                                                      group_cols = NULL,
                                                      output_col = "recovery_applicable") {
  data_tbl <- tibble::as_tibble(data)
  capture_system_col <- resolve_column(capture_system_col)
  destruction_technology_col <- resolve_column(destruction_technology_col)
  operating_hours_col <- resolve_column(operating_hours_col)
  redundancy_col <- resolve_optional_column(redundancy_col)
  utilisation_documented_col <- resolve_optional_column(utilisation_documented_col)
  groups <- if (is.null(group_cols)) character() else group_cols
  eligible_technologies <- tolower(trimws(eligible_technologies))

  assessment <- data_tbl |>
    dplyr::mutate(
      .capture_applicable = .data[[capture_system_col]] %in% TRUE |
        tolower(trimws(.data[[capture_system_col]])) %in% c("yes", "true"),
      .technology_applicable = tolower(trimws(.data[[destruction_technology_col]])) %in% eligible_technologies,
      .operating_applicable = .data[[operating_hours_col]] >= minimum_operating_hours,
      .redundancy_applicable = if (!is.null(redundancy_col)) .data[[redundancy_col]] %in% TRUE else TRUE,
      .utilisation_applicable = if (!is.null(utilisation_documented_col)) {
        .data[[utilisation_documented_col]] %in% TRUE |
          tolower(trimws(.data[[utilisation_documented_col]])) %in% c("yes", "true")
      } else {
        TRUE
      },
      .applicable = .capture_applicable &
        .technology_applicable &
        .operating_applicable &
        .redundancy_applicable &
        .utilisation_applicable
    )

  summarise_applicability(assessment, groups, output_col)
}

#' Check monitoring frameworks for AMS-III.H
#'
#' AMS-III.H requires projects to continuously monitor biogas flow, methane
#' concentration, and key wastewater parameters used in emission calculations.
#' This helper validates that monitoring frequencies and calibration intervals
#' satisfy methodological minimums.
#'
#' @param data Tibble containing monitoring framework metadata.
#' @param flow_monitoring_col Column storing the number of flow measurements per
#'   week.
#' @param methane_fraction_monitoring_col Column storing the number of methane
#'   fraction measurements per week.
#' @param cod_sampling_col Column storing the number of influent COD samples per
#'   month.
#' @param minimum_flow_measurements Minimum flow measurements per week. Defaults
#'   to 7 (daily with redundancy).
#' @param minimum_methane_measurements Minimum methane fraction measurements per
#'   week. Defaults to 3.
#' @param minimum_cod_samples Minimum influent COD samples per month. Defaults to
#'   4.
#' @param calibration_events_col Column storing the number of instrument
#'   calibrations per year.
#' @param minimum_calibrations Minimum calibration events per year. Defaults to 4.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether monitoring plans satisfy AMS-III.H.
#' @examples
#' monitoring <- tibble::tibble(
#'   site_id = c("WW1", "WW2"),
#'   flow_measurements_per_week = c(14, 4),
#'   methane_measurements_per_week = c(5, 1),
#'   cod_samples_per_month = c(6, 2),
#'   calibration_events_per_year = c(5, 2)
#' )
#' check_applicability_monitoring_framework_iiih(monitoring, group_cols = "site_id")
#' @export
check_applicability_monitoring_framework_iiih <- function(data,
                                                          flow_monitoring_col = "flow_measurements_per_week",
                                                          methane_fraction_monitoring_col = "methane_measurements_per_week",
                                                          cod_sampling_col = "cod_samples_per_month",
                                                          minimum_flow_measurements = 7,
                                                          minimum_methane_measurements = 3,
                                                          minimum_cod_samples = 4,
                                                          calibration_events_col = "calibration_events_per_year",
                                                          minimum_calibrations = 4,
                                                          group_cols = NULL,
                                                          output_col = "monitoring_applicable") {
  data_tbl <- tibble::as_tibble(data)
  flow_monitoring_col <- resolve_column(flow_monitoring_col)
  methane_fraction_monitoring_col <- resolve_column(methane_fraction_monitoring_col)
  cod_sampling_col <- resolve_column(cod_sampling_col)
  calibration_events_col <- resolve_column(calibration_events_col)
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(
      .flow_applicable = .data[[flow_monitoring_col]] >= minimum_flow_measurements,
      .methane_applicable = .data[[methane_fraction_monitoring_col]] >= minimum_methane_measurements,
      .cod_applicable = .data[[cod_sampling_col]] >= minimum_cod_samples,
      .calibration_applicable = .data[[calibration_events_col]] >= minimum_calibrations,
      .applicable = .flow_applicable & .methane_applicable & .cod_applicable & .calibration_applicable
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
