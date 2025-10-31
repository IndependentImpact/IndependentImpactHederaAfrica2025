#' Check feedstock characteristics for AMS-III.E applicability
#'
#' AMS-III.E limits eligible biomass to residues that would otherwise decay under
#' anaerobic conditions. This helper verifies that feedstock types are eligible
#' and moisture content remains within acceptable bounds.
#'
#' @param data Tibble containing feedstock metadata.
#' @param feedstock_type_col Column storing feedstock types.
#' @param eligible_feedstock Character vector listing eligible feedstock types.
#' @param moisture_content_col Column storing moisture content as a fraction.
#' @param maximum_moisture Maximum allowed moisture fraction. Defaults to 0.6.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising both criteria.
#' @return Tibble indicating whether each group satisfies feedstock
#'   applicability requirements.
#' @examples
#' metadata <- tibble::tibble(
#'   plant_id = c("A", "B"),
#'   feedstock_type = c("agricultural residues", "municipal waste"),
#'   moisture_fraction = c(0.45, 0.75)
#' )
#' check_applicability_feedstock_characteristics_iiie(metadata, group_cols = "plant_id")
#' @export
check_applicability_feedstock_characteristics_iiie <- function(data,
                                                               feedstock_type_col = "feedstock_type",
                                                               eligible_feedstock = c(
                                                                 "agricultural residues",
                                                                 "forest residues",
                                                                 "industrial biomass"
                                                               ),
                                                               moisture_content_col = "moisture_fraction",
                                                               maximum_moisture = 0.6,
                                                               group_cols = NULL,
                                                               output_col = "feedstock_applicable") {
  data_tbl <- tibble::as_tibble(data)
  feedstock_type_col <- if (rlang::is_string(feedstock_type_col)) feedstock_type_col else rlang::as_name(rlang::enquo(feedstock_type_col))
  moisture_content_col <- if (rlang::is_string(moisture_content_col)) moisture_content_col else rlang::as_name(rlang::enquo(moisture_content_col))
  groups <- if (is.null(group_cols)) character() else group_cols
  eligible_feedstock_clean <- tolower(trimws(eligible_feedstock))

  assessment <- data_tbl |>
    dplyr::mutate(
      .type_applicable = tolower(trimws(.data[[feedstock_type_col]])) %in% eligible_feedstock_clean,
      .moisture_applicable = .data[[moisture_content_col]] <= maximum_moisture,
      .applicable = .type_applicable & .moisture_applicable
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

#' Check that biomass control and additionality evidence is documented
#'
#' Projects must demonstrate that the biomass would have decayed anaerobically in
#' the absence of the project activity and that diversion to the treatment system
#' is controlled. This helper verifies the presence of control and additionality
#' indicators for each group.
#'
#' @param data Tibble containing applicability evidence.
#' @param anaerobic_baseline_col Column storing logical indicators confirming the
#'   biomass would have undergone anaerobic decay.
#' @param control_plan_col Column storing logical indicators that biomass
#'   collection and delivery are controlled.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising the criterion.
#' @return Tibble indicating whether required controls are documented.
#' @examples
#' applicability <- tibble::tibble(
#'   plant_id = c("A", "B"),
#'   anaerobic_baseline = c(TRUE, TRUE),
#'   biomass_control_plan = c(TRUE, FALSE)
#' )
#' check_applicability_biomass_control_iiie(applicability, group_cols = "plant_id")
#' @export
check_applicability_biomass_control_iiie <- function(data,
                                                     anaerobic_baseline_col = "anaerobic_baseline",
                                                     control_plan_col = "biomass_control_plan",
                                                     group_cols = NULL,
                                                     output_col = "biomass_control_applicable") {
  data_tbl <- tibble::as_tibble(data)
  anaerobic_baseline_col <- if (rlang::is_string(anaerobic_baseline_col)) anaerobic_baseline_col else rlang::as_name(rlang::enquo(anaerobic_baseline_col))
  control_plan_col <- if (rlang::is_string(control_plan_col)) control_plan_col else rlang::as_name(rlang::enquo(control_plan_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(
      .applicable = dplyr::if_else(
        is.na(.data[[anaerobic_baseline_col]]) | is.na(.data[[control_plan_col]]),
        FALSE,
        .data[[anaerobic_baseline_col]] & .data[[control_plan_col]]
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

#' Check monitoring practices for AMS-III.E
#'
#' AMS-III.E requires monitoring of energy output, operating hours, and feedstock
#' characteristics. This helper ensures datasets record the minimum number of
#' monitoring events covering these parameters.
#'
#' @param data Tibble containing monitoring plan metadata.
#' @param energy_measurements_col Column storing counts of energy output
#'   measurements per month.
#' @param operating_hours_col Column storing recorded operating hours per period.
#' @param feedstock_sampling_col Column storing counts of feedstock sampling
#'   events per month.
#' @param minimum_energy_measurements Minimum acceptable number of energy
#'   measurements per month. Defaults to 4.
#' @param minimum_operating_hours Minimum required operating hours per period.
#'   Defaults to 400.
#' @param minimum_feedstock_samples Minimum acceptable number of feedstock
#'   samples per month. Defaults to 2.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising monitoring
#'   compliance.
#' @return Tibble indicating whether monitoring requirements are satisfied.
#' @examples
#' monitoring <- tibble::tibble(
#'   plant_id = c("A", "B"),
#'   energy_measurements_per_month = c(6, 3),
#'   operating_hours_per_period = c(450, 320),
#'   feedstock_samples_per_month = c(3, 1)
#' )
#' check_applicability_monitoring_practices_iiie(monitoring, group_cols = "plant_id")
#' @export
check_applicability_monitoring_practices_iiie <- function(data,
                                                          energy_measurements_col = "energy_measurements_per_month",
                                                          operating_hours_col = "operating_hours_per_period",
                                                          feedstock_sampling_col = "feedstock_samples_per_month",
                                                          minimum_energy_measurements = 4,
                                                          minimum_operating_hours = 400,
                                                          minimum_feedstock_samples = 2,
                                                          group_cols = NULL,
                                                          output_col = "monitoring_practices_applicable") {
  data_tbl <- tibble::as_tibble(data)
  energy_measurements_col <- if (rlang::is_string(energy_measurements_col)) energy_measurements_col else rlang::as_name(rlang::enquo(energy_measurements_col))
  operating_hours_col <- if (rlang::is_string(operating_hours_col)) operating_hours_col else rlang::as_name(rlang::enquo(operating_hours_col))
  feedstock_sampling_col <- if (rlang::is_string(feedstock_sampling_col)) feedstock_sampling_col else rlang::as_name(rlang::enquo(feedstock_sampling_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(
      .energy = .data[[energy_measurements_col]] >= minimum_energy_measurements,
      .hours = .data[[operating_hours_col]] >= minimum_operating_hours,
      .samples = .data[[feedstock_sampling_col]] >= minimum_feedstock_samples,
      .applicable = .energy & .hours & .samples
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
