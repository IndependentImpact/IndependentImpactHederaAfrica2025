#' Check feedstock management for AMS-III.F applicability
#'
#' AMS-III.F requires that incoming waste streams are predominantly organic,
#' source segregated, and appropriately conditioned for aerobic composting. This
#' helper verifies feedstock eligibility, segregation practices, and contamination
#' rates for each grouping combination.
#'
#' @param data Tibble containing feedstock metadata.
#' @param waste_type_col Column storing the waste type or category.
#' @param eligible_waste Character vector listing eligible waste categories.
#' @param source_segregated_col Column storing logical indicators confirming that
#'   waste is source segregated.
#' @param contamination_rate_col Column storing the contamination fraction
#'   (non-organic share) of the incoming waste.
#' @param maximum_contamination Maximum allowable contamination fraction.
#'   Defaults to 0.1 (10%).
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether each group satisfies feedstock management
#'   requirements.
#' @examples
#' metadata <- tibble::tibble(
#'   site_id = c("A", "B"),
#'   waste_type = c("municipal organics", "mixed waste"),
#'   source_segregated = c(TRUE, FALSE),
#'   contamination_fraction = c(0.08, 0.25)
#' )
#' check_applicability_feedstock_management_iiif(metadata, group_cols = "site_id")
#' @export
check_applicability_feedstock_management_iiif <- function(data,
                                                          waste_type_col = "waste_type",
                                                          eligible_waste = c(
                                                            "municipal organics",
                                                            "agricultural residues",
                                                            "market waste",
                                                            "food processing waste"
                                                          ),
                                                          source_segregated_col = "source_segregated",
                                                          contamination_rate_col = "contamination_fraction",
                                                          maximum_contamination = 0.1,
                                                          group_cols = NULL,
                                                          output_col = "feedstock_applicable") {
  data_tbl <- tibble::as_tibble(data)
  waste_type_col <- if (rlang::is_string(waste_type_col)) waste_type_col else rlang::as_name(rlang::enquo(waste_type_col))
  source_segregated_col <- if (rlang::is_string(source_segregated_col)) source_segregated_col else rlang::as_name(rlang::enquo(source_segregated_col))
  contamination_rate_col <- if (rlang::is_string(contamination_rate_col)) contamination_rate_col else rlang::as_name(rlang::enquo(contamination_rate_col))
  groups <- if (is.null(group_cols)) character() else group_cols
  eligible_waste_clean <- tolower(trimws(eligible_waste))

  assessment <- data_tbl |>
    dplyr::mutate(
      .waste_applicable = tolower(trimws(.data[[waste_type_col]])) %in% eligible_waste_clean,
      .segregation_applicable = .data[[source_segregated_col]] %in% TRUE,
      .contamination_applicable = .data[[contamination_rate_col]] <= maximum_contamination,
      .applicable = .waste_applicable & .segregation_applicable & .contamination_applicable
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

#' Check composting system practices for AMS-III.F
#'
#' The methodology requires controlled aerobic conditions with documented
#' retention times and leachate management. This helper evaluates whether
#' composting facilities operate compliant systems.
#'
#' @param data Tibble containing composting system metadata.
#' @param aeration_method_col Column storing the aeration method description.
#' @param compliant_aeration_methods Character vector of aeration methods that
#'   satisfy AMS-III.F (e.g. forced aeration, regular windrow turning).
#' @param retention_time_col Column storing the minimum residence time (days).
#' @param minimum_retention_days Minimum residence time required in the compost
#'   reactor. Defaults to 42 days.
#' @param leachate_management_col Column storing logical indicators confirming
#'   leachate is collected and treated aerobically.
#' @param curing_phase_col Optional column storing logical indicators confirming
#'   a curing phase is implemented. Set to `NULL` to ignore.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether composting practices satisfy AMS-III.F.
#' @examples
#' systems <- tibble::tibble(
#'   site_id = c("A", "B"),
#'   aeration_method = c("forced aeration", "static pile"),
#'   retention_days = c(56, 30),
#'   leachate_managed = c(TRUE, TRUE)
#' )
#' check_applicability_composting_practices_iiif(systems, group_cols = "site_id")
#' @export
check_applicability_composting_practices_iiif <- function(data,
                                                          aeration_method_col = "aeration_method",
                                                          compliant_aeration_methods = c(
                                                            "forced aeration",
                                                            "turned windrow",
                                                            "in-vessel"
                                                          ),
                                                          retention_time_col = "retention_days",
                                                          minimum_retention_days = 42,
                                                          leachate_management_col = "leachate_managed",
                                                          curing_phase_col = "curing_phase",
                                                          group_cols = NULL,
                                                          output_col = "composting_practices_applicable") {
  data_tbl <- tibble::as_tibble(data)
  aeration_method_col <- if (rlang::is_string(aeration_method_col)) aeration_method_col else rlang::as_name(rlang::enquo(aeration_method_col))
  retention_time_col <- if (rlang::is_string(retention_time_col)) retention_time_col else rlang::as_name(rlang::enquo(retention_time_col))
  leachate_management_col <- if (rlang::is_string(leachate_management_col)) leachate_management_col else rlang::as_name(rlang::enquo(leachate_management_col))
  curing_phase_col <- if (is.null(curing_phase_col) || rlang::is_string(curing_phase_col)) curing_phase_col else rlang::as_name(rlang::enquo(curing_phase_col))
  groups <- if (is.null(group_cols)) character() else group_cols
  compliant_aeration_methods <- tolower(trimws(compliant_aeration_methods))

  assessment <- data_tbl |>
    dplyr::mutate(
      .aeration_applicable = tolower(trimws(.data[[aeration_method_col]])) %in% compliant_aeration_methods,
      .retention_applicable = .data[[retention_time_col]] >= minimum_retention_days,
      .leachate_applicable = .data[[leachate_management_col]] %in% TRUE,
      .curing_applicable = if (!is.null(curing_phase_col)) .data[[curing_phase_col]] %in% TRUE else TRUE,
      .applicable = .aeration_applicable & .retention_applicable & .leachate_applicable & .curing_applicable
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

#' Check monitoring framework for AMS-III.F
#'
#' AMS-III.F requires monitoring of organic content, compost pile temperature,
#' and moisture to demonstrate aerobic conditions. This helper validates that the
#' monitoring plan includes the minimum number of samples or measurements for
#' each parameter.
#'
#' @param data Tibble containing monitoring framework metadata.
#' @param organic_sampling_col Column storing the number of DOC or volatile solids
#'   samples analysed per year.
#' @param temperature_monitoring_col Column storing the number of temperature
#'   measurements per week.
#' @param moisture_monitoring_col Column storing the number of moisture
#'   measurements per week.
#' @param minimum_organic_samples Minimum acceptable number of organic content
#'   samples per year. Defaults to 12.
#' @param minimum_temperature_measurements Minimum temperature readings per week.
#'   Defaults to 3.
#' @param minimum_moisture_measurements Minimum moisture measurements per week.
#'   Defaults to 2.
#' @param group_cols Optional character vector specifying grouping columns.
#' @param output_col Name of the logical output column summarising compliance.
#' @return Tibble indicating whether monitoring plans satisfy AMS-III.F.
#' @examples
#' monitoring <- tibble::tibble(
#'   site_id = c("A", "B"),
#'   organic_samples_per_year = c(16, 8),
#'   temperature_checks_per_week = c(4, 2),
#'   moisture_checks_per_week = c(3, 1)
#' )
#' check_applicability_monitoring_framework_iiif(monitoring, group_cols = "site_id")
#' @export
check_applicability_monitoring_framework_iiif <- function(data,
                                                          organic_sampling_col = "organic_samples_per_year",
                                                          temperature_monitoring_col = "temperature_checks_per_week",
                                                          moisture_monitoring_col = "moisture_checks_per_week",
                                                          minimum_organic_samples = 12,
                                                          minimum_temperature_measurements = 3,
                                                          minimum_moisture_measurements = 2,
                                                          group_cols = NULL,
                                                          output_col = "monitoring_framework_applicable") {
  data_tbl <- tibble::as_tibble(data)
  organic_sampling_col <- if (rlang::is_string(organic_sampling_col)) organic_sampling_col else rlang::as_name(rlang::enquo(organic_sampling_col))
  temperature_monitoring_col <- if (rlang::is_string(temperature_monitoring_col)) temperature_monitoring_col else rlang::as_name(rlang::enquo(temperature_monitoring_col))
  moisture_monitoring_col <- if (rlang::is_string(moisture_monitoring_col)) moisture_monitoring_col else rlang::as_name(rlang::enquo(moisture_monitoring_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  assessment <- data_tbl |>
    dplyr::mutate(
      .organic = .data[[organic_sampling_col]] >= minimum_organic_samples,
      .temperature = .data[[temperature_monitoring_col]] >= minimum_temperature_measurements,
      .moisture = .data[[moisture_monitoring_col]] >= minimum_moisture_measurements,
      .applicable = .organic & .temperature & .moisture
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
