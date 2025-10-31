#' Check that legumes dominate the cropping pattern
#'
#' The AMS-III.A methodology requires that the project activity replaces
#' nitrogen fertilizer use in leguminous crops. This helper verifies that the
#' monitored legume area share exceeds a minimum threshold. Provide a dataset
#' with one row per farm or management unit and columns storing the total cropped
#' area and the portion planted with inoculated legumes.
#'
#' @param data Tibble containing monitoring records for farms or plots.
#' @param legume_area_col Column storing the area planted with legumes (hectares).
#' @param total_area_col Column storing the total cropped area (hectares).
#' @param min_legume_share Minimum allowable legume area share. Defaults to 0.2,
#'   reflecting that the methodology targets systems where legumes are a
#'   significant share of production.
#' @param group_cols Optional character vector of grouping columns that identify
#'   management units.
#' @return Tibble with the grouping columns and a logical column named
#'   `legume_share_applicable` indicating whether the requirement is satisfied.
#' @examples
#' library(tibble)
#' fields <- tibble(
#'   farm_id = c("A", "B"),
#'   legume_area_ha = c(24, 6),
#'   total_area_ha = c(30, 40)
#' )
#' check_applicability_legume_share_iiia(fields, "legume_area_ha", "total_area_ha")
#' @export
check_applicability_legume_share_iiia <- function(data,
                                                 legume_area_col = "legume_area_ha",
                                                 total_area_col = "total_area_ha",
                                                 min_legume_share = 0.2,
                                                 group_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  legume_area_col <- if (rlang::is_string(legume_area_col)) legume_area_col else rlang::as_name(rlang::enquo(legume_area_col))
  total_area_col <- if (rlang::is_string(total_area_col)) total_area_col else rlang::as_name(rlang::enquo(total_area_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  result <- data_tbl |>
    dplyr::mutate(
      legume_share = .data[[legume_area_col]] / .data[[total_area_col]],
      legume_share_applicable = legume_share >= min_legume_share
    ) |>
    dplyr::select(dplyr::all_of(c(groups, "legume_share", "legume_share_applicable")))

  if (length(groups) == 0) {
    result
  } else {
    result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        legume_share = mean(.data$legume_share, na.rm = TRUE),
        legume_share_applicable = all(.data$legume_share_applicable),
        .groups = "drop"
      )
  }
}

#' Confirm inoculant registration and quality control
#'
#' AMS-III.A requires that the biological inoculants applied to legumes are
#' registered with the relevant agricultural authority and undergo quality
#' control. This helper checks a logical column indicating compliance.
#'
#' @param data Tibble containing project monitoring records.
#' @param registration_col Column storing logical values signalling whether the
#'   inoculant batch meets regulatory requirements.
#' @param group_cols Optional character vector of grouping columns that identify
#'   management units.
#' @return Tibble with grouping columns and `inoculant_registration_applicable`
#'   indicating whether all observations satisfy the requirement.
#' @examples
#' records <- tibble::tibble(
#'   farm_id = c("A", "A", "B"),
#'   monitoring_period = c(1, 2, 1),
#'   registered = c(TRUE, TRUE, FALSE)
#' )
#' check_applicability_inoculant_registration_iiia(records, "registered", "farm_id")
#' @export
check_applicability_inoculant_registration_iiia <- function(data,
                                                            registration_col = "inoculant_registered",
                                                            group_cols = NULL) {
  data_tbl <- tibble::as_tibble(data)
  registration_col <- if (rlang::is_string(registration_col)) registration_col else rlang::as_name(rlang::enquo(registration_col))
  groups <- if (is.null(group_cols)) character() else group_cols

  status <- data_tbl |>
    dplyr::mutate(inoculant_registration_applicable = .data[[registration_col]] %in% TRUE) |>
    dplyr::select(dplyr::all_of(c(groups, "inoculant_registration_applicable")))

  if (length(groups) == 0) {
    tibble::tibble(inoculant_registration_applicable = all(status$inoculant_registration_applicable))
  } else {
    status |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        inoculant_registration_applicable = all(.data$inoculant_registration_applicable),
        .groups = "drop"
      )
  }
}

#' Validate monitoring practices for nutrient management
#'
#' The methodology requires at least three monitoring observations (e.g. seasons
#' or cropping cycles) documenting fertilizer displacement. This helper confirms
#' that each management unit meets the minimum sample size.
#'
#' @param data Tibble containing monitoring observations.
#' @param monitoring_unit_cols Character vector identifying the monitoring unit
#'   (e.g. farm and season identifiers).
#' @param min_periods Minimum number of monitoring periods required. Defaults to 3.
#' @return Tibble containing one row per management unit with a logical column
#'   `monitoring_practices_applicable` indicating compliance.
#' @examples
#' monitoring <- tibble::tibble(
#'   farm_id = c("A", "A", "A", "B"),
#'   season = c(1, 2, 3, 1)
#' )
#' check_applicability_monitoring_practices_iiia(
#'   monitoring,
#'   monitoring_unit_cols = c("farm_id")
#' )
#' @export
check_applicability_monitoring_practices_iiia <- function(data,
                                                          monitoring_unit_cols,
                                                          min_periods = 3) {
  data_tbl <- tibble::as_tibble(data)
  if (missing(monitoring_unit_cols) || length(monitoring_unit_cols) == 0) {
    stop("`monitoring_unit_cols` must identify at least one grouping column.")
  }

  data_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(monitoring_unit_cols))) |>
    dplyr::summarise(
      monitoring_periods_observed = dplyr::n(),
      monitoring_practices_applicable = monitoring_periods_observed >= min_periods,
      .groups = "drop"
    )
}
