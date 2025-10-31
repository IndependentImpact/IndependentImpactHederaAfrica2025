#' Check AMS-II.G efficiency improvement criterion
#'
#' AMS-II.G requires that project activities deliver measurable reductions in
#' non-renewable biomass consumption. This helper compares weighted baseline and
#' project biomass (applying non-renewable fractions) and verifies that the
#' fractional reduction exceeds a specified tolerance.
#'
#' @param baseline_data Tibble containing baseline biomass consumption and
#'   non-renewable fractions.
#' @param project_data Tibble containing project biomass consumption and
#'   non-renewable fractions.
#' @param consumption_col Column storing baseline biomass consumption.
#' @param fraction_col Column storing the baseline non-renewable fraction.
#' @param project_consumption_col Column storing project biomass consumption.
#' @param project_fraction_col Column storing the project non-renewable fraction.
#' @param tolerance Minimum fractional reduction in non-renewable biomass
#'   consumption required for applicability (default 20%).
#' @return Logical scalar indicating whether the efficiency improvement criterion
#'   is satisfied.
#' @examples
#' baseline <- tibble::tibble(
#'   baseline_biomass_consumption_tonnes = c(12, 14),
#'   baseline_non_renewable_fraction = c(0.85, 0.88)
#' )
#' project <- tibble::tibble(
#'   project_biomass_consumption_tonnes = c(7.4, 8.1),
#'   project_non_renewable_fraction = c(0.4, 0.38)
#' )
#' check_applicability_efficiency_improvement_iig(baseline, project)
#' @export
check_applicability_efficiency_improvement_iig <- function(baseline_data,
                                                           project_data,
                                                           consumption_col = "baseline_biomass_consumption_tonnes",
                                                           fraction_col = "baseline_non_renewable_fraction",
                                                           project_consumption_col = "project_biomass_consumption_tonnes",
                                                           project_fraction_col = "project_non_renewable_fraction",
                                                           tolerance = 0.2) {
  baseline_tbl <- dplyr::as_tibble(baseline_data)
  project_tbl <- dplyr::as_tibble(project_data)

  required_baseline <- c(consumption_col, fraction_col)
  required_project <- c(project_consumption_col, project_fraction_col)

  if (!all(required_baseline %in% names(baseline_tbl))) {
    stop("Baseline data must contain consumption and fraction columns.", call. = FALSE)
  }
  if (!all(required_project %in% names(project_tbl))) {
    stop("Project data must contain consumption and fraction columns.", call. = FALSE)
  }

  baseline_nrb <- sum(
    dplyr::coalesce(baseline_tbl[[consumption_col]], 0) *
      dplyr::coalesce(baseline_tbl[[fraction_col]], 0),
    na.rm = TRUE
  )
  project_nrb <- sum(
    dplyr::coalesce(project_tbl[[project_consumption_col]], 0) *
      dplyr::coalesce(project_tbl[[project_fraction_col]], 0),
    na.rm = TRUE
  )

  if (baseline_nrb <= 0) {
    stop("Baseline non-renewable biomass must be positive to evaluate applicability.", call. = FALSE)
  }

  reduction_fraction <- (baseline_nrb - project_nrb) / baseline_nrb
  is.finite(reduction_fraction) && reduction_fraction >= tolerance
}

#' Check AMS-II.G non-renewable fraction bounds
#'
#' Ensures that reported non-renewable biomass fractions fall within the
#' methodological bounds of 0 to 1.
#'
#' @param data Tibble containing non-renewable fraction columns.
#' @param fraction_cols Character vector of column names storing non-renewable
#'   fractions.
#' @return Logical scalar indicating whether all fractions lie within [0, 1].
#' @examples
#' monitoring <- tibble::tibble(
#'   baseline_non_renewable_fraction = c(0.8, 0.85),
#'   project_non_renewable_fraction = c(0.4, 0.38)
#' )
#' check_applicability_fraction_bounds_iig(monitoring)
#' @export
check_applicability_fraction_bounds_iig <- function(data,
                                                    fraction_cols = c(
                                                      "baseline_non_renewable_fraction",
                                                      "project_non_renewable_fraction"
                                                    )) {
  data_tbl <- dplyr::as_tibble(data)

  missing_cols <- setdiff(fraction_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`data` is missing fraction columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  is_numeric <- vapply(data_tbl[fraction_cols], is.numeric, logical(1))
  if (any(!is_numeric)) {
    stop("Fraction columns must be numeric.", call. = FALSE)
  }

  all_vals <- unlist(data_tbl[fraction_cols], use.names = FALSE)
  all_vals <- all_vals[!is.na(all_vals)]
  all(abs(all_vals) <= 1 & all_vals >= 0)
}

#' Check AMS-II.G monitoring completeness
#'
#' Verifies that monitoring datasets include the required baseline and project
#' parameters without missing values, supporting the methodology's data quality
#' requirements.
#'
#' @param monitoring_data Tibble containing monitoring observations.
#' @param required_cols Character vector of required column names.
#' @return Logical scalar indicating whether all required columns are present and
#'   contain non-missing values.
#' @examples
#' monitoring <- tibble::tibble(
#'   site_id = "cookstove-1",
#'   baseline_biomass_consumption_tonnes = 12,
#'   project_biomass_consumption_tonnes = 7.5,
#'   baseline_non_renewable_fraction = 0.85,
#'   project_non_renewable_fraction = 0.4,
#'   baseline_net_calorific_value_mj_per_tonne = 15.2,
#'   project_net_calorific_value_mj_per_tonne = 15.4
#' )
#' check_applicability_monitoring_iig(monitoring)
#' @export
check_applicability_monitoring_iig <- function(monitoring_data,
                                               required_cols = c(
                                                 "baseline_biomass_consumption_tonnes",
                                                 "baseline_non_renewable_fraction",
                                                 "baseline_net_calorific_value_mj_per_tonne",
                                                 "baseline_emission_factor_tco2_per_mj",
                                                 "project_biomass_consumption_tonnes",
                                                 "project_non_renewable_fraction",
                                                 "project_net_calorific_value_mj_per_tonne",
                                                 "project_emission_factor_tco2_per_mj"
                                               )) {
  data_tbl <- dplyr::as_tibble(monitoring_data)

  missing_cols <- setdiff(required_cols, names(data_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "`monitoring_data` is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (col in required_cols) {
    if (any(is.na(data_tbl[[col]]))) {
      return(FALSE)
    }
  }
  TRUE
}
