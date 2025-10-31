#' Applicability helpers for ACM0019
#'
#' These helpers evaluate key applicability conditions for ACM0019 nitric acid
#' projects, covering monitoring coverage, catalyst configurations, and the
#' plant operating regime.
#'
#' @param data_capture_rate Numeric vector with the share of valid monitoring
#'   data captured during the period (between 0 and 1).
#' @param minimum_coverage Minimum acceptable data capture threshold. Defaults to
#'   0.9 (90%).
#' @param catalyst_configuration Character vector describing the installed
#'   abatement catalyst configuration (e.g. "secondary", "tertiary").
#' @param allowed_configurations Character vector of allowed configurations.
#'   Defaults to secondary and tertiary options.
#' @param plant_regime Character scalar describing the nitric acid process
#'   regime (e.g. "weak-acid" or "strong-acid").
#' @param has_continuous_monitoring Logical flag indicating whether certified
#'   continuous monitoring systems are operating.
#'
#' @return Logical vector indicating whether the applicability condition holds.
#' @examples
#' check_applicability_monitoring_coverage(c(0.92, 0.88))
#' check_applicability_catalyst_configuration(c("secondary", "tertiary"))
#' check_applicability_operating_regime("weak-acid", TRUE)
#' @name acm0019_applicability
NULL

#' @rdname acm0019_applicability
#' @export
check_applicability_monitoring_coverage <- function(data_capture_rate,
                                                    minimum_coverage = 0.9) {
  if (!is.numeric(data_capture_rate) || any(data_capture_rate < 0 | data_capture_rate > 1, na.rm = TRUE)) {
    rlang::abort("`data_capture_rate` must be numeric with values between 0 and 1.")
  }
  if (!is.numeric(minimum_coverage) || length(minimum_coverage) != 1 || is.na(minimum_coverage) ||
      minimum_coverage <= 0 || minimum_coverage > 1) {
    rlang::abort("`minimum_coverage` must be a numeric scalar between 0 and 1.")
  }

  data_capture_rate >= minimum_coverage
}

#' @rdname acm0019_applicability
#' @export
check_applicability_catalyst_configuration <- function(catalyst_configuration,
                                                       allowed_configurations = c("secondary", "tertiary")) {
  if (!is.character(catalyst_configuration)) {
    rlang::abort("`catalyst_configuration` must be a character vector.")
  }
  if (!is.character(allowed_configurations) || length(allowed_configurations) == 0) {
    rlang::abort("`allowed_configurations` must be a non-empty character vector.")
  }

  catalyst_configuration_clean <- stringr::str_to_lower(stringr::str_trim(catalyst_configuration))
  allowed_clean <- stringr::str_to_lower(stringr::str_trim(allowed_configurations))

  catalyst_configuration_clean %in% allowed_clean
}

#' @rdname acm0019_applicability
#' @export
check_applicability_operating_regime <- function(plant_regime,
                                                 has_continuous_monitoring) {
  if (!is.character(plant_regime) || length(plant_regime) != 1) {
    rlang::abort("`plant_regime` must be a character scalar.")
  }
  if (!is.logical(has_continuous_monitoring) || length(has_continuous_monitoring) != 1 ||
      is.na(has_continuous_monitoring)) {
    rlang::abort("`has_continuous_monitoring` must be a logical scalar.")
  }

  valid_regimes <- c("weak-acid", "strong-acid")
  regime_clean <- stringr::str_to_lower(stringr::str_trim(plant_regime))
  if (!regime_clean %in% valid_regimes) {
    return(FALSE)
  }

  has_continuous_monitoring
}
