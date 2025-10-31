#' Check ACM0001 landfill management eligibility
#'
#' Evaluates whether the landfill is managed in a way that supports gas
#' collection, reflecting ACM0001 applicability conditions that exclude open
#' dumps without daily cover or leachate control.
#'
#' @param landfill_management Character string describing the landfill's
#'   management practice (e.g. "sanitary", "controlled", "open dump").
#' @param has_leachate_control Logical flag indicating if leachate control is
#'   implemented.
#' @return Logical value signifying whether the landfill meets ACM0001
#'   management requirements for gas capture.
#' @examples
#' check_applicability_landfill_management("sanitary", TRUE)
#' check_applicability_landfill_management("open dump", FALSE)
#' @export
check_applicability_landfill_management <- function(landfill_management,
                                                    has_leachate_control) {
  if (!is.character(landfill_management) || length(landfill_management) != 1) {
    rlang::abort("`landfill_management` must be a single character string.")
  }
  if (!is.logical(has_leachate_control) || length(has_leachate_control) != 1) {
    rlang::abort("`has_leachate_control` must be a single logical value.")
  }

  allowed <- c("sanitary", "controlled")
  tolower(landfill_management) %in% allowed && has_leachate_control
}

#' Check ACM0001 gas collection system readiness
#'
#' Validates that the landfill has installed or will install a gas collection
#' system with adequate capture efficiency to justify ACM0001 application.
#'
#' @param collection_system_installed Logical flag indicating whether a gas
#'   collection system is installed or planned for the project.
#' @param expected_capture_efficiency Numeric value between 0 and 1 describing
#'   the expected methane capture efficiency.
#' @param minimum_efficiency Minimum acceptable capture efficiency. Defaults to
#'   0.2, reflecting ACM0001 requirements that projects collect a meaningful
#'   fraction of generated gas.
#' @return Logical indicating whether the collection system satisfies ACM0001
#'   expectations.
#' @examples
#' check_applicability_gas_collection(TRUE, 0.4)
#' check_applicability_gas_collection(FALSE, 0.3)
#' @export
check_applicability_gas_collection <- function(collection_system_installed,
                                               expected_capture_efficiency,
                                               minimum_efficiency = 0.2) {
  if (!is.logical(collection_system_installed) ||
      length(collection_system_installed) != 1) {
    rlang::abort("`collection_system_installed` must be a single logical value.")
  }
  if (!is.numeric(expected_capture_efficiency) ||
      length(expected_capture_efficiency) != 1) {
    rlang::abort("`expected_capture_efficiency` must be a single numeric value.")
  }
  if (!is.numeric(minimum_efficiency) || length(minimum_efficiency) != 1) {
    rlang::abort("`minimum_efficiency` must be a single numeric value.")
  }
  if (expected_capture_efficiency < 0 || expected_capture_efficiency > 1) {
    rlang::abort("`expected_capture_efficiency` must lie between 0 and 1.")
  }
  if (minimum_efficiency < 0 || minimum_efficiency > 1) {
    rlang::abort("`minimum_efficiency` must lie between 0 and 1.")
  }

  collection_system_installed && expected_capture_efficiency >= minimum_efficiency
}
