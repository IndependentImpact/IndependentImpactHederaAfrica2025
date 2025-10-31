#' Check waste energy fraction meets ACM0012 threshold
#'
#' Ensures that the proportion of recovered energy originating from waste
#' sources is sufficient for ACM0012 eligibility. Equation 4 of ACM0012
#' requires at least 75\% of the exported energy to come from waste heat, waste
#' gas, or waste pressure streams.
#'
#' @param waste_energy_fraction Numeric vector giving the fraction of exported
#'   energy that originates from qualifying waste sources.
#' @param threshold Minimum qualifying fraction. Defaults to 0.75.
#' @return Logical vector indicating whether each observation is eligible.
#' @examples
#' check_waste_energy_fraction_acm0012(c(0.8, 0.7))
#' @export
check_waste_energy_fraction_acm0012 <- function(waste_energy_fraction, threshold = 0.75) {
  validate_non_negative_numeric(waste_energy_fraction, "waste_energy_fraction")
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) ||
      threshold < 0 || threshold > 1) {
    rlang::abort("`threshold` must be a single numeric value between 0 and 1.")
  }

  waste_energy_fraction >= threshold
}

#' Check that baseline equipment would continue operating without the project
#'
#' ACM0012 requires evidence that baseline equipment would have continued to run
#' in the absence of the project. This helper compares expected baseline
#' operating hours with observed project operating hours.
#'
#' @param baseline_operating_hours Expected annual operating hours under the
#'   baseline scenario.
#' @param project_operating_hours Observed annual operating hours for the waste
#'   energy recovery system.
#' @return Logical vector indicating whether baseline activity remains plausible.
#' @examples
#' check_baseline_continuation_acm0012(7500, 7000)
#' @export
check_baseline_continuation_acm0012 <- function(baseline_operating_hours,
                                                project_operating_hours) {
  validate_non_negative_numeric(baseline_operating_hours, "baseline_operating_hours")
  validate_non_negative_numeric(project_operating_hours, "project_operating_hours")
  recycle_inputs(baseline_operating_hours, project_operating_hours)

  baseline_operating_hours >= project_operating_hours * 0.9
}

#' Check that metering uncertainty satisfies ACM0012 requirements
#'
#' Applicability conditions require the combined uncertainty of key meters to be
#' below 1.5\% at the 95\% confidence level. This helper implements that test.
#'
#' @param measurement_uncertainty Percent uncertainty for the combined metering
#'   system expressed as a decimal (e.g., 0.015 for 1.5\%).
#' @return Logical vector that is `TRUE` when uncertainty requirements are met.
#' @examples
#' check_metering_uncertainty_acm0012(c(0.01, 0.02))
#' @export
check_metering_uncertainty_acm0012 <- function(measurement_uncertainty) {
  validate_non_negative_numeric(measurement_uncertainty, "measurement_uncertainty")

  measurement_uncertainty <= 0.015
}
