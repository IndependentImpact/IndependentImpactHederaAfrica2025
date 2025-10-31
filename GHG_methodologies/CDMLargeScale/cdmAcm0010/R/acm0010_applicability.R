#' Validate ACM0010 feedstock mix applicability
#'
#' Ensures the project feedstock mix is dominated by eligible organic wastes
#' such as manure, agricultural residues, and high-strength wastewater as
#' required by ACM0010. The function checks that at least 70 percent of the
#' provided feedstock categories are eligible and that no forbidden categories
#' (e.g. municipal solid waste) are present.
#'
#' @param feedstock Character vector of feedstock categories supplied to the
#'   anaerobic digestion system.
#' @param eligible Minimum proportion of eligible categories required for
#'   applicability. Defaults to `0.7` in line with ACM0010 guidance that the
#'   project must primarily treat manure or wastewater streams.
#' @return Logical scalar indicating whether the feedstock mix satisfies the
#'   methodology's applicability conditions.
#' @examples
#' check_applicability_feedstock_mix(c("manure", "wastewater"))
#' check_applicability_feedstock_mix(c("manure", "municipal solid waste"))
#' @export
check_applicability_feedstock_mix <- function(feedstock, eligible = 0.7) {
  if (!is.character(feedstock)) {
    rlang::abort("`feedstock` must be a character vector.")
  }
  if (!is.numeric(eligible) || length(eligible) != 1 || is.na(eligible)) {
    rlang::abort("`eligible` must be a single numeric value.")
  }
  if (eligible <= 0 || eligible > 1) {
    rlang::abort("`eligible` must be between 0 and 1.")
  }

  allowed <- c("manure", "wastewater", "agricultural residue", "food waste")
  forbidden <- c("municipal solid waste", "hazardous waste")

  if (any(tolower(feedstock) %in% forbidden)) {
    return(FALSE)
  }

  share_eligible <- mean(tolower(feedstock) %in% allowed)
  share_eligible >= eligible
}

#' Validate ACM0010 baseline system applicability
#'
#' Confirms that the baseline waste treatment system is an uncontrolled lagoon
#' or similar high-emitting configuration, which ACM0010 targets for methane
#' capture. Controlled anaerobic digesters or aerobic systems are ineligible as
#' baselines.
#'
#' @param baseline_system Character vector describing the baseline treatment
#'   technology (e.g. "open lagoon", "anaerobic digester", "composting").
#' @return A logical vector marking each element as applicable or not.
#' @examples
#' check_applicability_baseline_system(c("open lagoon", "composting"))
#' @export
check_applicability_baseline_system <- function(baseline_system) {
  if (!is.character(baseline_system)) {
    rlang::abort("`baseline_system` must be a character vector.")
  }

  eligible <- c("open lagoon", "anaerobic pond", "manure pile", "slurry tank")
  ineligible <- c("anaerobic digester", "composting", "aerobic treatment")

  result <- rep(FALSE, length(baseline_system))
  eligible_idx <- tolower(baseline_system) %in% eligible
  ineligible_idx <- tolower(baseline_system) %in% ineligible

  result[eligible_idx] <- TRUE
  result[ineligible_idx] <- FALSE

  result
}
