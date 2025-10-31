#' Check baseline fuel applicability for ACM0009
#'
#' Evaluates whether the baseline fuel mix is dominated by coal and/or petroleum
#' fuels as required by ACM0009. The function confirms that a minimum share of
#' the supplied fuel types belongs to the eligible set and that no natural gas is
#' present in the baseline configuration.
#'
#' @param baseline_fuels Character vector describing fuels used in the baseline
#'   scenario (e.g. "bituminous coal", "heavy fuel oil").
#' @param min_share Minimum share of eligible fuels required for applicability.
#'   Defaults to `0.9`, meaning at least 90% of fuels must be coal or petroleum
#'   based.
#'
#' @return Logical scalar indicating whether the baseline fuel mix satisfies the
#'   applicability conditions.
#' @examples
#' check_applicability_baseline_fuels(c("bituminous coal", "heavy fuel oil"))
#' check_applicability_baseline_fuels(c("bituminous coal", "natural gas"))
#' @export
check_applicability_baseline_fuels <- function(baseline_fuels, min_share = 0.9) {
  if (!is.character(baseline_fuels)) {
    rlang::abort("`baseline_fuels` must be a character vector.")
  }
  if (!is.numeric(min_share) || length(min_share) != 1 || is.na(min_share)) {
    rlang::abort("`min_share` must be a single numeric value.")
  }
  if (min_share <= 0 || min_share > 1) {
    rlang::abort("`min_share` must be between 0 and 1.")
  }

  fuels_lower <- tolower(baseline_fuels)
  eligible <- c(
    "coal", "bituminous coal", "sub-bituminous coal", "lignite",
    "heavy fuel oil", "fuel oil", "diesel", "residual oil"
  )
  ineligible <- c("natural gas", "lng", "cng")

  if (length(fuels_lower) == 0) {
    return(FALSE)
  }
  if (any(fuels_lower %in% ineligible)) {
    return(FALSE)
  }

  share_eligible <- mean(fuels_lower %in% eligible)
  share_eligible >= min_share
}

#' Check project fuel applicability for ACM0009
#'
#' Confirms that the project fuel is natural gas delivered either via pipeline
#' or in liquefied/compressed form, as stipulated by ACM0009.
#'
#' @param project_fuel Character vector describing the project fuel type.
#'
#' @return Logical vector marking each element as an eligible project fuel.
#' @examples
#' check_applicability_project_fuel(c("natural gas", "lng", "coal"))
#' @export
check_applicability_project_fuel <- function(project_fuel) {
  if (!is.character(project_fuel)) {
    rlang::abort("`project_fuel` must be a character vector.")
  }

  allowed <- c("natural gas", "liquefied natural gas", "lng", "cng", "compressed natural gas")
  tolower(project_fuel) %in% allowed
}
