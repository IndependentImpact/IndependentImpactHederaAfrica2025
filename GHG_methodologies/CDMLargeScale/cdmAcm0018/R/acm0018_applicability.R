#' Check ACM0018 biomass feedstock eligibility
#'
#' Validates that the biomass feedstock originates from renewable sources
#' recognised by the methodology, such as agricultural residues or dedicated
#' energy crops.
#'
#' @param feedstock Character vector naming the feedstock category for each
#'   facility.
#' @param allowed_feedstocks Optional character vector overriding the recognised
#'   feedstock categories.
#'
#' @return Logical vector indicating whether each feedstock is admissible under
#'   ACM0018.
#' @examples
#' check_applicability_biomass_feedstock_acm0018(
#'   c("agricultural residues", "coal"),
#'   allowed_feedstocks = c("agricultural residues", "forest residues")
#' )
#' @export
check_applicability_biomass_feedstock_acm0018 <- function(feedstock,
                                                          allowed_feedstocks = c(
                                                            "agricultural residues",
                                                            "forest residues",
                                                            "energy crops",
                                                            "bagasse",
                                                            "wood processing residues",
                                                            "biogenic waste"
                                                          )) {
  if (!is.character(feedstock)) {
    rlang::abort("`feedstock` must be a character vector.")
  }
  if (!is.character(allowed_feedstocks) || length(allowed_feedstocks) == 0) {
    rlang::abort("`allowed_feedstocks` must be a non-empty character vector.")
  }

  tolower(feedstock) %in% tolower(allowed_feedstocks)
}

#' Check ACM0018 power-only plant configuration
#'
#' Ensures that the project activity is a power-only plant and that no steam or
#' heat is exported to external users.
#'
#' @param exports_heat Logical vector indicating whether the facility exports
#'   heat or steam.
#' @param generates_electricity Logical vector confirming that the project
#'   generates electricity for delivery to the grid.
#'
#' @return Logical vector confirming that each facility satisfies the power-only
#'   configuration criteria.
#' @examples
#' check_applicability_power_only_acm0018(c(FALSE, TRUE), c(TRUE, TRUE))
#' @export
check_applicability_power_only_acm0018 <- function(exports_heat,
                                                   generates_electricity) {
  if (!is.logical(exports_heat)) {
    rlang::abort("`exports_heat` must be a logical vector.")
  }
  if (!is.logical(generates_electricity) ||
      length(generates_electricity) != length(exports_heat)) {
    rlang::abort("`generates_electricity` must be a logical vector matching `exports_heat` length.")
  }

  !exports_heat & generates_electricity
}

#' Check ACM0018 biomass fraction requirement
#'
#' Confirms that biomass provides the dominant share of the fuel input to the
#' power plant, typically at least 80 percent on an energy basis.
#'
#' @param biomass_fraction Numeric vector expressing the biomass energy share
#'   between 0 and 1.
#' @param minimum_fraction Minimum required biomass share. Defaults to 0.8.
#'
#' @return Logical vector indicating whether the biomass fraction requirement is
#'   satisfied.
#' @examples
#' check_applicability_biomass_fraction_acm0018(c(0.9, 0.7))
#' @export
check_applicability_biomass_fraction_acm0018 <- function(biomass_fraction,
                                                         minimum_fraction = 0.8) {
  if (!is.numeric(biomass_fraction) || any(is.na(biomass_fraction))) {
    rlang::abort("`biomass_fraction` must be a numeric vector without NA values.")
  }
  if (any(biomass_fraction < 0 | biomass_fraction > 1)) {
    rlang::abort("`biomass_fraction` values must lie between 0 and 1.")
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    rlang::abort("`minimum_fraction` must be a single numeric value.")
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    rlang::abort("`minimum_fraction` must lie between 0 and 1.")
  }

  biomass_fraction >= minimum_fraction
}
