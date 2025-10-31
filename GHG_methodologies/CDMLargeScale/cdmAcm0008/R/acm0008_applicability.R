#' Check ACM0008 coal mine type eligibility
#'
#' Validates that the project occurs at an underground or surface coal mine
#' with documented methane drainage. The helper enforces the qualitative
#' applicability criteria that coal mine methane (CMM) and/or ventilation air
#' methane (VAM) recovery projects are implemented at active or recently closed
#' mines with available methane resources.
#'
#' @param mine_type Character string naming the mine classification. Accepted
#'   values include "underground", "surface", and "post-closure".
#' @param requires_drainage Logical flag indicating whether methane drainage
#'   infrastructure is installed or will be installed by the project activity.
#' @param allowed_types Optional character vector overriding the recognised
#'   mine categories for validation.
#'
#' @return A single logical value signalling whether the methodology can be
#'   applied to the provided mine configuration.
#'
#' @examples
#' check_applicability_mine_type("underground", TRUE)
#' check_applicability_mine_type("lignite", TRUE)
#' @export
check_applicability_mine_type <- function(mine_type,
                                          requires_drainage,
                                          allowed_types = c(
                                            "underground",
                                            "surface",
                                            "post-closure"
                                          )) {
  if (!is.character(mine_type) || length(mine_type) != 1) {
    rlang::abort("`mine_type` must be a single character string.")
  }
  if (!is.logical(requires_drainage) || length(requires_drainage) != 1) {
    rlang::abort("`requires_drainage` must be a single logical value.")
  }
  if (!is.character(allowed_types) || length(allowed_types) == 0) {
    rlang::abort("`allowed_types` must be a non-empty character vector.")
  }

  tolower(mine_type) %in% tolower(allowed_types) && requires_drainage
}

#' Check ACM0008 methane content requirement
#'
#' Ensures that drained or ventilation air methane has sufficient methane
#' concentration to support oxidation or utilisation technologies. The
#' methodology typically requires methane volume fractions above 0.3 for CMM
#' capture and 0.004 for VAM oxidation systems.
#'
#' @param methane_fraction Numeric vector with methane volume fraction values
#'   expressed between 0 and 1.
#' @param minimum_fraction Minimum admissible fraction for the technology
#'   considered. Defaults to 0.3, corresponding to drainage gas projects.
#'
#' @return Logical vector indicating whether each observation meets the methane
#'   content threshold.
#'
#' @examples
#' check_applicability_methane_content(c(0.32, 0.28))
#' check_applicability_methane_content(0.007, minimum_fraction = 0.004)
#' @export
check_applicability_methane_content <- function(methane_fraction,
                                                minimum_fraction = 0.3) {
  if (!is.numeric(methane_fraction) || any(is.na(methane_fraction))) {
    rlang::abort("`methane_fraction` must be a numeric vector without NA values.")
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    rlang::abort("`minimum_fraction` must be a single numeric value.")
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    rlang::abort("`minimum_fraction` must lie between 0 and 1.")
  }
  if (any(methane_fraction < 0 | methane_fraction > 1)) {
    rlang::abort("`methane_fraction` values must lie between 0 and 1.")
  }

  methane_fraction >= minimum_fraction
}

#' Check ACM0008 utilisation pathway eligibility
#'
#' Validates that the chosen utilisation or oxidation pathway is supported by
#' the methodology and that all combustion devices are enclosed and metered.
#'
#' @param pathway Character vector describing the utilisation option (e.g.
#'   "electricity", "thermal", "flaring", "oxidation").
#' @param enclosed_combustion Logical vector flagging whether the combustion or
#'   oxidation units are enclosed and equipped with continuous monitoring.
#' @param supported Optional character vector listing recognised pathways.
#'
#' @return Logical vector confirming that each pathway satisfies the ACM0008
#'   eligibility requirements.
#'
#' @examples
#' check_applicability_utilisation_pathway(
#'   c("electricity", "venting"),
#'   c(TRUE, TRUE)
#' )
#' @export
check_applicability_utilisation_pathway <- function(pathway,
                                                    enclosed_combustion,
                                                    supported = c(
                                                      "electricity",
                                                      "thermal",
                                                      "flaring",
                                                      "oxidation"
                                                    )) {
  if (!is.character(pathway)) {
    rlang::abort("`pathway` must be a character vector.")
  }
  if (!is.logical(enclosed_combustion) || length(enclosed_combustion) != length(pathway)) {
    rlang::abort("`enclosed_combustion` must be a logical vector matching `pathway` length.")
  }
  if (!is.character(supported) || length(supported) == 0) {
    rlang::abort("`supported` must be a non-empty character vector.")
  }

  tolower(pathway) %in% tolower(supported) & enclosed_combustion
}
