#' Check ACM0002 grid connection status
#'
#' Validates that the renewable power plant is grid-connected and exports
#' a sufficient share of its net electricity to the grid, mirroring the
#' ACM0002 applicability condition for renewable generators.
#'
#' @param connection_status Character string describing the interconnection
#'   status. Accepts values such as "grid-connected", "isolated", or
#'   "mini-grid".
#' @param export_share Numeric value between 0 and 1 indicating the share of
#'   net generation exported to the grid. Defaults to 1 for full export.
#' @param minimum_export Numeric value between 0 and 1 specifying the minimum
#'   export share required to satisfy the methodology. Defaults to 0.9 to
#'   represent the ACM0002 focus on grid-delivered electricity.
#' @return Logical indicating whether the plant qualifies as grid-connected
#'   under ACM0002 assumptions.
#' @examples
#' check_applicability_grid_connection("grid-connected", export_share = 0.95)
#' check_applicability_grid_connection("isolated", export_share = 0.5)
#' @seealso [check_applicability_renewable_technology()]
#' @export
check_applicability_grid_connection <- function(connection_status,
                                                export_share = 1,
                                                minimum_export = 0.9) {
  if (!is.character(connection_status) || length(connection_status) != 1) {
    rlang::abort("`connection_status` must be a single character string.")
  }
  if (!is.numeric(export_share) || length(export_share) != 1) {
    rlang::abort("`export_share` must be a single numeric value between 0 and 1.")
  }
  if (!is.numeric(minimum_export) || length(minimum_export) != 1) {
    rlang::abort("`minimum_export` must be a single numeric value between 0 and 1.")
  }
  if (export_share < 0 || export_share > 1) {
    rlang::abort("`export_share` must lie between 0 and 1.")
  }
  if (minimum_export < 0 || minimum_export > 1) {
    rlang::abort("`minimum_export` must lie between 0 and 1.")
  }

  tolower(connection_status) == "grid-connected" && export_share >= minimum_export
}

#' Check ACM0002 renewable technology eligibility
#'
#' Confirms that the generating technology is among the renewable sources
#' covered by ACM0002 (wind, solar, hydro, geothermal, and biomass without
#' fossil co-firing).
#'
#' @param technology Character string describing the primary energy source.
#' @param allowed Vector of admissible technologies. Defaults to the ACM0002
#'   renewable set.
#' @return Logical indicating whether the technology is eligible.
#' @examples
#' check_applicability_renewable_technology("wind")
#' check_applicability_renewable_technology("coal")
#' @export
check_applicability_renewable_technology <- function(technology,
                                                     allowed = c("wind",
                                                                 "solar",
                                                                 "hydro",
                                                                 "geothermal",
                                                                 "biomass")) {
  if (!is.character(technology) || length(technology) != 1) {
    rlang::abort("`technology` must be a single character string.")
  }
  if (!is.character(allowed) || length(allowed) == 0) {
    rlang::abort("`allowed` must be a non-empty character vector.")
  }

  tolower(technology) %in% tolower(allowed)
}
