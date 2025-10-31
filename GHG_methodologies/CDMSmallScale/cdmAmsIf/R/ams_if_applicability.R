#' Check AMS-I.F captive system capacity threshold
#'
#' Validates the AMS-I.F applicability condition that renewable electricity
#' systems supplying captive and mini-grid consumers remain within the 15 MW
#' small-scale limit for Type I CDM project activities.
#'
#' @param capacity_kw Rated electrical capacity in kilowatts (kW).
#' @param threshold_kw Threshold in kW, defaulting to 15000 (15 MW).
#' @return Logical indicating whether the capacity condition is satisfied.
#' @examples
#' check_applicability_captive_capacity(capacity_kw = 8000)
#' check_applicability_captive_capacity(capacity_kw = 25000)
#' @export
check_applicability_captive_capacity <- function(capacity_kw, threshold_kw = 15000) {
  if (!is.numeric(capacity_kw) || length(capacity_kw) != 1) {
    stop("`capacity_kw` must be a single numeric value.", call. = FALSE)
  }
  if (capacity_kw < 0) {
    stop("`capacity_kw` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(threshold_kw) || length(threshold_kw) != 1) {
    stop("`threshold_kw` must be a single numeric value.", call. = FALSE)
  }
  if (threshold_kw <= 0) {
    stop("`threshold_kw` must be positive.", call. = FALSE)
  }

  capacity_kw <= threshold_kw
}

#' Check AMS-I.F renewable penetration of supply
#'
#' Ensures the captive mini-grid is predominantly supplied by renewable energy
#' resources (e.g. solar PV, wind, small hydro). The helper compares the
#' reported renewable share to a user-defined minimum fraction.
#'
#' @param renewable_fraction Share of total electricity supplied by renewable sources (0-1).
#' @param minimum_fraction Minimum renewable share required for eligibility (default 0.75).
#' @return Logical indicating whether the renewable supply criterion is met.
#' @examples
#' check_applicability_renewable_penetration(renewable_fraction = 0.85)
#' check_applicability_renewable_penetration(renewable_fraction = 0.6)
#' @export
check_applicability_renewable_penetration <- function(renewable_fraction, minimum_fraction = 0.75) {
  if (!is.numeric(renewable_fraction) || length(renewable_fraction) != 1) {
    stop("`renewable_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (renewable_fraction < 0 || renewable_fraction > 1) {
    stop("`renewable_fraction` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    stop("`minimum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  renewable_fraction >= minimum_fraction
}

#' Check AMS-I.F captive use share
#'
#' Confirms the renewable system primarily serves captive demand by evaluating
#' the share of net generation consumed on-site or within the mini-grid.
#'
#' @param captive_use_share Share of renewable electricity consumed within the captive system (0-1).
#' @param minimum_fraction Minimum captive share required for eligibility (default 0.6).
#' @return Logical indicating whether the captive-use criterion is satisfied.
#' @examples
#' check_applicability_captive_use_share(captive_use_share = 0.8)
#' check_applicability_captive_use_share(captive_use_share = 0.4)
#' @export
check_applicability_captive_use_share <- function(captive_use_share, minimum_fraction = 0.6) {
  if (!is.numeric(captive_use_share) || length(captive_use_share) != 1) {
    stop("`captive_use_share` must be a single numeric value.", call. = FALSE)
  }
  if (captive_use_share < 0 || captive_use_share > 1) {
    stop("`captive_use_share` must fall between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(minimum_fraction) || length(minimum_fraction) != 1) {
    stop("`minimum_fraction` must be a single numeric value.", call. = FALSE)
  }
  if (minimum_fraction < 0 || minimum_fraction > 1) {
    stop("`minimum_fraction` must fall between 0 and 1.", call. = FALSE)
  }

  captive_use_share >= minimum_fraction
}

#' Assess combined AMS-I.F applicability conditions
#'
#' Evaluates the primary quantitative applicability criteria for AMS-I.F,
#' returning a tibble summarising each test and whether it passed. The helper is
#' designed for use in workflows that need a transparent record of the
#' applicability assessment.
#'
#' @param capacity_kw Rated renewable capacity in kilowatts.
#' @param renewable_fraction Fraction of electricity supplied by renewable sources.
#' @param captive_use_share Fraction of electricity consumed within the captive or mini-grid system.
#' @param capacity_threshold_kw Capacity threshold in kilowatts (default 15000).
#' @param renewable_minimum_fraction Minimum renewable penetration required (default 0.75).
#' @param captive_minimum_fraction Minimum captive use share required (default 0.6).
#' @return A tibble with one row per applicability condition and a `passes` column.
#' @examples
#' assess_ams_if_applicability(
#'   capacity_kw = 9000,
#'   renewable_fraction = 0.9,
#'   captive_use_share = 0.7
#' )
#' @export
assess_ams_if_applicability <- function(capacity_kw,
                                        renewable_fraction,
                                        captive_use_share,
                                        capacity_threshold_kw = 15000,
                                        renewable_minimum_fraction = 0.75,
                                        captive_minimum_fraction = 0.6) {
  checks <- tibble::tibble(
    condition = c(
      "Capacity below small-scale threshold",
      "Renewable penetration sufficient",
      "Captive use share sufficient"
    ),
    passes = c(
      check_applicability_captive_capacity(capacity_kw, capacity_threshold_kw),
      check_applicability_renewable_penetration(renewable_fraction, renewable_minimum_fraction),
      check_applicability_captive_use_share(captive_use_share, captive_minimum_fraction)
    )
  )

  checks
}
