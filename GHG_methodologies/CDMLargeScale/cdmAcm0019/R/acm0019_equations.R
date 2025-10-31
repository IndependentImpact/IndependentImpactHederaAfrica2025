#' Equation helpers for ACM0019 nitric acid projects
#'
#' These helpers translate the core equations of ACM0019 into reusable
#' functions covering baseline emissions, project emissions, leakage and net
#' emission reductions. Inputs follow tidyverse conventions and are validated
#' for non-negative numeric values.
#'
#' @param production_tonnes Numeric vector of nitric acid production in metric
#'   tonnes.
#' @param baseline_ef_kg_per_tonne Numeric vector of baseline N2O emission
#'   factors expressed in kilograms of N2O per tonne of nitric acid.
#' @param project_ef_kg_per_tonne Numeric vector of project N2O emission factors
#'   expressed in kilograms of N2O per tonne of nitric acid.
#' @param gwp_n2o Global warming potential for nitrous oxide. Defaults to 265
#'   (AR5 value used in recent CDM guidance).
#' @param electricity_mwh Numeric vector of auxiliary electricity consumption in
#'   megawatt-hours.
#' @param grid_ef_t_per_mwh Numeric vector of grid emission factors in tonnes of
#'   CO2 per megawatt-hour.
#' @param steam_tonnes Numeric vector of imported steam quantities in tonnes.
#' @param steam_ef_t_per_tonne Numeric vector of steam emission factors in
#'   tonnes of CO2 per tonne of steam.
#' @param baseline_emissions_tco2e Numeric vector of baseline emissions in
#'   tonnes of CO2 equivalent.
#' @param project_emissions_tco2e Numeric vector of project emissions in tonnes
#'   of CO2 equivalent.
#' @param leakage_emissions_tco2e Numeric vector of leakage emissions in tonnes
#'   of CO2 equivalent.
#'
#' @return Numeric vector containing the requested emissions quantity in tonnes
#'   of CO2 equivalent.
#' @examples
#' baseline <- calculate_baseline_emissions_acm0019(1500, 9.5)
#' project <- calculate_project_emissions_acm0019(1500, 1.2)
#' leakage <- calculate_leakage_emissions_acm0019(120, 0.7, 50, 0.08)
#' calculate_net_emission_reductions_acm0019(baseline, project, leakage)
#' @name acm0019_equations
NULL

#' @rdname acm0019_equations
#' @export
calculate_baseline_emissions_acm0019 <- function(production_tonnes,
                                                 baseline_ef_kg_per_tonne,
                                                 gwp_n2o = 265) {
  if (!is.numeric(production_tonnes) || any(production_tonnes < 0, na.rm = TRUE)) {
    rlang::abort("`production_tonnes` must be a non-negative numeric vector.")
  }
  if (!is.numeric(baseline_ef_kg_per_tonne) || any(baseline_ef_kg_per_tonne < 0, na.rm = TRUE)) {
    rlang::abort("`baseline_ef_kg_per_tonne` must be a non-negative numeric vector.")
  }
  if (length(production_tonnes) != length(baseline_ef_kg_per_tonne)) {
    rlang::abort("`production_tonnes` and `baseline_ef_kg_per_tonne` must have the same length.")
  }
  if (!is.numeric(gwp_n2o) || length(gwp_n2o) != 1 || is.na(gwp_n2o) || gwp_n2o <= 0) {
    rlang::abort("`gwp_n2o` must be a positive numeric scalar.")
  }

  n2o_tonnes <- (production_tonnes * baseline_ef_kg_per_tonne) / 1000
  n2o_tonnes * gwp_n2o
}

#' @rdname acm0019_equations
#' @export
calculate_project_emissions_acm0019 <- function(production_tonnes,
                                                project_ef_kg_per_tonne,
                                                gwp_n2o = 265) {
  if (!is.numeric(production_tonnes) || any(production_tonnes < 0, na.rm = TRUE)) {
    rlang::abort("`production_tonnes` must be a non-negative numeric vector.")
  }
  if (!is.numeric(project_ef_kg_per_tonne) || any(project_ef_kg_per_tonne < 0, na.rm = TRUE)) {
    rlang::abort("`project_ef_kg_per_tonne` must be a non-negative numeric vector.")
  }
  if (length(production_tonnes) != length(project_ef_kg_per_tonne)) {
    rlang::abort("`production_tonnes` and `project_ef_kg_per_tonne` must have the same length.")
  }
  if (!is.numeric(gwp_n2o) || length(gwp_n2o) != 1 || is.na(gwp_n2o) || gwp_n2o <= 0) {
    rlang::abort("`gwp_n2o` must be a positive numeric scalar.")
  }

  n2o_tonnes <- (production_tonnes * project_ef_kg_per_tonne) / 1000
  n2o_tonnes * gwp_n2o
}

#' @rdname acm0019_equations
#' @export
calculate_leakage_emissions_acm0019 <- function(electricity_mwh = 0,
                                                grid_ef_t_per_mwh = 0,
                                                steam_tonnes = 0,
                                                steam_ef_t_per_tonne = 0) {
  numeric_inputs <- list(
    electricity_mwh = electricity_mwh,
    grid_ef_t_per_mwh = grid_ef_t_per_mwh,
    steam_tonnes = steam_tonnes,
    steam_ef_t_per_tonne = steam_ef_t_per_tonne
  )
  purrr::iwalk(numeric_inputs, function(x, nm) {
    if (!is.numeric(x) || any(x < 0, na.rm = TRUE)) {
      rlang::abort(paste0("`", nm, "` must be a non-negative numeric vector."))
    }
  })
  if (length(electricity_mwh) != length(grid_ef_t_per_mwh)) {
    rlang::abort("`electricity_mwh` and `grid_ef_t_per_mwh` must have the same length.")
  }
  if (length(steam_tonnes) != length(steam_ef_t_per_tonne)) {
    rlang::abort("`steam_tonnes` and `steam_ef_t_per_tonne` must have the same length.")
  }

  electricity_emissions <- electricity_mwh * grid_ef_t_per_mwh
  steam_emissions <- steam_tonnes * steam_ef_t_per_tonne
  electricity_emissions + steam_emissions
}

#' @rdname acm0019_equations
#' @export
calculate_net_emission_reductions_acm0019 <- function(baseline_emissions_tco2e,
                                                      project_emissions_tco2e,
                                                      leakage_emissions_tco2e = 0) {
  numeric_inputs <- list(
    baseline_emissions_tco2e = baseline_emissions_tco2e,
    project_emissions_tco2e = project_emissions_tco2e,
    leakage_emissions_tco2e = leakage_emissions_tco2e
  )
  purrr::iwalk(numeric_inputs, function(x, nm) {
    if (!is.numeric(x) || any(x < 0, na.rm = TRUE)) {
      rlang::abort(paste0("`", nm, "` must be a non-negative numeric vector."))
    }
  })

  target_length <- max(lengths(numeric_inputs))
  purrr::iwalk(numeric_inputs, function(x, nm) {
    if (!length(x) %in% c(1, target_length)) {
      rlang::abort(paste0("`", nm, "` must have length 1 or match the longest input."))
    }
  })

  baseline_values <- rep(baseline_emissions_tco2e, length.out = target_length)
  project_values <- rep(project_emissions_tco2e, length.out = target_length)
  leakage_values <- rep(leakage_emissions_tco2e, length.out = target_length)

  baseline_values - project_values - leakage_values
}
