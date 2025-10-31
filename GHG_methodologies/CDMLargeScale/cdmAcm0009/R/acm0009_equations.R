#' Energy and emission equations for ACM0009 fuel switching projects
#'
#' Helper functions translating ACM0009 equations into reusable R code. The
#' functions operate on numeric vectors and recycle scalar inputs where
#' appropriate, providing baseline emissions, project emissions, leakage, and net
#' emission reductions.
#'
#' @param fuel_quantity Numeric vector of fuel consumed during a monitoring
#'   period expressed in native units (e.g. tonnes for coal or thousand Nm3 for
#'   natural gas).
#' @param net_calorific_value Numeric vector of net calorific values in
#'   terajoules per unit of fuel quantity.
#' @param emission_factor Numeric vector of CO2 emission factors in tonnes CO2e
#'   per terajoule.
#' @param methane_slip_m3 Numeric vector of uncombusted methane volumes in cubic
#'   metres.
#' @param methane_density Density of methane in tonnes per cubic metre. Defaults
#'   to `0.0007168`.
#' @param gwp_ch4 Global warming potential for methane. Defaults to `28`.
#' @param additional_leakage_tco2e Numeric vector of additional leakage emissions
#'   (e.g. transport of LNG) expressed directly in tonnes of CO2e.
#' @param baseline_emissions_tco2e Numeric vector of baseline emissions in tCO2e.
#' @param project_emissions_tco2e Numeric vector of project emissions in tCO2e.
#' @param leakage_emissions_tco2e Numeric vector of leakage emissions in tCO2e.
#'
#' @return Numeric vector with the requested emissions quantity.
#' @name acm0009_equations
NULL

validate_non_negative_numeric <- function(x, name) {
  if (!is.numeric(x)) {
    rlang::abort(sprintf("`%s` must be numeric.", name))
  }
  if (any(x < 0, na.rm = TRUE)) {
    rlang::abort(sprintf("`%s` must be non-negative.", name))
  }
}

recycle_to_length <- function(values, target_length, name) {
  if (!length(values) %in% c(1, target_length)) {
    rlang::abort(sprintf("`%s` must have length 1 or match the longest input.", name))
  }
  rep(values, length.out = target_length)
}

#' @rdname acm0009_equations
#' @export
calculate_energy_content_acm0009 <- function(fuel_quantity, net_calorific_value) {
  validate_non_negative_numeric(fuel_quantity, "fuel_quantity")
  validate_non_negative_numeric(net_calorific_value, "net_calorific_value")

  inputs <- list(fuel_quantity = fuel_quantity, net_calorific_value = net_calorific_value)
  target_length <- max(lengths(inputs))
  purrr::iwalk(inputs, function(x, nm) {
    if (!length(x) %in% c(1, target_length)) {
      rlang::abort(paste0("`", nm, "` must have length 1 or match the longest input."))
    }
  })

  fuel_values <- recycle_to_length(fuel_quantity, target_length, "fuel_quantity")
  ncv_values <- recycle_to_length(net_calorific_value, target_length, "net_calorific_value")

  fuel_values * ncv_values
}

#' @rdname acm0009_equations
#' @export
calculate_baseline_emissions_acm0009 <- function(fuel_quantity,
                                                 net_calorific_value,
                                                 emission_factor) {
  validate_non_negative_numeric(emission_factor, "emission_factor")
  energy <- calculate_energy_content_acm0009(fuel_quantity, net_calorific_value)
  emission_values <- recycle_to_length(emission_factor, length(energy), "emission_factor")
  energy * emission_values
}

#' @rdname acm0009_equations
#' @export
calculate_project_emissions_acm0009 <- function(fuel_quantity,
                                                net_calorific_value,
                                                emission_factor) {
  calculate_baseline_emissions_acm0009(fuel_quantity, net_calorific_value, emission_factor)
}

#' @rdname acm0009_equations
#' @export
calculate_leakage_emissions_acm0009 <- function(methane_slip_m3 = 0,
                                                methane_density = 0.0007168,
                                                gwp_ch4 = 28,
                                                additional_leakage_tco2e = 0) {
  validate_non_negative_numeric(methane_slip_m3, "methane_slip_m3")
  validate_non_negative_numeric(additional_leakage_tco2e, "additional_leakage_tco2e")

  if (!is.numeric(methane_density) || length(methane_density) != 1 ||
      is.na(methane_density) || methane_density <= 0) {
    rlang::abort("`methane_density` must be a positive numeric scalar.")
  }
  if (!is.numeric(gwp_ch4) || length(gwp_ch4) != 1 || is.na(gwp_ch4) || gwp_ch4 <= 0) {
    rlang::abort("`gwp_ch4` must be a positive numeric scalar.")
  }

  inputs <- list(methane_slip_m3 = methane_slip_m3, additional_leakage_tco2e = additional_leakage_tco2e)
  target_length <- max(lengths(inputs))
  if (target_length == 0) {
    target_length <- 1
  }
  purrr::iwalk(inputs, function(x, nm) {
    if (!length(x) %in% c(1, target_length)) {
      rlang::abort(paste0("`", nm, "` must have length 1 or match the longest input."))
    }
  })

  methane_values <- recycle_to_length(methane_slip_m3, target_length, "methane_slip_m3")
  leakage_values <- recycle_to_length(additional_leakage_tco2e, target_length, "additional_leakage_tco2e")

  methane_emissions <- methane_values * methane_density * gwp_ch4
  methane_emissions + leakage_values
}

#' @rdname acm0009_equations
#' @export
calculate_net_emission_reductions_acm0009 <- function(baseline_emissions_tco2e,
                                                      project_emissions_tco2e,
                                                      leakage_emissions_tco2e = 0) {
  validate_non_negative_numeric(baseline_emissions_tco2e, "baseline_emissions_tco2e")
  validate_non_negative_numeric(project_emissions_tco2e, "project_emissions_tco2e")
  validate_non_negative_numeric(leakage_emissions_tco2e, "leakage_emissions_tco2e")

  inputs <- list(
    baseline_emissions_tco2e = baseline_emissions_tco2e,
    project_emissions_tco2e = project_emissions_tco2e,
    leakage_emissions_tco2e = leakage_emissions_tco2e
  )
  target_length <- max(lengths(inputs))
  purrr::iwalk(inputs, function(x, nm) {
    if (!length(x) %in% c(1, target_length)) {
      rlang::abort(paste0("`", nm, "` must have length 1 or match the longest input."))
    }
  })

  baseline_values <- recycle_to_length(baseline_emissions_tco2e, target_length, "baseline_emissions_tco2e")
  project_values <- recycle_to_length(project_emissions_tco2e, target_length, "project_emissions_tco2e")
  leakage_values <- recycle_to_length(leakage_emissions_tco2e, target_length, "leakage_emissions_tco2e")

  baseline_values - project_values - leakage_values
}
