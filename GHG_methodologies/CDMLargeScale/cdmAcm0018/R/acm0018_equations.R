#' Calculate baseline emissions for ACM0018
#'
#' Computes baseline emissions from electricity that would otherwise be generated
#' by fossil-fuelled grid-connected plants.
#'
#' @param electricity_output_mwh Net electricity generation exported to the grid
#'   in megawatt-hours.
#' @param baseline_emission_factor Emission factor of the displaced grid in
#'   tonnes CO2e per megawatt-hour.
#'
#' @return Numeric vector of baseline emissions in tonnes CO2e.
#' @examples
#' calculate_baseline_emissions_acm0018(5000, 0.8)
#' @export
calculate_baseline_emissions_acm0018 <- function(electricity_output_mwh,
                                                 baseline_emission_factor) {
  if (!is.numeric(electricity_output_mwh) || any(electricity_output_mwh < 0)) {
    rlang::abort("`electricity_output_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(baseline_emission_factor) || any(baseline_emission_factor < 0)) {
    rlang::abort("`baseline_emission_factor` must be a non-negative numeric vector.")
  }
  if (!(length(baseline_emission_factor) %in% c(1, length(electricity_output_mwh)))) {
    rlang::abort("`baseline_emission_factor` must be length 1 or match `electricity_output_mwh`.")
  }

  electricity_output_mwh * baseline_emission_factor
}

#' Calculate project emissions for ACM0018
#'
#' Aggregates project emissions from auxiliary fossil fuel firing, emergency
#' fossil electricity generation, and biomass transport.
#'
#' @param auxiliary_fossil_tj Auxiliary fossil fuel consumption in terajoules.
#' @param auxiliary_fossil_ef Emission factor for auxiliary fuel in tonnes CO2e
#'   per terajoule.
#' @param onsite_generation_mwh Electricity produced using fossil fuels for
#'   internal needs in megawatt-hours.
#' @param onsite_emission_factor Emission factor for the fossil electricity
#'   generation in tonnes CO2e per megawatt-hour.
#' @param biomass_transport_tkm Biomass transport activity in tonne-kilometres.
#' @param transport_emission_factor Emission factor for biomass transport in
#'   tonnes CO2e per tonne-kilometre.
#'
#' @return Numeric vector of project emissions in tonnes CO2e.
#' @examples
#' calculate_project_emissions_acm0018(0.2, 74, 50, 0.7, 400, 0.00012)
#' @export
calculate_project_emissions_acm0018 <- function(auxiliary_fossil_tj,
                                                auxiliary_fossil_ef,
                                                onsite_generation_mwh = 0,
                                                onsite_emission_factor = 0,
                                                biomass_transport_tkm = 0,
                                                transport_emission_factor = 0) {
  if (!is.numeric(auxiliary_fossil_tj) || any(auxiliary_fossil_tj < 0)) {
    rlang::abort("`auxiliary_fossil_tj` must be a non-negative numeric vector.")
  }
  if (!is.numeric(auxiliary_fossil_ef) || any(auxiliary_fossil_ef < 0)) {
    rlang::abort("`auxiliary_fossil_ef` must be a non-negative numeric vector.")
  }
  if (!(length(auxiliary_fossil_ef) %in% c(1, length(auxiliary_fossil_tj)))) {
    rlang::abort("`auxiliary_fossil_ef` must be length 1 or match `auxiliary_fossil_tj`.")
  }
  if (!is.numeric(onsite_generation_mwh) || any(onsite_generation_mwh < 0)) {
    rlang::abort("`onsite_generation_mwh` must be a non-negative numeric vector.")
  }
  if (!is.numeric(onsite_emission_factor) || any(onsite_emission_factor < 0)) {
    rlang::abort("`onsite_emission_factor` must be a non-negative numeric vector.")
  }
  if (!(length(onsite_emission_factor) %in% c(1, length(onsite_generation_mwh)))) {
    rlang::abort("`onsite_emission_factor` must be length 1 or match `onsite_generation_mwh`.")
  }
  if (!is.numeric(biomass_transport_tkm) || any(biomass_transport_tkm < 0)) {
    rlang::abort("`biomass_transport_tkm` must be a non-negative numeric vector.")
  }
  if (!is.numeric(transport_emission_factor) || any(transport_emission_factor < 0)) {
    rlang::abort("`transport_emission_factor` must be a non-negative numeric vector.")
  }
  if (!(length(transport_emission_factor) %in% c(1, length(biomass_transport_tkm)))) {
    rlang::abort("`transport_emission_factor` must be length 1 or match `biomass_transport_tkm`.")
  }

  auxiliary_fossil_tj * auxiliary_fossil_ef +
    onsite_generation_mwh * onsite_emission_factor +
    biomass_transport_tkm * transport_emission_factor
}

#' Calculate leakage emissions for ACM0018
#'
#' Estimates leakage from market shifts in biomass consumption or upstream
#' residue diversion by applying a leakage fraction to the baseline emissions.
#'
#' @param leakage_fraction Numeric vector between 0 and 1 representing the
#'   fraction of baseline emissions attributed to leakage.
#' @param baseline_emissions Baseline emissions associated with the monitoring
#'   data in tonnes CO2e.
#'
#' @return Numeric vector of leakage emissions in tonnes CO2e.
#' @examples
#' calculate_leakage_emissions_acm0018(0.05, 1000)
#' @export
calculate_leakage_emissions_acm0018 <- function(leakage_fraction,
                                                baseline_emissions) {
  if (!is.numeric(leakage_fraction) || any(is.na(leakage_fraction))) {
    rlang::abort("`leakage_fraction` must be a numeric vector without NA values.")
  }
  if (any(leakage_fraction < 0 | leakage_fraction > 1)) {
    rlang::abort("`leakage_fraction` must lie between 0 and 1.")
  }
  if (!is.numeric(baseline_emissions) || any(baseline_emissions < 0)) {
    rlang::abort("`baseline_emissions` must be a non-negative numeric vector.")
  }
  if (!(length(leakage_fraction) %in% c(1, length(baseline_emissions)))) {
    rlang::abort("`leakage_fraction` must be length 1 or match `baseline_emissions`.")
  }

  leakage_fraction * baseline_emissions
}

#' Calculate emission reductions for ACM0018
#'
#' Derives emission reductions by subtracting project and leakage emissions from
#' baseline emissions.
#'
#' @param baseline_emissions Baseline emissions in tonnes CO2e.
#' @param project_emissions Project emissions in tonnes CO2e.
#' @param leakage_emissions Leakage emissions in tonnes CO2e. Defaults to zero.
#'
#' @return Numeric vector of emission reductions in tonnes CO2e.
#' @examples
#' calculate_emission_reductions_acm0018(1200, 150, 30)
#' @export
calculate_emission_reductions_acm0018 <- function(baseline_emissions,
                                                  project_emissions,
                                                  leakage_emissions = 0) {
  if (!is.numeric(baseline_emissions)) {
    rlang::abort("`baseline_emissions` must be numeric.")
  }
  if (!is.numeric(project_emissions)) {
    rlang::abort("`project_emissions` must be numeric.")
  }
  if (!is.numeric(leakage_emissions)) {
    rlang::abort("`leakage_emissions` must be numeric.")
  }

  baseline_emissions - project_emissions - leakage_emissions
}
