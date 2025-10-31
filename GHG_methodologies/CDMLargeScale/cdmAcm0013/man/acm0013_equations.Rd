% Generated manually to mirror roxygen documentation
\name{acm0013_equations}
\alias{calculate_baseline_emissions_acm0013}
\alias{calculate_project_emissions_acm0013}
\alias{calculate_leakage_emissions_acm0013}
\alias{calculate_net_emission_reductions_acm0013}
\title{ACM0013 emission calculation helpers}
\description{Helper functions to compute baseline, project, leakage, and net emission reductions for ACM0013 projects.}
\usage{
calculate_baseline_emissions_acm0013(electricity_generated_mwh, baseline_emission_factor_tco2_per_mwh)

calculate_project_emissions_acm0013(fuel_consumed, fuel_emission_factor_tco2_per_unit, auxiliary_electricity_mwh = 0,
  auxiliary_emission_factor_tco2_per_mwh = 0, other_project_emissions_tco2e = 0)

calculate_leakage_emissions_acm0013(upstream_fuel_emissions_tco2e = 0, displaced_unit_emissions_tco2e = 0,
  other_leakage_tco2e = 0)

calculate_net_emission_reductions_acm0013(baseline_emissions_tco2e, project_emissions_tco2e, leakage_emissions_tco2e)
}
\arguments{
  \item{electricity_generated_mwh}{Electricity delivered to the grid under the baseline scenario in MWh.}
  \item{baseline_emission_factor_tco2_per_mwh}{Combined margin baseline emission factor in tCO2 per MWh.}
  \item{fuel_consumed}{Monitored fossil fuel consumption in the units that match the emission factor.}
  \item{fuel_emission_factor_tco2_per_unit}{Fuel emission factor in tCO2 per fuel unit.}
  \item{auxiliary_electricity_mwh}{Auxiliary electricity imported from the grid in MWh.}
  \item{auxiliary_emission_factor_tco2_per_mwh}{Grid emission factor applied to auxiliary electricity in tCO2 per MWh.}
  \item{other_project_emissions_tco2e}{Additional project emissions already expressed in tonnes CO2e.}
  \item{upstream_fuel_emissions_tco2e}{Upstream fuel supply chain emissions attributable to the project.}
  \item{displaced_unit_emissions_tco2e}{Emissions associated with the displaced unit attributable to the project.}
  \item{other_leakage_tco2e}{Other leakage sources in tonnes CO2e.}
  \item{baseline_emissions_tco2e}{Baseline emissions in tonnes CO2e.}
  \item{project_emissions_tco2e}{Project emissions in tonnes CO2e.}
  \item{leakage_emissions_tco2e}{Leakage emissions in tonnes CO2e.}
}
\value{Numeric vectors of emissions in tonnes CO2e.}
\examples{
calculate_baseline_emissions_acm0013(40000, 0.85)
calculate_project_emissions_acm0013(1e5, 7.5e-5, auxiliary_electricity_mwh = 500,
  auxiliary_emission_factor_tco2_per_mwh = 0.9, other_project_emissions_tco2e = 20)
calculate_leakage_emissions_acm0013(5, 10, 2)
calculate_net_emission_reductions_acm0013(50000, 42000, 1200)
}
