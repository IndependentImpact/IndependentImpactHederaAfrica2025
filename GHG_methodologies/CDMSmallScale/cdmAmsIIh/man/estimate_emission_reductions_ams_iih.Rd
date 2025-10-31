% Generated manually
\name{estimate_emission_reductions_ams_iih}
\alias{estimate_emission_reductions_ams_iih}
\title{Run the AMS-II.H emission reduction workflow}
\description{
Chains the AMS-II.H equation helpers to convert decentralized baseline and centralized project monitoring data into net emission reductions. Baseline and project datasets describe fuel use, emission factors, and optional useful output alongside auxiliary electricity and leakage estimates.
}
\usage{
estimate_emission_reductions_ams_iih(
  baseline_data,
  project_data,
  leakage_data = NULL,
  group_cols = NULL,
  baseline_fuel_consumption_col = "baseline_fuel_use_gj",
  baseline_emission_factor_col = "baseline_emission_factor_tco2_per_gj",
  baseline_useful_output_col = "baseline_useful_output_gj",
  project_fuel_consumption_col = "project_fuel_use_gj",
  project_emission_factor_col = "project_emission_factor_tco2_per_gj",
  project_useful_output_col = "project_useful_output_gj",
  project_aux_electricity_col = "project_auxiliary_electricity_mwh",
  project_electricity_emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
  leakage_col = "leakage_emissions_tco2e"
)
}
\arguments{
  \item{baseline_data}{Tibble containing baseline monitoring observations.}
  \item{project_data}{Tibble containing project monitoring observations.}
  \item{leakage_data}{Optional tibble containing leakage emissions in tCO2e.}
  \item{group_cols}{Optional character vector of grouping columns shared across the inputs.}
  \item{baseline_fuel_consumption_col}{Column storing baseline fuel use (GJ).}
  \item{baseline_emission_factor_col}{Column storing baseline emission factors (tCO2e/GJ).}
  \item{baseline_useful_output_col}{Optional column storing baseline useful output (GJ).}
  \item{project_fuel_consumption_col}{Column storing project fuel use (GJ).}
  \item{project_emission_factor_col}{Column storing project emission factors (tCO2e/GJ).}
  \item{project_useful_output_col}{Optional column storing project useful output (GJ).}
  \item{project_aux_electricity_col}{Optional column storing project auxiliary electricity use (MWh).}
  \item{project_electricity_emission_factor_col}{Column storing emission factors for auxiliary electricity (tCO2e/MWh).}
  \item{leakage_col}{Column storing leakage emissions (tCO2e) when \code{leakage_data} is supplied.}
}
\value{
A tibble containing baseline emissions, project central and auxiliary emissions, leakage, specific energy diagnostics (when available), and net emission reductions.
}
\examples{
baseline <- tibble::tibble(
  facility = c("Line_A", "Line_B"),
  baseline_fuel_use_gj = c(4200, 3150),
  baseline_emission_factor_tco2_per_gj = 0.071,
  baseline_useful_output_gj = c(3600, 2700)
)
project <- tibble::tibble(
  facility = c("Line_A", "Line_B"),
  project_fuel_use_gj = c(3000, 2250),
  project_emission_factor_tco2_per_gj = 0.068,
  project_useful_output_gj = c(2880, 2160),
  project_auxiliary_electricity_mwh = c(220, 165),
  project_electricity_emission_factor_tco2_per_mwh = 0.62
)
estimate_emission_reductions_ams_iih(
  baseline,
  project,
  group_cols = "facility"
)
}
