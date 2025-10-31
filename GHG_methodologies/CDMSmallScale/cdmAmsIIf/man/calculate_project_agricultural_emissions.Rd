\name{calculate_project_agricultural_emissions}
\alias{calculate_project_agricultural_emissions}
\title{Calculate project agricultural emissions (Equation 2)}
\usage{
calculate_project_agricultural_emissions(project_data,
                                                     fuel_energy_col = "project_fuel_energy_gj",
                                                     fuel_emission_factor_col = "project_fuel_emission_factor_tco2_per_gj",
                                                     electricity_col = "project_electricity_mwh",
                                                     electricity_emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
                                                     group_cols = NULL,
                                                     output_col = "project_emissions_tco2e")
}
\arguments{
  \item{project_data}{Tidy tibble describing project energy use for the}
  \item{fuel_energy_col}{Column containing project fuel energy in GJ.}
  \item{fuel_emission_factor_col}{Column storing project fuel emission factors}
  \item{electricity_col}{Column containing project electricity consumption in}
  \item{electricity_emission_factor_col}{Column storing project electricity}
  \item{group_cols}{Optional grouping columns for the output.}
  \item{output_col}{Name of the column that will contain project emissions in}
}
\value{A tibble containing project emissions aggregated by the requested}
\description{Implements Equation (2) of AMS-II.F by converting monitored project fuel and
electricity consumption into emissions. Inputs mirror
[calculate_baseline_agricultural_emissions()] to streamline workflows.
  agricultural facility.
  in tCO2e per GJ.
  MWh.
  emission factors in tCO2e per MWh.
  tCO2e. Defaults to `"project_emissions_tco2e"`.
  grouping structure.}
\examples{
project <- tibble::tibble(
  facility_id = c("rice_mill_1", "rice_mill_2"),
  project_fuel_energy_gj = c(520, 640),
  project_fuel_emission_factor_tco2_per_gj = c(0.028, 0.031),
  project_electricity_mwh = c(145, 163),
  project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.58)
)
calculate_project_agricultural_emissions(project, group_cols = "facility_id")
}
