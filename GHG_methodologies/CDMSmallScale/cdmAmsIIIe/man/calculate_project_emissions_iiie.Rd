% Generated manually for cdmAmsIIIe
\name{calculate_project_emissions_iiie}
\alias{calculate_project_emissions_iiie}
\title{Calculate project emissions for AMS-III.E biomass treatment}
\usage{
calculate_project_emissions_iiie(
  data,
  treated_biomass_col = "treated_biomass_tonnes",
  methane_potential_col = "methane_potential_m3_per_tonne",
  methane_slip_fraction_col = "methane_slip_fraction",
  days_col = "days_in_period",
  auxiliary_fuel_consumption_col = "auxiliary_fuel_consumption_tj",
  auxiliary_fuel_emission_factor_col = "auxiliary_fuel_ef_tco2_per_tj",
  fossil_backup_consumption_col = NULL,
  fossil_backup_ef_col = NULL,
  group_cols = NULL,
  output_col = "project_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing project operation data.}
  \item{treated_biomass_col}{Column storing biomass treated in the project
  scenario (tonnes).}
  \item{methane_potential_col}{Column storing methane generation potential (m3
  CH4 per tonne).}
  \item{methane_slip_fraction_col}{Column storing the fraction of methane not
  destroyed by the technology.}
  \item{days_col}{Column storing the number of days represented by each record.}
  \item{auxiliary_fuel_consumption_col}{Column storing auxiliary fuel
  consumption (TJ).}
  \item{auxiliary_fuel_emission_factor_col}{Emission factor for the auxiliary
  fuel (tCO2/TJ).}
  \item{fossil_backup_consumption_col}{Optional column storing backup fossil fuel
  consumption (TJ).}
  \item{fossil_backup_ef_col}{Optional column storing emission factors for the
  backup fuel (tCO2/TJ).}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column with project emissions in tCO2e.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped project emissions.}
\description{
Convert methane slip, auxiliary fuel, and backup fuel use into project emissions
expressed in tCO2e.
}
\examples{
project <- tibble::tibble(
  plant_id = c("A", "B"),
  treated_biomass_tonnes = c(1100, 880),
  methane_potential_m3_per_tonne = c(120, 110),
  methane_slip_fraction = c(0.05, 0.04),
  auxiliary_fuel_consumption_tj = c(1.2, 0.9),
  auxiliary_fuel_ef_tco2_per_tj = 74,
  days_in_period = 365
)
calculate_project_emissions_iiie(project)
}
