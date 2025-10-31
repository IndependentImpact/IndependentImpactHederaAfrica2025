% Generated manually
\name{ams_ie_equations}
\alias{calculate_non_renewable_biomass}
\alias{calculate_baseline_energy_content}
\alias{calculate_baseline_emissions}
\alias{calculate_project_emissions}
\alias{calculate_emission_reductions}
\title{AMS-I.E equation helpers}
\description{Helper functions that translate the AMS-I.E numbered equations into reproducible R code.}
\usage{
calculate_non_renewable_biomass(biomass_data, consumption_col = "biomass_consumption_tonnes",
  fraction = 1, fraction_col = "non_renewable_fraction", group_cols = NULL,
  output_col = "non_renewable_biomass_tonnes")

calculate_baseline_energy_content(non_renewable_biomass,
  biomass_col = "non_renewable_biomass_tonnes", ncv,
  output_col = "baseline_energy_mj")

calculate_baseline_emissions(energy_data, energy_col = "baseline_energy_mj",
  emission_factor, output_col = "baseline_emissions_tco2e")

calculate_project_emissions(project_energy, energy_col = "project_energy_mj",
  project_emission_factor = 0, output_col = "project_emissions_tco2e")

calculate_emission_reductions(baseline_emissions, project_emissions,
  baseline_col = "baseline_emissions_tco2e",
  project_col = "project_emissions_tco2e",
  output_col = "emission_reductions_tco2e")
}
\arguments{
  \item{biomass_data}{Tibble containing biomass consumption observations.}
  \item{consumption_col}{Column storing baseline biomass consumption.}
  \item{fraction}{Scalar fraction of biomass that is non-renewable when \code{fraction_col} is \code{NULL}.}
  \item{fraction_col}{Optional column with the fraction of biomass that is non-renewable.}
  \item{group_cols}{Grouping columns used to aggregate results.}
  \item{output_col}{Name of the resulting column returned by the helper.}
  \item{non_renewable_biomass}{Tibble produced by \code{calculate_non_renewable_biomass()}.}
  \item{biomass_col}{Column containing non-renewable biomass values.}
  \item{ncv}{Net calorific value in MJ per unit of biomass.}
  \item{energy_data}{Tibble with baseline thermal energy.}
  \item{energy_col}{Column containing energy in MJ.}
  \item{emission_factor}{Baseline emission factor in tCO2e/MJ.}
  \item{project_energy}{Tibble containing project fossil energy.}
  \item{project_emission_factor}{Project emission factor in tCO2e/MJ.}
  \item{baseline_emissions}{Tibble with baseline emissions.}
  \item{project_emissions}{Tibble with project emissions.}
  \item{baseline_col}{Column name for baseline emissions.}
  \item{project_col}{Column name for project emissions.}
}
\value{A tibble with the requested quantities for each equation.}
\examples{
biomass <- tibble::tibble(
  site = c("A", "B"),
  biomass_consumption_tonnes = c(10, 12),
  non_renewable_fraction = c(0.8, 0.9)
)
non_renewable <- calculate_non_renewable_biomass(
  biomass,
  consumption_col = "biomass_consumption_tonnes",
  fraction_col = "non_renewable_fraction",
  group_cols = "site"
)
energy <- calculate_baseline_energy_content(non_renewable, ncv = 15)
emissions <- calculate_baseline_emissions(energy, emission_factor = 0.0001)
project <- calculate_project_emissions(tibble::tibble(project_energy_mj = c(100, 120)), 0.00009)
calculate_emission_reductions(emissions, project)
}
