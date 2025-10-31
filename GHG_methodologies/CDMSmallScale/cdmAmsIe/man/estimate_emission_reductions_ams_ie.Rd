% Generated manually
\name{estimate_emission_reductions_ams_ie}
\alias{estimate_emission_reductions_ams_ie}
\title{Estimate emission reductions under AMS-I.E}
\description{Chains the AMS-I.E equation helpers to translate monitoring data into emission reductions.}
\usage{
estimate_emission_reductions_ams_ie(biomass_data,
  consumption_col = "biomass_consumption_tonnes",
  fraction_col = "non_renewable_fraction", fraction = 1, ncv,
  group_cols = NULL, emission_factor, project_energy_col = NULL,
  project_emission_factor = 0)
}
\arguments{
  \item{biomass_data}{Tibble containing monitoring observations.}
  \item{consumption_col}{Column storing biomass consumption.}
  \item{fraction_col}{Column storing the non-renewable fraction.}
  \item{fraction}{Scalar non-renewable fraction used when \code{fraction_col} is \code{NULL}.}
  \item{ncv}{Net calorific value in MJ per unit biomass.}
  \item{group_cols}{Grouping columns identifying entities.}
  \item{emission_factor}{Baseline emission factor in tCO2e/MJ.}
  \item{project_energy_col}{Optional column storing project fossil energy in MJ.}
  \item{project_emission_factor}{Project emission factor in tCO2e/MJ.}
}
\value{A tibble containing baseline energy, baseline emissions, project emissions, and emission reductions.}
\examples{
monitoring <- tibble::tibble(
  site_id = c("A", "A", "B"),
  biomass_consumption_tonnes = c(10, 6, 12),
  non_renewable_fraction = c(0.8, 0.75, 0.9),
  project_energy_mj = c(100, 120, 140)
)
estimate_emission_reductions_ams_ie(
  monitoring,
  group_cols = "site_id",
  ncv = 15,
  emission_factor = 0.0001,
  project_energy_col = "project_energy_mj",
  project_emission_factor = 0.00009
)
}
