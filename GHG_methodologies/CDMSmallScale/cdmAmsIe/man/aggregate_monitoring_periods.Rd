% Generated manually
\name{aggregate_monitoring_periods}
\alias{aggregate_monitoring_periods}
\title{Aggregate monitoring periods for AMS-I.E}
\description{Summarise monitoring data across reporting periods using the AMS-I.E calculation helpers.}
\usage{
aggregate_monitoring_periods(biomass_data, monitoring_cols = c("year", "month"),
  group_cols = "user_id", consumption_col = "biomass_consumption_tonnes",
  fraction_col = "non_renewable_fraction", ncv_col = "net_calorific_value",
  emission_factor_col = "emission_factor", project_energy_col = "project_energy_mj",
  project_emission_factor_col = NULL, project_emission_factor = 0)
}
\arguments{
  \item{biomass_data}{Tibble containing monitoring observations.}
  \item{monitoring_cols}{Columns defining a monitoring period.}
  \item{group_cols}{Columns identifying entities.}
  \item{consumption_col}{Column storing biomass consumption.}
  \item{fraction_col}{Column storing the non-renewable fraction.}
  \item{ncv_col}{Column storing the net calorific value in MJ per unit biomass.}
  \item{emission_factor_col}{Column containing the baseline emission factor.}
  \item{project_energy_col}{Column containing project fossil energy in MJ.}
  \item{project_emission_factor_col}{Optional column containing project emission factors in tCO2e/MJ.}
  \item{project_emission_factor}{Default project emission factor when a dedicated column is not supplied.}
}
\value{A tibble with non-renewable biomass, baseline energy, baseline emissions, project emissions, and emission reductions for each entity and monitoring period.}
\examples{
monitoring <- simulate_ams_ie_dataset(n_users = 2, n_periods = 3, seed = 123)
aggregate_monitoring_periods(monitoring)
}
