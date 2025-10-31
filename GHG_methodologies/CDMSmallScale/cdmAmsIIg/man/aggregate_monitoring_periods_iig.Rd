% Generated manually for cdmAmsIIg
\name{aggregate_monitoring_periods_iig}
\alias{aggregate_monitoring_periods_iig}
\title{Aggregate monitoring periods for AMS-II.G}
\description{Summarises monitoring observations across reporting periods, producing baseline and project inputs that can be passed to \code{estimate_emission_reductions_ams_iig()}.}
\usage{
aggregate_monitoring_periods_iig(
  monitoring_data,
  group_cols = "site_id",
  period_cols = c("year", "month"),
  baseline_consumption_col = "baseline_biomass_consumption_tonnes",
  baseline_fraction_col = "baseline_non_renewable_fraction",
  baseline_ncv_col = "baseline_net_calorific_value_mj_per_tonne",
  baseline_emission_factor_col = "baseline_emission_factor_tco2_per_mj",
  project_consumption_col = "project_biomass_consumption_tonnes",
  project_fraction_col = "project_non_renewable_fraction",
  project_ncv_col = "project_net_calorific_value_mj_per_tonne",
  project_emission_factor_col = "project_emission_factor_tco2_per_mj",
  leakage_col = "leakage_emissions_tco2e"
)
}
\arguments{
\item{monitoring_data}{Tibble containing monitoring period observations.}
\item{group_cols}{Character vector of entity identifiers (e.g., household IDs).}
\item{period_cols}{Character vector defining the monitoring period (e.g., year, month).}
\item{baseline_consumption_col}{Column storing baseline biomass consumption.}
\item{baseline_fraction_col}{Column storing the baseline non-renewable fraction.}
\item{baseline_ncv_col}{Column storing the baseline NCV in MJ per tonne.}
\item{baseline_emission_factor_col}{Column storing the baseline emission factor in tCO2e/MJ.}
\item{project_consumption_col}{Column storing project biomass consumption.}
\item{project_fraction_col}{Column storing the project non-renewable fraction.}
\item{project_ncv_col}{Column storing the project NCV in MJ per tonne.}
\item{project_emission_factor_col}{Column storing the project emission factor in tCO2e/MJ.}
\item{leakage_col}{Column storing leakage emissions in tCO2e.}
}
\value{A tibble aggregated by \code{group_cols} and \code{period_cols} with baseline and project monitoring inputs.}
\examples{
data <- simulate_ams_iig_dataset(n_sites = 2, n_periods = 2, seed = 42)
aggregate_monitoring_periods_iig(data)
}
