% Generated manually for cdmAmsIIg
\name{estimate_emission_reductions_ams_iig}
\alias{estimate_emission_reductions_ams_iig}
\title{Run the AMS-II.G emission reduction workflow}
\description{Chains the AMS-II.G equation helpers to compute baseline emissions, project emissions, leakage, and net emission reductions from tidy monitoring datasets.}
\usage{
estimate_emission_reductions_ams_iig(
  baseline_data,
  project_data,
  leakage_data = NULL,
  group_cols = NULL,
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
\item{baseline_data}{Tibble containing baseline monitoring observations.}
\item{project_data}{Tibble containing project monitoring observations.}
\item{leakage_data}{Optional tibble containing leakage estimates.}
\item{group_cols}{Optional character vector of grouping columns common to the supplied datasets.}
\item{baseline_consumption_col}{Column storing baseline biomass consumption (tonnes).}
\item{baseline_fraction_col}{Column storing the baseline non-renewable fraction.}
\item{baseline_ncv_col}{Column storing the baseline net calorific value in MJ per tonne.}
\item{baseline_emission_factor_col}{Column storing the baseline emission factor in tCO2e/MJ.}
\item{project_consumption_col}{Column storing project biomass consumption (tonnes).}
\item{project_fraction_col}{Column storing the project non-renewable fraction.}
\item{project_ncv_col}{Column storing the project net calorific value in MJ per tonne.}
\item{project_emission_factor_col}{Column storing the project emission factor in tCO2e/MJ.}
\item{leakage_col}{Column storing leakage emissions in tCO2e when \code{leakage_data} is supplied.}
}
\value{A tibble containing baseline and project non-renewable biomass, thermal energy, emissions, leakage, and net emission reductions by the requested grouping structure.}
\examples{
baseline <- tibble::tibble(
  site_id = c("A", "B"),
  baseline_biomass_consumption_tonnes = c(12, 14),
  baseline_non_renewable_fraction = c(0.82, 0.88),
  baseline_net_calorific_value_mj_per_tonne = c(15.2, 14.8),
  baseline_emission_factor_tco2_per_mj = c(0.00009, 0.000092)
)
project <- tibble::tibble(
  site_id = c("A", "B"),
  project_biomass_consumption_tonnes = c(7.4, 8.0),
  project_non_renewable_fraction = c(0.41, 0.39),
  project_net_calorific_value_mj_per_tonne = c(15.4, 15.0),
  project_emission_factor_tco2_per_mj = c(0.00009, 0.000092)
)
estimate_emission_reductions_ams_iig(baseline, project, group_cols = "site_id")
}
