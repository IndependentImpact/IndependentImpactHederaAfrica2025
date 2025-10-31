% Generated manually for cdmAmsIIg
\name{check_applicability_efficiency_improvement_iig}
\alias{check_applicability_efficiency_improvement_iig}
\alias{check_applicability_fraction_bounds_iig}
\alias{check_applicability_monitoring_iig}
\title{Applicability checks for AMS-II.G}
\description{Helpers that evaluate key AMS-II.G applicability conditions covering efficiency improvements, non-renewable fraction bounds, and monitoring completeness.}
\usage{
check_applicability_efficiency_improvement_iig(
  baseline_data,
  project_data,
  consumption_col = "baseline_biomass_consumption_tonnes",
  fraction_col = "baseline_non_renewable_fraction",
  project_consumption_col = "project_biomass_consumption_tonnes",
  project_fraction_col = "project_non_renewable_fraction",
  tolerance = 0.2
)

check_applicability_fraction_bounds_iig(
  data,
  fraction_cols = c("baseline_non_renewable_fraction", "project_non_renewable_fraction")
)

check_applicability_monitoring_iig(
  monitoring_data,
  required_cols = c(
    "baseline_biomass_consumption_tonnes",
    "baseline_non_renewable_fraction",
    "baseline_net_calorific_value_mj_per_tonne",
    "baseline_emission_factor_tco2_per_mj",
    "project_biomass_consumption_tonnes",
    "project_non_renewable_fraction",
    "project_net_calorific_value_mj_per_tonne",
    "project_emission_factor_tco2_per_mj"
  )
)
}
\arguments{
\item{baseline_data}{Tibble containing baseline biomass consumption and non-renewable fractions.}
\item{project_data}{Tibble containing project biomass consumption and non-renewable fractions.}
\item{consumption_col}{Column storing baseline biomass consumption.}
\item{fraction_col}{Column storing the baseline non-renewable fraction.}
\item{project_consumption_col}{Column storing project biomass consumption.}
\item{project_fraction_col}{Column storing the project non-renewable fraction.}
\item{tolerance}{Minimum fractional reduction in non-renewable biomass consumption required for applicability.}
\item{data}{Tibble containing non-renewable fraction columns.}
\item{fraction_cols}{Character vector of fraction column names that must lie within [0, 1].}
\item{monitoring_data}{Tibble containing monitoring observations.}
\item{required_cols}{Character vector of required monitoring columns.}
}
\value{Logical scalars indicating whether each applicability condition is satisfied.}
\examples{
baseline <- tibble::tibble(
  baseline_biomass_consumption_tonnes = c(12, 14),
  baseline_non_renewable_fraction = c(0.85, 0.88)
)
project <- tibble::tibble(
  project_biomass_consumption_tonnes = c(7.2, 7.9),
  project_non_renewable_fraction = c(0.4, 0.38)
)
check_applicability_efficiency_improvement_iig(baseline, project)

monitoring <- tibble::tibble(
  baseline_biomass_consumption_tonnes = 12,
  baseline_non_renewable_fraction = 0.85,
  baseline_net_calorific_value_mj_per_tonne = 15.2,
  baseline_emission_factor_tco2_per_mj = 0.00009,
  project_biomass_consumption_tonnes = 7.4,
  project_non_renewable_fraction = 0.4,
  project_net_calorific_value_mj_per_tonne = 15.4,
  project_emission_factor_tco2_per_mj = 0.00009
)
check_applicability_monitoring_iig(monitoring)
}
