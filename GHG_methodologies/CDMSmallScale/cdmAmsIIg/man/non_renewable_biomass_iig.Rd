% Generated manually for cdmAmsIIg
\name{calculate_baseline_non_renewable_biomass_iig}
\alias{calculate_baseline_non_renewable_biomass_iig}
\alias{calculate_project_non_renewable_biomass_iig}
\title{Calculate non-renewable biomass under AMS-II.G}
\description{Applies the fraction of non-renewable biomass to monitored baseline or project biomass consumption and aggregates the results by the requested grouping structure. The helpers implement Equation (1) of AMS-II.G for the baseline and an analogous expression for the project scenario.}
\usage{
calculate_baseline_non_renewable_biomass_iig(
  baseline_data,
  consumption_col = "baseline_biomass_consumption_tonnes",
  fraction_col = "baseline_non_renewable_fraction",
  group_cols = NULL,
  output_col = "baseline_non_renewable_biomass_tonnes"
)

calculate_project_non_renewable_biomass_iig(
  project_data,
  project_consumption_col = "project_biomass_consumption_tonnes",
  project_fraction_col = "project_non_renewable_fraction",
  group_cols = NULL,
  output_col = "project_non_renewable_biomass_tonnes"
)
}
\arguments{
\item{baseline_data}{Tibble containing baseline biomass monitoring data.}
\item{consumption_col}{Column storing baseline biomass consumption (tonnes).}
\item{fraction_col}{Column storing the baseline fraction of biomass that is non-renewable.}
\item{group_cols}{Optional character vector of grouping columns used to aggregate results.}
\item{output_col}{Name of the output column storing non-renewable biomass totals.}
\item{project_data}{Tibble containing project biomass monitoring data.}
\item{project_consumption_col}{Column storing project biomass consumption (tonnes).}
\item{project_fraction_col}{Column storing the project fraction of biomass that is non-renewable.}
}
\value{A tibble with one row per grouping structure (or a single aggregated row when \code{group_cols} is \code{NULL}) containing non-renewable biomass totals.}
\examples{
baseline <- tibble::tibble(
  site_id = c("A", "A", "B"),
  baseline_biomass_consumption_tonnes = c(10, 6, 12),
  baseline_non_renewable_fraction = c(0.8, 0.75, 0.9)
)
calculate_baseline_non_renewable_biomass_iig(baseline, group_cols = "site_id")

project <- tibble::tibble(
  site_id = c("A", "B"),
  project_biomass_consumption_tonnes = c(6, 7),
  project_non_renewable_fraction = c(0.4, 0.35)
)
calculate_project_non_renewable_biomass_iig(project, group_cols = "site_id")
}
