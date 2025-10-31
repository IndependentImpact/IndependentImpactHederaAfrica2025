% Generated manually
\name{calculate_emission_reductions_iih}
\alias{calculate_emission_reductions_iih}
\title{Aggregate emission reductions for AMS-II.H}
\description{
Equation (4) of AMS-II.H determines emission reductions by subtracting project and leakage emissions from baseline totals. This helper accepts pre-computed components and performs the subtraction consistently across grouped datasets.
}
\usage{
calculate_emission_reductions_iih(
  baseline_emissions,
  project_emissions,
  leakage_emissions = NULL,
  group_cols = NULL,
  output_col = "emission_reductions_tco2e"
)
}
\arguments{
  \item{baseline_emissions}{Tibble containing baseline emissions with one row per group.}
  \item{project_emissions}{Tibble containing total project emissions (central plus auxiliary) with one row per group.}
  \item{leakage_emissions}{Optional tibble containing leakage emissions in tCO2e.}
  \item{group_cols}{Optional character vector of grouping columns shared across the inputs.}
  \item{output_col}{Name of the emission reduction column to create.}
}
\value{
A tibble containing net emission reductions in tCO2e.
}
\examples{
baseline <- tibble::tibble(line = c("A", "B"), baseline_emissions_tco2e = c(12, 9))
project <- tibble::tibble(line = c("A", "B"), project_emissions_tco2e = c(6.4, 5.1))
leakage <- tibble::tibble(line = c("A", "B"), leakage_emissions_tco2e = c(0.2, 0.1))
calculate_emission_reductions_iih(baseline, project, leakage, group_cols = "line")
}
