% Generated manually for cdmAmsIIIe
\name{calculate_emission_reductions_iiie}
\alias{calculate_emission_reductions_iiie}
\title{Calculate emission reductions for AMS-III.E}
\usage{
calculate_emission_reductions_iiie(
  baseline_emissions,
  project_emissions,
  leakage_emissions = NULL,
  group_cols = NULL
)
}
\arguments{
  \item{baseline_emissions}{Tibble produced by
  \code{calculate_baseline_methane_emissions_iiie}.}
  \item{project_emissions}{Tibble produced by
  \code{calculate_project_emissions_iiie}.}
  \item{leakage_emissions}{Optional tibble produced by
  \code{calculate_leakage_emissions_iiie}.}
  \item{group_cols}{Optional character vector identifying grouping columns.}
}
\value{Tibble containing baseline, project, leakage, and net emission reductions.}
\description{
Combine baseline, project, and leakage emissions to estimate net emission
reductions for AMS-III.E projects.
}
\examples{
baseline <- tibble::tibble(baseline_emissions_tco2e = 20)
project <- tibble::tibble(project_emissions_tco2e = 5)
calculate_emission_reductions_iiie(baseline, project)
}
