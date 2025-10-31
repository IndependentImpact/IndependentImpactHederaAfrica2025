% Generated manually for cdmAmsIIIf
\name{calculate_emission_reductions_iiif}
\alias{calculate_emission_reductions_iiif}
\title{Calculate emission reductions for AMS-III.F}
\usage{
calculate_emission_reductions_iiif(
  baseline_emissions,
  project_emissions,
  leakage_emissions = NULL,
  group_cols = NULL
)
}
\arguments{
  \item{baseline_emissions}{Tibble produced by
  \code{calculate_baseline_methane_emissions_iiif}.}
  \item{project_emissions}{Tibble produced by
  \code{calculate_project_emissions_iiif}.}
  \item{leakage_emissions}{Optional tibble produced by
  \code{calculate_leakage_emissions_iiif}.}
  \item{group_cols}{Optional character vector identifying grouping columns.}
}
\value{Tibble containing baseline, project, leakage, and net emission reductions.}
\description{
Combine baseline, project, and leakage emissions to estimate net emission
reductions consistent with AMS-III.F.
}
\examples{
baseline <- tibble::tibble(baseline_emissions_tco2e = 25)
project <- tibble::tibble(project_emissions_tco2e = 6)
calculate_emission_reductions_iiif(baseline, project)
}
