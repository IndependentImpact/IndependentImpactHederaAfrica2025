\name{calculate_emission_reductions_iiih}
\alias{calculate_emission_reductions_iiih}
\title{Calculate emission reductions for AMS-III.H}
\usage{
calculate_emission_reductions_iiih(
  baseline_emissions,
  project_emissions,
  leakage_emissions = NULL,
  group_cols = NULL
)
}
\arguments{
\item{baseline_emissions}{Tibble produced by \code{calculate_baseline_methane_emissions_iiih}.}

\item{project_emissions}{Tibble produced by \code{calculate_project_emissions_iiih}.}

\item{leakage_emissions}{Optional tibble produced by \code{calculate_leakage_emissions_iiih}.}

\item{group_cols}{Optional character vector identifying grouping columns.}
}
\value{Tibble containing baseline, project, leakage, and net emission reductions.}
\description{
Combines baseline, project, and optional leakage emissions to estimate net
reductions for AMS-III.H projects.
}
\examples{
baseline <- tibble::tibble(baseline_emissions_tco2e = 1800)
project <- tibble::tibble(project_emissions_tco2e = 420)
calculate_emission_reductions_iiih(baseline, project)
}
