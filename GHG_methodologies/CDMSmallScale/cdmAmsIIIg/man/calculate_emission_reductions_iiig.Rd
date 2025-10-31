% Generated manually for cdmAmsIIIg
\name{calculate_emission_reductions_iiig}
\alias{calculate_emission_reductions_iiig}
\title{Calculate emission reductions for AMS-III.G}
\description{
Combine baseline, project, and optional leakage emissions into net emission
reductions while preserving grouping columns when provided.
}
\usage{
calculate_emission_reductions_iiig(
  baseline_emissions,
  project_emissions,
  leakage_emissions = NULL,
  group_cols = NULL
)
}
\arguments{
\item{baseline_emissions}{Tibble produced by
\code{calculate_baseline_methane_emissions_iiig}.}

\item{project_emissions}{Tibble produced by
\code{calculate_project_emissions_iiig}.}

\item{leakage_emissions}{Optional tibble produced by
\code{calculate_leakage_emissions_iiig}.}

\item{group_cols}{Optional character vector identifying grouping columns.}
}
\value{
Tibble containing baseline, project, leakage, and net emission reductions.
}
\examples{
baseline <- tibble::tibble(baseline_emissions_tco2e = 1800)
project <- tibble::tibble(project_emissions_tco2e = 420)
calculate_emission_reductions_iiig(baseline, project)
}
