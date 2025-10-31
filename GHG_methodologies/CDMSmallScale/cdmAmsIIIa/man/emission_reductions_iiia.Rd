% Generated manually
\name{emission_reductions_iiia}
\alias{calculate_emission_reductions_iiia}
\title{Calculate AMS-III.A emission reductions}
\description{
Combines baseline, project, and leakage emissions to determine AMS-III.A emission reductions.
}
\usage{
calculate_emission_reductions_iiia(baseline_emissions, project_emissions, leakage_emissions = NULL, group_cols = NULL)
}
\arguments{
\item{baseline_emissions}{Tibble produced by \code{calculate_baseline_fertilizer_emissions_iiia()}.}

\item{project_emissions}{Tibble containing project fertilizer and inoculant emissions.}

\item{leakage_emissions}{Optional tibble produced by \code{calculate_leakage_emissions_iiia()}.}

\item{group_cols}{Optional character vector identifying grouping columns.}
}
\value{
Tibble containing baseline emissions, project emissions, leakage, and emission reductions.
}
\examples{
baseline <- tibble::tibble(baseline_emissions_tco2e = 5)
project <- tibble::tibble(
  project_fertilizer_emissions_tco2e = 0.5,
  project_inoculant_emissions_tco2e = 0.2
)
calculate_emission_reductions_iiia(baseline, project)
}
