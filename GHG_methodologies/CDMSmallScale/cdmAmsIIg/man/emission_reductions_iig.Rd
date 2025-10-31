% Generated manually for cdmAmsIIg
\name{calculate_leakage_emissions_iig}
\alias{calculate_leakage_emissions_iig}
\alias{calculate_emission_reductions_iig}
\title{Leakage and emission reductions under AMS-II.G}
\description{Aggregates leakage emissions associated with AMS-II.G projects and combines baseline, project, and leakage totals to obtain net emission reductions.}
\usage{
calculate_leakage_emissions_iig(
  leakage_data = NULL,
  leakage_col = "leakage_emissions_tco2e",
  group_cols = NULL,
  output_col = "leakage_emissions_tco2e"
)

calculate_emission_reductions_iig(
  baseline_emissions,
  project_emissions,
  leakage_emissions = NULL,
  group_cols = NULL,
  output_col = "emission_reductions_tco2e"
)
}
\arguments{
\item{leakage_data}{Optional tibble containing leakage emission estimates.}
\item{leakage_col}{Column storing leakage emissions in tCO2e.}
\item{group_cols}{Optional character vector of grouping columns.}
\item{output_col}{Name of the output column.}
\item{baseline_emissions}{Tibble containing baseline emission totals with a column named \code{baseline_emissions_tco2e}.}
\item{project_emissions}{Tibble containing project emission totals with a column named \code{project_emissions_tco2e}.}
\item{leakage_emissions}{Optional tibble containing leakage totals with a column named \code{leakage_emissions_tco2e}.}
}
\value{A tibble with leakage emissions or net emission reductions by grouping structure.}
\examples{
baseline <- tibble::tibble(site_id = "A", baseline_emissions_tco2e = 1.2)
project <- tibble::tibble(site_id = "A", project_emissions_tco2e = 0.4)
leakage <- tibble::tibble(site_id = "A", leakage_emissions_tco2e = 0.05)
calculate_emission_reductions_iig(baseline, project, leakage, group_cols = "site_id")
}
