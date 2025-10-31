\name{calculate_emission_reductions_iif}
\alias{calculate_emission_reductions_iif}
\title{Calculate emission reductions for AMS-II.F (Equation 4)}
\usage{
calculate_emission_reductions_iif(baseline_emissions,
                                              project_emissions,
                                              leakage_emissions = NULL,
                                              baseline_col = "baseline_emissions_tco2e",
                                              project_col = "project_emissions_tco2e",
                                              leakage_col = "leakage_emissions_tco2e",
                                              output_col = "emission_reductions_tco2e",
                                              group_cols = NULL)
}
\arguments{
  \item{baseline_emissions}{Tibble containing baseline emissions, typically the}
  \item{project_emissions}{Tibble containing project emissions, typically the}
  \item{leakage_emissions}{Optional tibble containing leakage emissions from}
  \item{baseline_col}{Column storing baseline emissions (tCO2e).}
  \item{project_col}{Column storing project emissions (tCO2e).}
  \item{leakage_col}{Column storing leakage emissions (tCO2e).}
  \item{output_col}{Column storing emission reductions (tCO2e).}
  \item{group_cols}{Optional character vector of grouping columns shared across}
}
\value{A tibble with emission reductions aggregated by the requested}
\description{Implements Equation (4) of AMS-II.F by subtracting project and leakage
emissions from baseline emissions to estimate net emission reductions.
  output of [calculate_baseline_agricultural_emissions()].
  output of [calculate_project_agricultural_emissions()].
  [calculate_leakage_emissions_iif()].
  inputs. When `NULL`, no join is attempted.
  grouping structure.}
\examples{
baseline <- tibble::tibble(facility_id = "rice_mill_1", baseline_emissions_tco2e = 420)
project <- tibble::tibble(facility_id = "rice_mill_1", project_emissions_tco2e = 180)
leakage <- tibble::tibble(facility_id = "rice_mill_1", leakage_emissions_tco2e = 6)
calculate_emission_reductions_iif(baseline, project, leakage, group_cols = "facility_id")
}
