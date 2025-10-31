% Generated manually because roxygen2 is unavailable
\name{calculate_emission_reductions_iiid}
\alias{calculate_emission_reductions_iiid}
\title{Calculate net emission reductions for AMS-III.D}
\description{Combine baseline, project, recovered, and leakage emission totals to obtain net emission reductions in tCO2e.}
\usage{
calculate_emission_reductions_iiid(
  baseline,
  project,
  recovered,
  leakage,
  group_cols = NULL,
  output_col = "emission_reductions_tco2e"
)
}
\arguments{
  \item{baseline}{Tibble returned by \code{calculate_baseline_methane_emissions_iiid()}.}
  \item{project}{Tibble returned by \code{calculate_project_methane_emissions_iiid()}.}
  \item{recovered}{Tibble returned by \code{calculate_recovered_methane_iiid()}.}
  \item{leakage}{Tibble returned by \code{calculate_leakage_emissions_iiid()}.}
  \item{group_cols}{Character vector specifying grouping columns.}
  \item{output_col}{Name of the emission reduction output column.}
}
\value{Tibble containing emission reductions per group.}
\examples{
baseline <- tibble::tibble(farm_id = "A", baseline_emissions_tco2e = 100)
project <- tibble::tibble(farm_id = "A", project_emissions_tco2e = 20)
recovered <- tibble::tibble(farm_id = "A", recovered_methane_tco2e = 60)
leakage <- tibble::tibble(farm_id = "A", leakage_emissions_tco2e = 5)
calculate_emission_reductions_iiid(baseline, project, recovered, leakage, group_cols = "farm_id")
}
