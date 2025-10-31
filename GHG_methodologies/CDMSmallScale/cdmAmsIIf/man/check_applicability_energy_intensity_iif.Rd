\name{check_applicability_energy_intensity_iif}
\alias{check_applicability_energy_intensity_iif}
\title{Check AMS-II.F energy intensity criterion}
\usage{
check_applicability_energy_intensity_iif(baseline_data,
                                                     project_data,
                                                     energy_col = "baseline_total_energy_mwh",
                                                     project_energy_col = "project_total_energy_mwh",
                                                     output_col = "service_level_indicator",
                                                     tolerance = 0.1)
}
\arguments{
  \item{baseline_data}{Tibble containing baseline energy and output indicators.}
  \item{project_data}{Tibble containing project energy and output indicators.}
  \item{energy_col}{Baseline column storing total energy consumption (MWh).}
  \item{project_energy_col}{Project column storing total energy consumption (MWh).}
  \item{output_col}{Column storing the output proxy (e.g. tonnes processed). Must}
  \item{tolerance}{Minimum fractional reduction in specific energy consumption}
}
\value{A logical scalar indicating whether the specific energy reduction meets}
\description{AMS-II.F requires that project activities deliver measurable reductions in
specific energy consumption for the agricultural service or product. This
helper compares baseline and project total energy use relative to an output
proxy (e.g. tonnes of grain processed, litres of milk cooled, or area irrigated)
and verifies that the fractional reduction exceeds a configurable tolerance.
  be present in both datasets.
  required to satisfy the criterion. Defaults to 10%.
  the tolerance threshold.}
\examples{
baseline <- tibble::tibble(total_energy_mwh = c(220, 195), output_proxy = c(420, 380))
project <- tibble::tibble(total_energy_mwh = c(150, 138), output_proxy = c(420, 380))
check_applicability_energy_intensity_iif(baseline, project,
  energy_col = "total_energy_mwh",
  project_energy_col = "total_energy_mwh",
  output_col = "output_proxy"
)
}
