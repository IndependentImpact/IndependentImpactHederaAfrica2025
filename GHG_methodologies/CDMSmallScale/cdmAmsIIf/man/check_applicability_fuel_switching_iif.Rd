\name{check_applicability_fuel_switching_iif}
\alias{check_applicability_fuel_switching_iif}
\title{Check AMS-II.F fuel switching criterion}
\usage{
check_applicability_fuel_switching_iif(baseline_data,
                                                   project_data,
                                                   energy_col = "baseline_thermal_energy_gj",
                                                   emission_factor_col = "baseline_thermal_emission_factor_tco2_per_gj",
                                                   project_energy_col = "project_thermal_energy_gj",
                                                   project_emission_factor_col = "project_thermal_emission_factor_tco2_per_gj")
}
\arguments{
  \item{baseline_data}{Tibble containing baseline thermal energy and emission}
  \item{project_data}{Tibble containing project thermal energy and emission}
  \item{energy_col}{Baseline column storing thermal energy demand (GJ).}
  \item{emission_factor_col}{Baseline column storing emission factors (tCO2e/GJ).}
  \item{project_energy_col}{Project column storing thermal energy demand (GJ).}
  \item{project_emission_factor_col}{Project column storing emission factors (tCO2e/GJ).}
}
\value{Logical scalar indicating whether the project fuel mix is lower in}
\description{Projects that replace high-carbon fuels with lower-emission alternatives must
demonstrate that the weighted emission factor of the project fuel mix is below
the baseline mix. This helper evaluates weighted averages using thermal energy
demand as weights.
  factors.
  factors.
  emissions than the baseline mix.}
\examples{
baseline <- tibble::tibble(thermal_energy_gj = c(320, 180), emission_factor_tco2_per_gj = c(0.07, 0.072))
project <- tibble::tibble(thermal_energy_gj = c(280, 120), emission_factor_tco2_per_gj = c(0.045, 0.05))
check_applicability_fuel_switching_iif(baseline, project,
  energy_col = "thermal_energy_gj",
  emission_factor_col = "emission_factor_tco2_per_gj",
  project_energy_col = "thermal_energy_gj",
  project_emission_factor_col = "emission_factor_tco2_per_gj"
)
}
