% Generated manually for cdmAmsIIg
\name{calculate_emissions_from_energy_iig}
\alias{calculate_emissions_from_energy_iig}
\title{Convert thermal energy into emissions under AMS-II.G}
\description{Multiplies thermal energy demand by the appropriate emission factor to obtain baseline or project emissions for AMS-II.G projects. Emission factors are assumed to be constant within each grouping structure.}
\usage{
calculate_emissions_from_energy_iig(
  energy_data,
  energy_col = "baseline_thermal_energy_mj",
  factor_data,
  emission_factor_col = "baseline_emission_factor_tco2_per_mj",
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e"
)
}
\arguments{
\item{energy_data}{Tibble containing thermal energy results.}
\item{energy_col}{Column storing thermal energy in MJ.}
\item{factor_data}{Tibble containing emission factors.}
\item{emission_factor_col}{Column storing emission factors in tCO2e/MJ.}
\item{group_cols}{Optional character vector of grouping columns.}
\item{output_col}{Name of the column storing emissions in tonnes of CO2e.}
}
\value{A tibble containing emission totals by grouping structure.}
\examples{
energy <- tibble::tibble(
  site_id = c("A", "B"),
  baseline_thermal_energy_mj = c(180, 210)
)
factors <- tibble::tibble(
  site_id = c("A", "B"),
  baseline_emission_factor_tco2_per_mj = c(0.00009, 0.000095)
)
calculate_emissions_from_energy_iig(
  energy_data = energy,
  factor_data = factors,
  group_cols = "site_id"
)
}
