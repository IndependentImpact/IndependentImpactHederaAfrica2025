% Generated manually
\name{calculate_baseline_decentralized_emissions_iih}
\alias{calculate_baseline_decentralized_emissions_iih}
\title{Calculate baseline decentralized utility emissions}
\description{
Equation (1) of AMS-II.H consolidates greenhouse gas emissions from decentralized utility generation. The helper multiplies baseline fuel use by emission factors and optionally reports specific energy consumption when useful output data are provided.
}
\usage{
calculate_baseline_decentralized_emissions_iih(
  baseline_data,
  fuel_consumption_col = "baseline_fuel_use_gj",
  emission_factor_col = "baseline_emission_factor_tco2_per_gj",
  useful_output_col = "baseline_useful_output_gj",
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e",
  specific_energy_col = "baseline_specific_energy_gj_per_gj"
)
}
\arguments{
  \item{baseline_data}{A tibble describing the decentralized baseline system.}
  \item{fuel_consumption_col}{Column storing baseline fuel use in GJ.}
  \item{emission_factor_col}{Column storing baseline emission factors in tCO2e/GJ.}
  \item{useful_output_col}{Optional column storing useful thermal output (GJ). Set to \code{NULL} to skip specific energy diagnostics.}
  \item{group_cols}{Optional character vector of grouping columns to retain in the result.}
  \item{output_col}{Name of the emissions column to create.}
  \item{specific_energy_col}{Name of the specific energy column created when \code{useful_output_col} is supplied.}
}
\value{
A tibble with grouped baseline emissions (tCO2e) and, when requested, specific energy consumption (GJ/GJ).
}
\examples{
baseline <- tibble::tibble(
  line = c("Boiler_A", "Boiler_B"),
  baseline_fuel_use_gj = c(4200, 3150),
  baseline_emission_factor_tco2_per_gj = c(0.071, 0.071),
  baseline_useful_output_gj = c(3600, 2700)
)
calculate_baseline_decentralized_emissions_iih(baseline, group_cols = "line")
}
