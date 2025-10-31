% Generated manually
\name{calculate_project_central_emissions_iih}
\alias{calculate_project_central_emissions_iih}
\title{Calculate project centralized utility emissions}
\description{
Equation (2) of AMS-II.H quantifies greenhouse gas emissions from the centralized utility plant implemented by the project. The helper multiplies monitored fuel use by emission factors and, when useful output is supplied, reports project specific energy consumption.
}
\usage{
calculate_project_central_emissions_iih(
  project_data,
  fuel_consumption_col = "project_fuel_use_gj",
  emission_factor_col = "project_emission_factor_tco2_per_gj",
  useful_output_col = "project_useful_output_gj",
  group_cols = NULL,
  output_col = "project_central_emissions_tco2e",
  specific_energy_col = "project_specific_energy_gj_per_gj"
)
}
\arguments{
  \item{project_data}{A tibble describing the centralized project system.}
  \item{fuel_consumption_col}{Column storing project fuel use in GJ.}
  \item{emission_factor_col}{Column storing project emission factors in tCO2e/GJ.}
  \item{useful_output_col}{Optional column storing useful thermal output (GJ). Set to \code{NULL} to skip specific energy diagnostics.}
  \item{group_cols}{Optional character vector of grouping columns to retain in the result.}
  \item{output_col}{Name of the emissions column to create.}
  \item{specific_energy_col}{Name of the specific energy column created when \code{useful_output_col} is supplied.}
}
\value{
A tibble with grouped project emissions (tCO2e) and, when requested, project specific energy consumption (GJ/GJ).
}
\examples{
project <- tibble::tibble(
  line = c("Header_1", "Header_2"),
  project_fuel_use_gj = c(3000, 2250),
  project_emission_factor_tco2_per_gj = c(0.068, 0.068),
  project_useful_output_gj = c(2880, 2160)
)
calculate_project_central_emissions_iih(project, group_cols = "line")
}
