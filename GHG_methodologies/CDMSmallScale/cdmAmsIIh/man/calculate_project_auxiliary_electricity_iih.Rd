% Generated manually
\name{calculate_project_auxiliary_electricity_iih}
\alias{calculate_project_auxiliary_electricity_iih}
\title{Calculate project auxiliary electricity emissions}
\description{
Equation (3) of AMS-II.H converts auxiliary electricity consumption for pumps, controls, and distribution equipment into greenhouse gas emissions using grid emission factors. When no auxiliary electricity is consumed the helper returns zeros.
}
\usage{
calculate_project_auxiliary_electricity_iih(
  project_data,
  electricity_consumption_col = "project_auxiliary_electricity_mwh",
  emission_factor_col = "project_electricity_emission_factor_tco2_per_mwh",
  group_cols = NULL,
  output_col = "project_auxiliary_emissions_tco2e"
)
}
\arguments{
  \item{project_data}{A tibble containing monitored auxiliary electricity use.}
  \item{electricity_consumption_col}{Column storing auxiliary electricity consumption (MWh). Set to \code{NULL} to return zeros.}
  \item{emission_factor_col}{Column storing grid emission factors (tCO2e/MWh). Required when \code{electricity_consumption_col} is supplied.}
  \item{group_cols}{Optional character vector of grouping columns.}
  \item{output_col}{Name of the emissions column to create.}
}
\value{
A tibble with grouped auxiliary electricity emissions in tCO2e.
}
\examples{
monitoring <- tibble::tibble(
  facility = c("Plant_A", "Plant_B"),
  project_auxiliary_electricity_mwh = c(240, 180),
  project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.55)
)
calculate_project_auxiliary_electricity_iih(monitoring, group_cols = "facility")
}
