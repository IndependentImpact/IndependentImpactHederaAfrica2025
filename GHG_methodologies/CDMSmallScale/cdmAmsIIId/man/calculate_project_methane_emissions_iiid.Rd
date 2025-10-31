% Generated manually because roxygen2 is unavailable
\name{calculate_project_methane_emissions_iiid}
\alias{calculate_project_methane_emissions_iiid}
\title{Calculate project methane emissions after recovery and destruction}
\description{Estimate methane emissions under project conditions by subtracting captured and destroyed methane from project generation totals.}
\usage{
calculate_project_methane_emissions_iiid(
  data,
  volatile_solids_col = "volatile_solids_kg_per_day",
  methane_potential_col = "methane_potential_m3_per_kg_vs",
  project_methane_conversion_factor_col = "project_mcf_fraction",
  capture_efficiency_col = "capture_efficiency_fraction",
  destruction_efficiency_col = "destruction_efficiency_fraction",
  days_col = "days_in_period",
  group_cols = NULL,
  output_col = "project_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing project monitoring data.}
  \item{volatile_solids_col}{Column storing volatile solids generated per day (kg VS/day).}
  \item{methane_potential_col}{Column storing methane producing potential (m\eqn{^3} CH\eqn{_4}/kg VS).}
  \item{project_methane_conversion_factor_col}{Column storing the project methane conversion factor (fraction).}
  \item{capture_efficiency_col}{Column storing the fraction of methane captured by the recovery system.}
  \item{destruction_efficiency_col}{Column storing the fraction of captured methane destroyed.}
  \item{days_col}{Column storing the number of days represented by each record.}
  \item{group_cols}{Optional character vector of grouping columns.}
  \item{output_col}{Name of the output column for project emissions.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped project methane emissions in tCO2e.}
\examples{
project <- tibble::tibble(
  farm_id = c("A", "B"),
  volatile_solids_kg_per_day = c(45, 30),
  methane_potential_m3_per_kg_vs = 0.25,
  project_mcf_fraction = c(0.65, 0.6),
  capture_efficiency_fraction = c(0.85, 0.8),
  destruction_efficiency_fraction = c(0.98, 0.95),
  days_in_period = 365
)
calculate_project_methane_emissions_iiid(project)
}
