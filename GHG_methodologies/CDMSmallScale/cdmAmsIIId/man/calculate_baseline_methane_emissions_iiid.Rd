% Generated manually because roxygen2 is unavailable
\name{calculate_baseline_methane_emissions_iiid}
\alias{calculate_baseline_methane_emissions_iiid}
\title{Calculate baseline methane emissions from unmanaged manure systems}
\description{Multiply volatile solids generation, methane potential, and baseline methane conversion factors to estimate baseline methane emissions in tCO2e.}
\usage{
calculate_baseline_methane_emissions_iiid(
  data,
  volatile_solids_col = "volatile_solids_kg_per_day",
  methane_potential_col = "methane_potential_m3_per_kg_vs",
  methane_conversion_factor_col = "baseline_mcf_fraction",
  days_col = "days_in_period",
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing baseline monitoring data.}
  \item{volatile_solids_col}{Column storing volatile solids generated per day (kg VS/day).}
  \item{methane_potential_col}{Column storing methane producing potential (m\eqn{^3} CH\eqn{_4}/kg VS).}
  \item{methane_conversion_factor_col}{Column storing the baseline methane conversion factor (fraction).}
  \item{days_col}{Column storing the number of days represented by each record.}
  \item{group_cols}{Optional character vector of grouping columns.}
  \item{output_col}{Name of the output column for baseline emissions.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouping columns (if supplied) and baseline emissions in tCO2e.}
\examples{
library(tibble)
baseline <- tibble(
  farm_id = c("A", "B"),
  volatile_solids_kg_per_day = c(45, 30),
  methane_potential_m3_per_kg_vs = 0.25,
  baseline_mcf_fraction = c(0.8, 0.75),
  days_in_period = 365
)
calculate_baseline_methane_emissions_iiid(baseline)
}
