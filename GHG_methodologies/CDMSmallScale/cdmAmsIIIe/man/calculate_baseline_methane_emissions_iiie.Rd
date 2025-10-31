% Generated manually for cdmAmsIIIe
\name{calculate_baseline_methane_emissions_iiie}
\alias{calculate_baseline_methane_emissions_iiie}
\title{Calculate baseline methane emissions from unmanaged biomass decay}
\usage{
calculate_baseline_methane_emissions_iiie(
  data,
  biomass_col = "biomass_tonnes",
  methane_potential_col = "methane_potential_m3_per_tonne",
  decay_fraction_col = "anaerobic_decay_fraction",
  days_col = "days_in_period",
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing baseline monitoring data.}
  \item{biomass_col}{Column storing biomass quantity (tonnes).}
  \item{methane_potential_col}{Column storing methane generation potential (m3
  CH4 per tonne).}
  \item{decay_fraction_col}{Column storing the fraction of biomass expected to
  decay anaerobically.}
  \item{days_col}{Column storing the number of days represented by each record.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column with baseline emissions in tCO2e.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped baseline methane emissions.}
\description{
Multiply biomass quantities, methane potential, anaerobic decay fractions, and
monitoring duration to estimate baseline methane emissions in tCO2e.
}
\examples{
library(tibble)
baseline <- tibble(
  plant_id = c("A", "B"),
  biomass_tonnes = c(1200, 900),
  methane_potential_m3_per_tonne = c(120, 110),
  anaerobic_decay_fraction = c(0.65, 0.55),
  days_in_period = 365
)
calculate_baseline_methane_emissions_iiie(baseline)
}
