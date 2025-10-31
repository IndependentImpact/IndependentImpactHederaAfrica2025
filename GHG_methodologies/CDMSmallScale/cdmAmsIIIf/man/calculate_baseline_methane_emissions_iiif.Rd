% Generated manually for cdmAmsIIIf
\name{calculate_baseline_methane_emissions_iiif}
\alias{calculate_baseline_methane_emissions_iiif}
\title{Calculate baseline methane emissions for compostable waste}
\usage{
calculate_baseline_methane_emissions_iiif(
  data,
  waste_quantity_col = "waste_tonnes",
  doc_fraction_col = "doc_fraction",
  docf_col = "docf_fraction",
  methane_correction_factor_col = "baseline_mcf_fraction",
  methane_fraction = 0.5,
  oxidation_factor_col = NULL,
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e",
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing baseline waste characterisation data.}
  \item{waste_quantity_col}{Column storing the quantity of waste subject to the
  project activity (tonnes).}
  \item{doc_fraction_col}{Column storing the degradable organic carbon fraction
  (tonne C per tonne of waste).}
  \item{docf_col}{Column storing the fraction of DOC that degrades.}
  \item{methane_correction_factor_col}{Column storing the baseline methane
  correction factor (dimensionless).}
  \item{methane_fraction}{Methane fraction of landfill gas.}
  \item{oxidation_factor_col}{Optional column storing the oxidation factor.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column containing baseline emissions in
  tCO2e.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped baseline methane emissions in tCO2e.}
\description{
Equation (1) of AMS-III.F multiplies waste quantities, degradable organic carbon
parameters, and methane correction factors to estimate baseline methane
emissions that would occur in unmanaged disposal sites.
}
\examples{
library(tibble)
baseline <- tibble(
  site_id = c("A", "B"),
  waste_tonnes = c(1000, 850),
  doc_fraction = c(0.15, 0.16),
  docf_fraction = 0.5,
  baseline_mcf_fraction = c(0.8, 0.7)
)
calculate_baseline_methane_emissions_iiif(baseline)
}
