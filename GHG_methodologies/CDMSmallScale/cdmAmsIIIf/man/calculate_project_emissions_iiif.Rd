% Generated manually for cdmAmsIIIf
\name{calculate_project_emissions_iiif}
\alias{calculate_project_emissions_iiif}
\title{Calculate project emissions for aerobic composting systems}
\usage{
calculate_project_emissions_iiif(
  data,
  composted_waste_col = "composted_waste_tonnes",
  doc_fraction_col = "doc_fraction",
  docf_col = "docf_fraction",
  compost_mcf_col = "compost_mcf_fraction",
  methane_fraction = 0.5,
  compost_oxidation_col = NULL,
  electricity_consumption_col = "electricity_mwh",
  electricity_emission_factor_col = "grid_ef_tco2_per_mwh",
  fossil_fuel_consumption_col = "diesel_litres",
  fossil_fuel_emission_factor_col = "diesel_ef_tco2_per_litre",
  group_cols = NULL,
  output_col = "project_emissions_tco2e",
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing composting operation data.}
  \item{composted_waste_col}{Column storing the amount of waste composted
  (tonnes).}
  \item{doc_fraction_col}{Column storing the degradable organic carbon
  fraction.}
  \item{docf_col}{Column storing the fraction of DOC that degrades.}
  \item{compost_mcf_col}{Column storing the methane correction factor achieved by
  the composting system.}
  \item{methane_fraction}{Methane fraction of landfill gas.}
  \item{compost_oxidation_col}{Optional column storing the oxidation factor for
  the compost matrix.}
  \item{electricity_consumption_col}{Optional column storing electricity use
  (MWh).}
  \item{electricity_emission_factor_col}{Optional column storing the grid
  emission factor (tCO2e per MWh).}
  \item{fossil_fuel_consumption_col}{Optional column storing fossil fuel
  consumption.}
  \item{fossil_fuel_emission_factor_col}{Optional column storing emission factors
  for fossil fuel inputs.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column containing project emissions in
  tCO2e.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped project emissions in tCO2e.}
\description{
Equation (2) of AMS-III.F quantifies residual methane from the composting
process and adds fossil energy use required to operate the system.
}
\examples{
project <- tibble::tibble(
  site_id = c("A", "B"),
  composted_waste_tonnes = c(950, 800),
  doc_fraction = c(0.15, 0.16),
  docf_fraction = 0.5,
  compost_mcf_fraction = c(0.1, 0.12),
  electricity_mwh = c(40, 32),
  grid_ef_tco2_per_mwh = 0.6
)
calculate_project_emissions_iiif(project)
}
