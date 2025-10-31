% Generated manually for cdmAmsIIIg
\name{calculate_project_emissions_iiig}
\alias{calculate_project_emissions_iiig}
\title{Calculate project methane emissions for AMS-III.G}
\description{
Convert project monitoring data into methane emissions that remain after
collection, destruction, and oxidation while including optional energy inputs
for landfill gas management systems.
}
\usage{
calculate_project_emissions_iiig(
  data,
  waste_disposed_col = "waste_disposed_tonnes",
  methane_generation_potential_col = "methane_generation_potential_m3_per_tonne",
  project_collection_efficiency_col = "project_collection_efficiency_fraction",
  destruction_efficiency_col = "destruction_efficiency_fraction",
  project_oxidation_fraction_col = "oxidation_fraction",
  days_col = NULL,
  electricity_consumption_col = "electricity_consumption_mwh",
  electricity_emission_factor_col = "electricity_ef_tco2_per_mwh",
  diesel_consumption_col = "diesel_consumption_litres",
  diesel_emission_factor_col = "diesel_ef_tco2_per_litre",
  group_cols = NULL,
  output_col = "project_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
\item{data}{Tibble containing project monitoring data.}

\item{waste_disposed_col}{Column storing waste disposed in tonnes.}

\item{methane_generation_potential_col}{Column storing methane generation
potential in cubic metres per tonne of waste.}

\item{project_collection_efficiency_col}{Column storing the project methane
collection efficiency.}

\item{destruction_efficiency_col}{Column storing flare or utilisation
system destruction efficiency.}

\item{project_oxidation_fraction_col}{Optional column storing the oxidation
fraction for uncaptured methane.}

\item{days_col}{Optional column storing the number of days represented by
each record.}

\item{electricity_consumption_col}{Optional column storing electricity use
in MWh.}

\item{electricity_emission_factor_col}{Optional column storing grid emission
factors in tCO2 per MWh.}

\item{diesel_consumption_col}{Optional column storing diesel use in litres.}

\item{diesel_emission_factor_col}{Optional column storing diesel emission
factors in tCO2 per litre.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the output column for project emissions in
tCO2e.}

\item{methane_density_t_per_m3}{Density of methane in tonnes per cubic
metre.}

\item{gwp_ch4}{Global warming potential of methane.}
}
\value{
Tibble containing grouped project emissions in tCO2e.
}
\examples{
project <- tibble::tibble(
  site_id = c("LF1", "LF2"),
  waste_disposed_tonnes = c(12000, 9000),
  methane_generation_potential_m3_per_tonne = c(90, 85),
  project_collection_efficiency_fraction = c(0.65, 0.6),
  destruction_efficiency_fraction = c(0.98, 0.96),
  oxidation_fraction = c(0.1, 0.08)
)
calculate_project_emissions_iiig(project, group_cols = "site_id")
}
