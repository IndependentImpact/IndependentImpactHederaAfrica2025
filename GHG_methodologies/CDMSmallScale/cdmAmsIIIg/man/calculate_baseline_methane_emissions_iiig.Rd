% Generated manually for cdmAmsIIIg
\name{calculate_baseline_methane_emissions_iiig}
\alias{calculate_baseline_methane_emissions_iiig}
\title{Calculate baseline methane emissions for AMS-III.G}
\description{
Estimate baseline methane emissions released from a landfill in the absence
of gas recovery by combining waste disposal, methane generation potential,
optional baseline recovery, and oxidation fractions.
}
\usage{
calculate_baseline_methane_emissions_iiig(
  data,
  waste_disposed_col = "waste_disposed_tonnes",
  methane_generation_potential_col = "methane_generation_potential_m3_per_tonne",
  baseline_collection_efficiency_col = "baseline_collection_efficiency_fraction",
  oxidation_fraction_col = "oxidation_fraction",
  days_col = NULL,
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
\item{data}{Tibble containing baseline landfill monitoring data.}

\item{waste_disposed_col}{Column storing waste disposed in tonnes.}

\item{methane_generation_potential_col}{Column storing methane generation
potential in cubic metres per tonne of waste.}

\item{baseline_collection_efficiency_col}{Optional column storing the
baseline methane collection efficiency.}

\item{oxidation_fraction_col}{Optional column storing the oxidation
fraction for uncaptured methane.}

\item{days_col}{Optional column storing the number of days represented by
each record.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the output column for baseline emissions in
tCO2e.}

\item{methane_density_t_per_m3}{Density of methane in tonnes per cubic
metre.}

\item{gwp_ch4}{Global warming potential of methane.}
}
\value{
Tibble containing grouped baseline methane emissions in tCO2e.
}
\examples{
library(tibble)
baseline <- tibble(
  site_id = c("LF1", "LF2"),
  waste_disposed_tonnes = c(12000, 9000),
  methane_generation_potential_m3_per_tonne = c(90, 85),
  oxidation_fraction = c(0.1, 0.05)
)
calculate_baseline_methane_emissions_iiig(baseline, group_cols = "site_id")
}
