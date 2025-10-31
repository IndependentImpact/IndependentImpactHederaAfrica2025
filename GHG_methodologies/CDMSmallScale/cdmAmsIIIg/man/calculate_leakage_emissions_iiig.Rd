% Generated manually for cdmAmsIIIg
\name{calculate_leakage_emissions_iiig}
\alias{calculate_leakage_emissions_iiig}
\title{Calculate leakage emissions for AMS-III.G}
\description{
Aggregate transport, residual waste treatment, and displacement impacts to
produce a leakage adjustment consistent with AMS-III.G monitoring guidance.
}
\usage{
calculate_leakage_emissions_iiig(
  data,
  waste_transported_col = "waste_transported_tonnes",
  transport_distance_col = "transport_distance_km",
  transport_emission_factor_col = "transport_ef_tco2_per_tkm",
  residual_waste_col = "residual_waste_tonnes",
  residual_waste_emission_factor_col = "residual_waste_ef_tco2_per_tonne",
  displaced_electricity_col = "displaced_electricity_mwh",
  displaced_electricity_emission_factor_col = "displaced_electricity_ef_tco2_per_mwh",
  group_cols = NULL,
  output_col = "leakage_emissions_tco2e"
)
}
\arguments{
\item{data}{Tibble containing leakage monitoring data.}

\item{waste_transported_col}{Column storing transported waste in tonnes.}

\item{transport_distance_col}{Column storing average transport distance in
kilometres.}

\item{transport_emission_factor_col}{Column storing transport emission
factors in tCO2 per tonne-kilometre.}

\item{residual_waste_col}{Optional column storing residual waste in tonnes.}

\item{residual_waste_emission_factor_col}{Optional column storing residual
waste emission factors in tCO2 per tonne.}

\item{displaced_electricity_col}{Optional column storing displaced electricity
in MWh.}

\item{displaced_electricity_emission_factor_col}{Optional column storing
emission factors for displaced electricity in tCO2 per MWh.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the output column for leakage emissions in
tCO2e.}
}
\value{
Tibble containing grouped leakage emissions in tCO2e.
}
\examples{
leakage <- tibble::tibble(
  site_id = c("LF1", "LF2"),
  waste_transported_tonnes = c(500, 450),
  transport_distance_km = c(20, 35),
  transport_ef_tco2_per_tkm = c(0.00012, 0.0001)
)
calculate_leakage_emissions_iiig(leakage, group_cols = "site_id")
}
