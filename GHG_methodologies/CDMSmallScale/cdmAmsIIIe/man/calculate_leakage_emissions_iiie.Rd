% Generated manually for cdmAmsIIIe
\name{calculate_leakage_emissions_iiie}
\alias{calculate_leakage_emissions_iiie}
\title{Calculate leakage emissions for AMS-III.E}
\usage{
calculate_leakage_emissions_iiie(
  data,
  biomass_transported_col = "biomass_transported_tonnes",
  transport_distance_col = "transport_distance_km",
  transport_emission_factor_col = "transport_ef_tco2_per_tkm",
  alternative_use_fraction_col = "alternative_use_fraction",
  alternative_use_emission_factor_col = "alternative_use_ef_tco2_per_tonne",
  group_cols = NULL,
  output_col = "leakage_emissions_tco2e"
)
}
\arguments{
  \item{data}{Tibble containing leakage monitoring data.}
  \item{biomass_transported_col}{Column storing biomass transported (tonnes).}
  \item{transport_distance_col}{Column storing transport distance (km).}
  \item{transport_emission_factor_col}{Column storing transport emission factors
  (tCO2e per tonne-km).}
  \item{alternative_use_fraction_col}{Column storing the fraction of biomass
  diverted from alternative uses.}
  \item{alternative_use_emission_factor_col}{Emission factor associated with the
  alternative use (tCO2e per tonne).}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column with leakage emissions in tCO2e.}
}
\value{Tibble containing grouped leakage emissions.}
\description{
Aggregate leakage arising from biomass transport and displacement of alternative
biomass uses.
}
\examples{
leakage <- tibble::tibble(
  plant_id = c("A", "B"),
  biomass_transported_tonnes = c(500, 420),
  transport_distance_km = c(45, 30),
  transport_ef_tco2_per_tkm = 0.00012,
  alternative_use_fraction = c(0.1, 0.05),
  alternative_use_ef_tco2_per_tonne = 0.5
)
calculate_leakage_emissions_iiie(leakage)
}
