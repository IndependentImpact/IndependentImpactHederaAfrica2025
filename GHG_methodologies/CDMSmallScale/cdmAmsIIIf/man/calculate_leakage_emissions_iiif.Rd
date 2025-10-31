% Generated manually for cdmAmsIIIf
\name{calculate_leakage_emissions_iiif}
\alias{calculate_leakage_emissions_iiif}
\title{Calculate leakage emissions associated with compost distribution}
\usage{
calculate_leakage_emissions_iiif(
  data,
  compost_transported_col = "compost_transported_tonnes",
  transport_distance_col = "transport_distance_km",
  transport_emission_factor_col = "transport_ef_tco2_per_tkm",
  residual_waste_col = NULL,
  residual_waste_emission_factor_col = NULL,
  displaced_fertiliser_col = NULL,
  displaced_fertiliser_emission_factor_col = NULL,
  group_cols = NULL,
  output_col = "leakage_emissions_tco2e"
)
}
\arguments{
  \item{data}{Tibble containing leakage monitoring data.}
  \item{compost_transported_col}{Column storing the mass of compost transported
  (tonnes).}
  \item{transport_distance_col}{Column storing transport distance (km).}
  \item{transport_emission_factor_col}{Column storing transport emission factors
  (tCO2e per tonne-km).}
  \item{residual_waste_col}{Column storing residual waste requiring
  disposal (tonnes).}
  \item{residual_waste_emission_factor_col}{Optional column storing emission
  factors for residual waste treatment (tCO2e per tonne).}
  \item{displaced_fertiliser_col}{Column storing the amount of synthetic
  fertiliser displaced.}
  \item{displaced_fertiliser_emission_factor_col}{Optional column storing the
  emission factor associated with displaced fertiliser (tCO2e per unit).}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column containing leakage emissions in
  tCO2e.}
}
\value{Tibble containing grouped leakage emissions in tCO2e.}
\description{
Equation (3) of AMS-III.F accounts for transport of compost, residual waste
requiring disposal, and avoided emissions from fertiliser displacement.
}
\examples{
leakage <- tibble::tibble(
  site_id = c("A", "B"),
  compost_transported_tonnes = c(900, 760),
  transport_distance_km = c(25, 30),
  transport_ef_tco2_per_tkm = 0.0001,
  displaced_fertiliser_tonnes = c(50, 45),
  fertiliser_ef_tco2_per_tonne = 0.4
)
calculate_leakage_emissions_iiif(leakage)
}
