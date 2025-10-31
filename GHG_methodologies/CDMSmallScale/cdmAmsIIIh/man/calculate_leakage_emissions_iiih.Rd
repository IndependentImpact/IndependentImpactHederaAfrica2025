\name{calculate_leakage_emissions_iiih}
\alias{calculate_leakage_emissions_iiih}
\title{Calculate leakage emissions for AMS-III.H projects}
\usage{
calculate_leakage_emissions_iiih(
  data,
  sludge_mass_col = "sludge_tonnes",
  transport_distance_col = "transport_distance_km",
  transport_emission_factor_col = "transport_ef_tco2_per_tkm",
  sludge_treatment_emission_factor_col = "sludge_treatment_ef_tco2_per_tonne",
  chemical_dosage_col = "chemical_usage_tonnes",
  chemical_emission_factor_col = "chemical_ef_tco2_per_tonne",
  displaced_fossil_fuel_col = "displaced_fossil_fuel_gj",
  displaced_fossil_fuel_emission_factor_col = "displaced_fossil_fuel_ef_tco2_per_gj",
  group_cols = NULL,
  output_col = "leakage_emissions_tco2e"
)
}
\arguments{
\item{data}{Tibble containing leakage monitoring data.}

\item{sludge_mass_col}{Column storing sludge transported off-site in tonnes.}

\item{transport_distance_col}{Column storing transport distance in kilometres.}

\item{transport_emission_factor_col}{Column storing emission factors in tCO2 per tonne-kilometre.}

\item{sludge_treatment_emission_factor_col}{Optional column storing emission factors for sludge treatment.}

\item{chemical_dosage_col}{Optional column storing chemical usage in tonnes.}

\item{chemical_emission_factor_col}{Optional column storing emission factors in tCO2 per tonne of chemical.}

\item{displaced_fossil_fuel_col}{Optional column storing fossil fuel displaced by biogas utilisation in GJ.}

\item{displaced_fossil_fuel_emission_factor_col}{Optional column storing emission factors in tCO2 per GJ (credited as negative leakage).}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the output column for leakage emissions in tCO2e.}
}
\value{Tibble containing grouped leakage emissions in tCO2e.}
\description{
Aggregates transport, treatment, chemical usage, and displacement credits to
produce net leakage estimates for AMS-III.H.
}
\examples{
leakage <- tibble::tibble(
  site_id = c("WW1", "WW2"),
  sludge_tonnes = c(450, 320),
  transport_distance_km = c(25, 40),
  transport_ef_tco2_per_tkm = c(0.0001, 0.00012)
)
calculate_leakage_emissions_iiih(leakage, group_cols = "site_id")
}
