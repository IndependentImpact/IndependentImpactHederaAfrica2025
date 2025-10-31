\name{calculate_leakage_emissions_iif}
\alias{calculate_leakage_emissions_iif}
\title{Calculate leakage emissions for AMS-II.F (Equation 3)}
\usage{
calculate_leakage_emissions_iif(leakage_data,
                                            leakage_col = "leakage_emissions_tco2e",
                                            group_cols = NULL,
                                            output_col = "leakage_emissions_tco2e")
}
\arguments{
  \item{leakage_data}{Optional tibble describing leakage emissions.}
  \item{leakage_col}{Column containing leakage values in tCO2e.}
  \item{group_cols}{Optional grouping columns matching the baseline/project}
  \item{output_col}{Name of the column that will contain leakage totals.}
}
\value{A tibble with leakage emissions aggregated by the requested grouping}
\description{Sums leakage emissions associated with fossil fuel transport, displaced
biomass usage, or other indirect effects as outlined in AMS-II.F. When the
supplied tibble already reflects aggregated leakage for each grouping
structure, the helper simply renames the column to ensure compatibility with
[calculate_emission_reductions_iif()].
  datasets.
  structure. Returns `NULL` when `leakage_data` is `NULL`.}
\examples{
leakage <- tibble::tibble(facility_id = "rice_mill_1", leakage_emissions_tco2e = 4.2)
calculate_leakage_emissions_iif(leakage, group_cols = "facility_id")
}
