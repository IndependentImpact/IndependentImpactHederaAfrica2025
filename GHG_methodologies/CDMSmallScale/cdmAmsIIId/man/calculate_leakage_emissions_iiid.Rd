% Generated manually because roxygen2 is unavailable
\name{calculate_leakage_emissions_iiid}
\alias{calculate_leakage_emissions_iiid}
\title{Aggregate leakage emissions for AMS-III.D}
\description{Summarise leakage emissions linked to manure management projects.}
\usage{
calculate_leakage_emissions_iiid(
  data,
  leakage_col = "leakage_emissions_tco2e",
  group_cols = NULL,
  output_col = "leakage_emissions_tco2e"
)
}
\arguments{
  \item{data}{Tibble containing leakage observations.}
  \item{leakage_col}{Column storing leakage emissions in tCO2e.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output leakage column.}
}
\value{Tibble containing grouped leakage emissions.}
\examples{
leakage <- tibble::tibble(
  farm_id = c("A", "B"),
  leakage_emissions_tco2e = c(5, 3)
)
calculate_leakage_emissions_iiid(leakage)
}
