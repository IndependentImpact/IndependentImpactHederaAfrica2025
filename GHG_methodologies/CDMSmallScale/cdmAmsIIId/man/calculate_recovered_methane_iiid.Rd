% Generated manually because roxygen2 is unavailable
\name{calculate_recovered_methane_iiid}
\alias{calculate_recovered_methane_iiid}
\title{Convert recovered methane volumes into destroyed emissions}
\description{Transform monitored methane recovery volumes into destroyed emissions expressed in tCO2e.}
\usage{
calculate_recovered_methane_iiid(
  data,
  methane_recovered_col = "methane_recovered_m3",
  destruction_efficiency_col = "destruction_efficiency_fraction",
  group_cols = NULL,
  output_col = "recovered_methane_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
  \item{data}{Tibble containing methane recovery monitoring data.}
  \item{methane_recovered_col}{Column storing methane recovered in cubic metres.}
  \item{destruction_efficiency_col}{Column storing the fraction of recovered methane destroyed.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the output column for destroyed methane.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped totals of destroyed methane in tCO2e.}
\examples{
recovery <- tibble::tibble(
  digester_id = c("D1", "D2"),
  methane_recovered_m3 = c(15000, 12000),
  destruction_efficiency_fraction = c(0.99, 0.98)
)
calculate_recovered_methane_iiid(recovery, group_cols = "digester_id")
}
