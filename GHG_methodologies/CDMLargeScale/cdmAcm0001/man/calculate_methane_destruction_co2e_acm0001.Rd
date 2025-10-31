% Generated manually
\name{calculate_methane_destruction_co2e_acm0001}
\alias{calculate_methane_destruction_co2e_acm0001}
\title{Express methane destruction as CO2e under ACM0001}
\usage{calculate_methane_destruction_co2e_acm0001(
  methane_captured_m3,
  destruction_efficiency,
  methane_density_t_per_m3 = 0.000716,
  gwp_ch4 = 28
)}
\arguments{
  \item{methane_captured_m3}{Methane captured by the project in cubic metres.}
  \item{destruction_efficiency}{Fraction of captured methane destroyed.}
  \item{methane_density_t_per_m3}{Methane density in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Numeric vector of methane destruction expressed in tonnes CO2e.}
\description{
Combines methane mass destroyed with the methane global warming potential to
return greenhouse gas benefits of ACM0001 destruction activities in tonnes CO2e.
}
\examples{
calculate_methane_destruction_co2e_acm0001(4000, 0.9)
}
