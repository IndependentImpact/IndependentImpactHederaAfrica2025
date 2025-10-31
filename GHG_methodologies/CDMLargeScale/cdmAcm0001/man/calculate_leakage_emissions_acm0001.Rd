% Generated manually
\name{calculate_leakage_emissions_acm0001}
\alias{calculate_leakage_emissions_acm0001}
\title{Calculate ACM0001 leakage emissions}
\usage{calculate_leakage_emissions_acm0001(leakage_fraction, methane_captured_m3, methane_density_t_per_m3 = 0.000716, gwp_ch4 = 28)}
\arguments{
  \item{leakage_fraction}{Fraction of captured methane attributed to leakage pathways.}
  \item{methane_captured_m3}{Methane captured in cubic metres.}
  \item{methane_density_t_per_m3}{Methane density in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential for methane.}
}
\value{Numeric vector of leakage emissions in tonnes CO2e.}
\description{Estimates leakage associated with landfill gas projects under ACM0001.}
