% Generated manually
\name{calculate_methane_destroyed_acm0001}
\alias{calculate_methane_destroyed_acm0001}
\title{Calculate methane destroyed under ACM0001}
\usage{calculate_methane_destroyed_acm0001(
  methane_captured_m3,
  destruction_efficiency,
  methane_density_t_per_m3 = 0.000716
)}
\arguments{
  \item{methane_captured_m3}{Methane captured by the project in cubic metres.}
  \item{destruction_efficiency}{Fraction of captured methane destroyed.}
  \item{methane_density_t_per_m3}{Methane density in tonnes per cubic metre.}
}
\value{Numeric vector of methane mass destroyed in tonnes.}
\description{
Converts captured methane volumes and the destruction efficiency of flares or
utilisation equipment into methane mass destroyed under ACM0001.
}
\examples{
calculate_methane_destroyed_acm0001(4000, 0.9)
}
