% Generated manually
\name{estimate_emission_reductions_acm0001}
\alias{estimate_emission_reductions_acm0001}
\title{Estimate ACM0001 emission reductions}
\usage{estimate_emission_reductions_acm0001(monitoring_data)}
\arguments{
  \item{monitoring_data}{Landfill gas monitoring dataset with ACM0001-required columns.}
}
\value{A tibble of total methane generation, captured methane, baseline, project, leakage, methane destruction, and emission reductions.}
\description{Aggregates monitoring data to compute total emissions accounting for ACM0001 landfill gas projects, including methane destruction reported in tonnes and tonnes CO2e.}
\examples{
data <- simulate_acm0001_dataset(4, seed = 99)
estimate_emission_reductions_acm0001(data)
}
