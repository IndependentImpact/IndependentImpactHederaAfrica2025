% Generated manually
\name{simulate_acm0001_dataset}
\alias{simulate_acm0001_dataset}
\title{Simulate ACM0001 monitoring data}
\usage{simulate_acm0001_dataset(n_periods = 12, seed = NULL)}
\arguments{
  \item{n_periods}{Number of monitoring periods to simulate.}
  \item{seed}{Optional integer seed for reproducibility.}
}
\value{A tibble containing synthetic ACM0001 monitoring records, including oxidation fractions and methane destruction parameters.}
\description{Generates synthetic landfill gas monitoring data consistent with ACM0001 inputs, including methane oxidation fractions and supporting parameters for methane destruction calculations.}
\examples{
simulate_acm0001_dataset(2, seed = 42)
}
