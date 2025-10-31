% Generated manually for cdmAmsIIIe
\name{simulate_ams_iiie_dataset}
\alias{simulate_ams_iiie_dataset}
\title{Simulate monitoring data for AMS-III.E projects}
\usage{
simulate_ams_iiie_dataset(n_plants = 3, n_periods = 4, seed = NULL)
}
\arguments{
  \item{n_plants}{Number of facilities to simulate.}
  \item{n_periods}{Number of monitoring periods per facility.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{List containing tibbles \code{baseline}, \code{project}, \code{leakage}, and \code{applicability}.}
\description{
Generate synthetic datasets compatible with the AMS-III.E helpers for demos,
examples, and tests.
}
\examples{
simulate_ams_iiie_dataset(n_plants = 2, n_periods = 3, seed = 123)
}
