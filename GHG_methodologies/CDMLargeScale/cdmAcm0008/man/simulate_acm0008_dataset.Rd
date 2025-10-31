% Generated manually
\name{simulate_acm0008_dataset}
\alias{simulate_acm0008_dataset}
\title{Simulate monitoring data for ACM0008}
\description{Generates tidy monitoring records representing coal mine methane recovery projects with methane flow, concentration, oxidation, and auxiliary electricity data.}
\usage{
simulate_acm0008_dataset(periods = 6, observations_per_period = 30, seed = NULL)
}
\arguments{
\item{periods}{Number of monitoring periods to simulate.}
\item{observations_per_period}{Number of observations per monitoring period.}
\item{seed}{Optional random seed for reproducibility.}
}
\value{A tibble containing simulated monitoring observations.}
\examples{
simulate_acm0008_dataset(2, observations_per_period = 10, seed = 100)
}
