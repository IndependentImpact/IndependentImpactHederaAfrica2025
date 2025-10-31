\name{simulate_acm0012_dataset}
\alias{simulate_acm0012_dataset}
\title{Simulate ACM0012 monitoring data}
\description{
Generate synthetic monitoring data for ACM0012 waste energy recovery projects
using \pkg{DeclareDesign}.
}
\usage{
simulate_acm0012_dataset(n_periods = 12, observations_per_period = 30, seed = NULL)
}
\arguments{
  \item{n_periods}{Number of monitoring periods to simulate.}
  \item{observations_per_period}{Number of observations per period.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{
A tibble containing simulated monitoring records.
}
\examples{
simulate_acm0012_dataset(2, observations_per_period = 5, seed = 2024)
}
