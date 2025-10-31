% Generated manually
\name{simulate_acm0019_dataset}
\alias{simulate_acm0019_dataset}
\title{Simulate ACM0019 monitoring data}
\description{Generates tidy monitoring datasets consistent with ACM0019 nitric acid projects, including production, emission factors, and auxiliary energy use.}
\usage{
simulate_acm0019_dataset(periods = 6, observations_per_period = 30, seed = NULL)
}
\arguments{
\item{periods}{Number of monitoring periods to simulate.}

\item{observations_per_period}{Number of observations per period.}

\item{seed}{Optional seed for reproducibility.}
}
\value{A tibble containing simulated monitoring observations.}
\examples{
simulate_acm0019_dataset(periods = 3, observations_per_period = 5, seed = 42)
}
