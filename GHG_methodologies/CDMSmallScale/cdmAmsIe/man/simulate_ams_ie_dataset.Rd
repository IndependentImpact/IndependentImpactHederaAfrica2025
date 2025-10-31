% Generated manually
\name{simulate_ams_ie_dataset}
\alias{simulate_ams_ie_dataset}
\title{Simulate an AMS-I.E monitoring dataset}
\description{Generate a tidy tibble with synthetic monitoring data for AMS-I.E activities.}
\usage{
simulate_ams_ie_dataset(n_users = 3, n_periods = 4, start_year = 2024, seed = NULL)
}
\arguments{
  \item{n_users}{Number of unique users or project sites.}
  \item{n_periods}{Number of monitoring periods per user.}
  \item{start_year}{Starting calendar year for the simulation.}
  \item{seed}{Optional random seed for reproducibility.}
}
\value{A tibble containing user identifiers, monitoring periods, biomass consumption, non-renewable fractions, emission factors, and project energy data.}
\examples{
simulate_ams_ie_dataset(n_users = 2, n_periods = 3, seed = 42)
}
