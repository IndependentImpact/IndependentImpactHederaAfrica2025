% Generated manually for cdmAmsIIIf
\name{simulate_ams_iiif_dataset}
\alias{simulate_ams_iiif_dataset}
\title{Simulate monitoring data for AMS-III.F projects}
\usage{
simulate_ams_iiif_dataset(n_sites = 3, n_periods = 4, seed = NULL)
}
\arguments{
  \item{n_sites}{Number of composting facilities to simulate.}
  \item{n_periods}{Number of monitoring periods per facility.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{List containing tibbles \code{baseline}, \code{project}, \code{leakage}, and \code{applicability}.}
\description{
Generate synthetic baseline, project, leakage, and applicability datasets that
are compatible with the AMS-III.F helper functions.
}
\examples{
simulate_ams_iiif_dataset(n_sites = 2, n_periods = 3, seed = 123)
}
