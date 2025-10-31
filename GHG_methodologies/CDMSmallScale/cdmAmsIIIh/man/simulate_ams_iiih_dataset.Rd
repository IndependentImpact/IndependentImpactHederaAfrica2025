\name{simulate_ams_iiih_dataset}
\alias{simulate_ams_iiih_dataset}
\title{Simulate monitoring data for AMS-III.H projects}
\usage{
simulate_ams_iiih_dataset(n_sites = 3, n_periods = 4, seed = NULL)
}
\arguments{
\item{n_sites}{Number of wastewater facilities to simulate.}

\item{n_periods}{Number of monitoring periods per facility.}

\item{seed}{Optional seed for reproducibility.}
}
\value{List containing tibbles \code{baseline}, \code{project}, \code{leakage}, and \code{applicability}.}
\description{
Generates synthetic wastewater monitoring datasets consistent with the
AMS-III.H helper functions.
}
\examples{
simulate_ams_iiih_dataset(n_sites = 2, n_periods = 3, seed = 2025)
}
