% Generated manually for cdmAmsIIIg
\name{simulate_ams_iiig_dataset}
\alias{simulate_ams_iiig_dataset}
\title{Simulate monitoring data for AMS-III.G projects}
\description{
Generate synthetic baseline, project, leakage, and applicability datasets
compatible with the AMS-III.G helper functions.
}
\usage{
simulate_ams_iiig_dataset(n_sites = 3, n_periods = 4, seed = NULL)
}
\arguments{
\item{n_sites}{Number of landfill sites to simulate.}

\item{n_periods}{Number of monitoring periods per site.}

\item{seed}{Optional seed for reproducibility.}
}
\value{
List containing tibbles \code{baseline}, \code{project}, \code{leakage}, and
\code{applicability}.
}
\examples{
simulate_ams_iiig_dataset(n_sites = 2, n_periods = 3, seed = 2025)
}
