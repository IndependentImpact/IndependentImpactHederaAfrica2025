% Generated manually for cdmAmsIIg
\name{simulate_ams_iig_dataset}
\alias{simulate_ams_iig_dataset}
\title{Simulate monitoring data for AMS-II.G}
\description{Generates synthetic monitoring observations for AMS-II.G projects, including baseline and project biomass consumption, non-renewable fractions, emission factors, leakage placeholders, and monitoring metadata.}
\usage{
simulate_ams_iig_dataset(
  n_sites = 5,
  n_periods = 4,
  start_year = 2024,
  start_month = 1,
  seed = NULL,
  baseline_consumption_mean = 14,
  efficiency_gain = 0.45,
  baseline_fraction_mean = 0.85,
  project_fraction_mean = 0.4
)
}
\arguments{
\item{n_sites}{Number of project sites or households to simulate.}
\item{n_periods}{Number of monitoring periods per site.}
\item{start_year}{Starting calendar year for the monitoring periods.}
\item{start_month}{Starting month for the monitoring periods (1-12).}
\item{seed}{Optional random seed for reproducibility.}
\item{baseline_consumption_mean}{Mean baseline biomass consumption per period (tonnes).}
\item{efficiency_gain}{Expected fractional reduction in biomass consumption due to efficiency improvements (0-1).}
\item{baseline_fraction_mean}{Mean baseline non-renewable fraction.}
\item{project_fraction_mean}{Mean project non-renewable fraction.}
}
\value{A tibble containing monitoring metadata and the columns required by \code{estimate_emission_reductions_ams_iig()}.}
\examples{
simulate_ams_iig_dataset(n_sites = 2, n_periods = 3, seed = 123)
}
