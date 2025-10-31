% Generated manually because roxygen2 is unavailable
\name{simulate_ams_iiid_dataset}
\alias{simulate_ams_iiid_dataset}
\title{Simulate monitoring data for AMS-III.D methane recovery projects}
\description{Generate a tidy tibble representing farms equipped with methane recovery systems and associated monitoring data.}
\usage{
simulate_ams_iiid_dataset(
  n_farms = 5,
  n_periods = 6,
  start_year = 2024,
  seed = NULL,
  vs_mean = 35,
  methane_potential_mean = 0.24,
  baseline_mcf_mean = 0.8,
  project_mcf_mean = 0.6
)
}
\arguments{
  \item{n_farms}{Number of farms or digesters to simulate.}
  \item{n_periods}{Number of monitoring periods per farm.}
  \item{start_year}{Calendar year of the first monitoring period.}
  \item{seed}{Optional integer seed for reproducibility.}
  \item{vs_mean}{Mean volatile solids generation per day (kg).}
  \item{methane_potential_mean}{Mean methane potential (m\eqn{^3} CH\eqn{_4}/kg VS).}
  \item{baseline_mcf_mean}{Mean baseline methane conversion factor (fraction).}
  \item{project_mcf_mean}{Mean project methane conversion factor (fraction).}
}
\value{Tibble containing simulated monitoring data compatible with AMS-III.D helpers.}
\examples{
simulate_ams_iiid_dataset(n_farms = 2, n_periods = 3, seed = 101)
}
