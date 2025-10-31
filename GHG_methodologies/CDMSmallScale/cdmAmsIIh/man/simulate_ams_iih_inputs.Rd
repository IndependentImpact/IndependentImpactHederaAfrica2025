% Generated manually
\name{simulate_ams_iih_inputs}
\alias{simulate_ams_iih_inputs}
\title{Simulate monitoring data for AMS-II.H}
\description{
Generates synthetic baseline, project, and leakage datasets for AMS-II.H projects. The simulation includes fuel use, emission factors, useful output, auxiliary electricity, and leakage placeholders that match the inputs required by the workflow helpers.
}
\usage{
simulate_ams_iih_inputs(
  n_facilities = 4,
  seed = NULL,
  baseline_fuel_mean = 4200,
  central_efficiency_gain = 0.35,
  electricity_intensity = 0.045,
  leakage_fraction = 0.02
)
}
\arguments{
  \item{n_facilities}{Number of industrial facilities to simulate.}
  \item{seed}{Optional integer seed for reproducibility.}
  \item{baseline_fuel_mean}{Mean baseline fuel use per facility (GJ).}
  \item{central_efficiency_gain}{Expected fractional reduction in specific energy consumption (0-1).}
  \item{electricity_intensity}{Auxiliary electricity intensity in MWh per unit of useful output (GJ).}
  \item{leakage_fraction}{Fraction of baseline fuel use represented as leakage emissions.}
}
\value{
A list containing \code{baseline_data}, \code{project_data}, and \code{leakage_data} tibbles.
}
\examples{
simulate_ams_iih_inputs(n_facilities = 2, seed = 42)
}
