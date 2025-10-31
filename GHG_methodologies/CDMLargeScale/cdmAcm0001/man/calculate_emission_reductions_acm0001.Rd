% Generated manually
\name{calculate_emission_reductions_acm0001}
\alias{calculate_emission_reductions_acm0001}
\title{Calculate ACM0001 emission reductions}
\usage{calculate_emission_reductions_acm0001(baseline_emissions, project_emissions, leakage_emissions = 0)}
\arguments{
  \item{baseline_emissions}{Baseline emissions in tonnes CO2e.}
  \item{project_emissions}{Project emissions in tonnes CO2e.}
  \item{leakage_emissions}{Leakage emissions in tonnes CO2e.}
}
\value{Numeric vector of emission reductions in tonnes CO2e.}
\description{Returns emission reductions as baseline minus project minus leakage emissions.}
