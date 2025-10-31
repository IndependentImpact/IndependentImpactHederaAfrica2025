% Generated manually for cdmAmsIIIe
\name{estimate_emission_reductions_ams_iiie}
\alias{estimate_emission_reductions_ams_iiie}
\title{Estimate emission reductions for AMS-III.E projects}
\usage{
estimate_emission_reductions_ams_iiie(
  baseline_data,
  project_data,
  leakage_data = NULL,
  group_cols = NULL,
  baseline_args = list(),
  project_args = list(),
  leakage_args = list()
)
}
\arguments{
  \item{baseline_data}{Tibble containing baseline biomass data.}
  \item{project_data}{Tibble containing project operation data.}
  \item{leakage_data}{Optional tibble containing leakage monitoring data.}
  \item{group_cols}{Optional character vector identifying grouping columns.}
  \item{baseline_args}{Named list of arguments passed to
  \code{calculate_baseline_methane_emissions_iiie}.}
  \item{project_args}{Named list of arguments passed to
  \code{calculate_project_emissions_iiie}.}
  \item{leakage_args}{Named list of arguments passed to
  \code{calculate_leakage_emissions_iiie}.}
}
\value{Tibble containing baseline, project, leakage, and net emission reductions.}
\description{
Compose the AMS-III.E helpers into an end-to-end workflow that converts
monitoring inputs into net emission reductions.
}
\examples{
monitoring <- simulate_ams_iiie_dataset(n_plants = 1, n_periods = 2, seed = 42)
estimate_emission_reductions_ams_iiie(
  baseline_data = monitoring$baseline,
  project_data = monitoring$project,
  leakage_data = monitoring$leakage,
  group_cols = "plant_id"
)
}
