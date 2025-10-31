% Generated manually for cdmAmsIIIf
\name{estimate_emission_reductions_ams_iiif}
\alias{estimate_emission_reductions_ams_iiif}
\title{Estimate emission reductions for AMS-III.F projects}
\usage{
estimate_emission_reductions_ams_iiif(
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
  \item{baseline_data}{Tibble containing baseline waste generation data.}
  \item{project_data}{Tibble containing composting operation data.}
  \item{leakage_data}{Optional tibble containing leakage monitoring data.}
  \item{group_cols}{Optional character vector identifying grouping columns.}
  \item{baseline_args}{Named list of arguments passed to
  \code{calculate_baseline_methane_emissions_iiif}.}
  \item{project_args}{Named list of arguments passed to
  \code{calculate_project_emissions_iiif}.}
  \item{leakage_args}{Named list of arguments passed to
  \code{calculate_leakage_emissions_iiif}.}
}
\value{Tibble containing baseline, project, leakage, and net emission reductions.}
\description{
Compose the AMS-III.F helpers to transform monitoring inputs into net emission
reductions.
}
\examples{
monitoring <- simulate_ams_iiif_dataset(n_sites = 1, n_periods = 2, seed = 42)
estimate_emission_reductions_ams_iiif(
  baseline_data = monitoring$baseline,
  project_data = monitoring$project,
  leakage_data = monitoring$leakage,
  group_cols = "site_id"
)
}
