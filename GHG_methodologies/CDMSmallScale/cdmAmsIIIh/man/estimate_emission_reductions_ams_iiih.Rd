\name{estimate_emission_reductions_ams_iiih}
\alias{estimate_emission_reductions_ams_iiih}
\title{Estimate emission reductions for AMS-III.H projects}
\usage{
estimate_emission_reductions_ams_iiih(
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
\item{baseline_data}{Tibble containing baseline wastewater data.}

\item{project_data}{Tibble containing project monitoring data.}

\item{leakage_data}{Optional tibble containing leakage monitoring data.}

\item{group_cols}{Optional character vector identifying grouping columns.}

\item{baseline_args}{Named list of arguments passed to \code{calculate_baseline_methane_emissions_iiih}.}

\item{project_args}{Named list of arguments passed to \code{calculate_project_emissions_iiih}.}

\item{leakage_args}{Named list of arguments passed to \code{calculate_leakage_emissions_iiih}.}
}
\value{Tibble containing baseline, project, leakage, and net emission reductions.}
\description{
Coordinates the baseline, project, and leakage helpers to transform monitoring
inputs into emission reductions for AMS-III.H wastewater projects.
}
\examples{
monitoring <- simulate_ams_iiih_dataset(n_sites = 1, n_periods = 2, seed = 99)
estimate_emission_reductions_ams_iiih(
  baseline_data = monitoring$baseline,
  project_data = monitoring$project,
  leakage_data = monitoring$leakage,
  group_cols = "site_id"
)
}
