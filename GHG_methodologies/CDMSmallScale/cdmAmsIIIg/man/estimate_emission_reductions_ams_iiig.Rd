% Generated manually for cdmAmsIIIg
\name{estimate_emission_reductions_ams_iiig}
\alias{estimate_emission_reductions_ams_iiig}
\title{Estimate emission reductions for AMS-III.G projects}
\description{
Compose the AMS-III.G helpers to transform baseline, project, and leakage
data into net emission reductions with optional custom arguments for each
step.
}
\usage{
estimate_emission_reductions_ams_iiig(
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
\item{baseline_data}{Tibble containing baseline landfill data.}

\item{project_data}{Tibble containing project monitoring data.}

\item{leakage_data}{Optional tibble containing leakage monitoring data.}

\item{group_cols}{Optional character vector identifying grouping columns.}

\item{baseline_args}{Named list passed to
\code{calculate_baseline_methane_emissions_iiig}.}

\item{project_args}{Named list passed to
\code{calculate_project_emissions_iiig}.}

\item{leakage_args}{Named list passed to
\code{calculate_leakage_emissions_iiig}.}
}
\value{
Tibble containing baseline, project, leakage, and net emission reductions.
}
\examples{
monitoring <- simulate_ams_iiig_dataset(n_sites = 1, n_periods = 2, seed = 99)
estimate_emission_reductions_ams_iiig(
  baseline_data = monitoring$baseline,
  project_data = monitoring$project,
  leakage_data = monitoring$leakage,
  group_cols = "site_id"
)
}
