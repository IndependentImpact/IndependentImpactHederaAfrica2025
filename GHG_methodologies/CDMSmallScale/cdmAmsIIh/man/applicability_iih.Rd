% Generated manually
\name{applicability_iih}
\alias{check_applicability_centralization_scope_iih}
\alias{check_applicability_monitoring_iih}
\alias{check_applicability_efficiency_improvement_iih}
\title{Applicability checks for AMS-II.H}
\description{
Helpers that verify the key applicability conditions for AMS-II.H projects, including centralization scope, monitoring completeness, and efficiency improvement requirements.
}
\usage{
check_applicability_centralization_scope_iih(
  baseline_summary,
  project_summary,
  utility_col = "utility_service",
  baseline_unit_col = "baseline_unit_count",
  project_unit_col = "project_unit_count"
)

check_applicability_monitoring_iih(
  monitoring_data,
  required_cols = c(
    "baseline_fuel_use_gj",
    "baseline_emission_factor_tco2_per_gj",
    "project_fuel_use_gj",
    "project_emission_factor_tco2_per_gj"
  )
)

check_applicability_efficiency_improvement_iih(
  baseline_data,
  project_data,
  group_cols = NULL,
  baseline_fuel_col = "baseline_fuel_use_gj",
  baseline_output_col = "baseline_useful_output_gj",
  project_fuel_col = "project_fuel_use_gj",
  project_output_col = "project_useful_output_gj",
  minimum_improvement = 0.05
)
}
\arguments{
  \item{baseline_summary}{Tibble describing the decentralized baseline system.}
  \item{project_summary}{Tibble describing the centralized project system.}
  \item{utility_col}{Column identifying the utility service.}
  \item{baseline_unit_col}{Column storing the number of decentralized units in the baseline.}
  \item{project_unit_col}{Column storing the number of centralized units in the project scenario.}
  \item{monitoring_data}{Tibble containing combined baseline and project monitoring data.}
  \item{required_cols}{Character vector of columns that must exist with no missing values.}
  \item{baseline_data}{Tibble containing baseline monitoring data.}
  \item{project_data}{Tibble containing project monitoring data.}
  \item{group_cols}{Optional character vector of grouping columns shared across the datasets.}
  \item{baseline_fuel_col}{Column storing baseline fuel use (GJ).}
  \item{baseline_output_col}{Column storing baseline useful output (GJ).}
  \item{project_fuel_col}{Column storing project fuel use (GJ).}
  \item{project_output_col}{Column storing project useful output (GJ).}
  \item{minimum_improvement}{Minimum fractional improvement required for applicability.}
}
\value{
Logical scalars indicating whether each applicability criterion is satisfied.
}
\examples{
baseline <- tibble::tibble(
  utility_service = c("steam", "hot_water"),
  baseline_unit_count = c(4, 3)
)
project <- tibble::tibble(
  utility_service = c("steam", "hot_water"),
  project_unit_count = c(1, 1)
)
check_applicability_centralization_scope_iih(baseline, project)

monitoring <- tibble::tibble(
  baseline_fuel_use_gj = 4200,
  baseline_emission_factor_tco2_per_gj = 0.071,
  project_fuel_use_gj = 3000,
  project_emission_factor_tco2_per_gj = 0.068
)
check_applicability_monitoring_iih(monitoring)

efficiency <- check_applicability_efficiency_improvement_iih(
  baseline_data = tibble::tibble(
    facility = c("A", "B"),
    baseline_fuel_use_gj = c(4200, 3150),
    baseline_useful_output_gj = c(3600, 2700)
  ),
  project_data = tibble::tibble(
    facility = c("A", "B"),
    project_fuel_use_gj = c(3000, 2250),
    project_useful_output_gj = c(2880, 2160)
  ),
  group_cols = "facility",
  minimum_improvement = 0.05
)
}
