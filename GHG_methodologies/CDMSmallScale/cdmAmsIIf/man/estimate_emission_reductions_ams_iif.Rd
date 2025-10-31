\name{estimate_emission_reductions_ams_iif}
\alias{estimate_emission_reductions_ams_iif}
\title{Run the AMS-II.F emission reduction workflow}
\usage{
estimate_emission_reductions_ams_iif(baseline_data,
                                                 project_data,
                                                 leakage_data = NULL,
                                                 group_cols = NULL,
                                                 baseline_args = list(),
                                                 project_args = list(),
                                                 leakage_args = list(),
                                                 reduction_args = list())
}
\arguments{
  \item{baseline_data}{Tidy baseline dataset describing counterfactual fuel and}
  \item{project_data}{Tidy project dataset describing monitored fuel and}
  \item{leakage_data}{Optional tibble of leakage components aligned with the}
  \item{group_cols}{Character vector of grouping columns common to the supplied}
  \item{baseline_args}{Optional list of additional arguments passed to}
  \item{project_args}{Optional list of additional arguments passed to}
  \item{leakage_args}{Optional list of additional arguments passed to}
  \item{reduction_args}{Optional list of arguments forwarded to}
}
\value{A tibble containing baseline, project, leakage, and emission reduction}
\description{Orchestrates the numbered equations for AMS-II.F to compute annual emission
reductions from agricultural energy efficiency or fuel switching projects.
The meta-function wires together
[calculate_baseline_agricultural_emissions()],
[calculate_project_agricultural_emissions()],
[calculate_leakage_emissions_iif()], and
[calculate_emission_reductions_iif()] while allowing optional fine-tuning via
argument lists.
  electricity use.
  electricity use.
  grouping structure of the baseline/project data.
  datasets.
  [calculate_baseline_agricultural_emissions()].
  [calculate_project_agricultural_emissions()].
  [calculate_leakage_emissions_iif()].
  [calculate_emission_reductions_iif()]. `group_cols` is injected
  automatically.
  totals by the requested grouping structure.}
\examples{
baseline <- tibble::tibble(
  facility_id = "rice_mill_1",
  baseline_fuel_energy_gj = 960,
  baseline_fuel_emission_factor_tco2_per_gj = 0.072,
  baseline_electricity_mwh = 180,
  baseline_electricity_emission_factor_tco2_per_mwh = 0.61
)
project <- tibble::tibble(
  facility_id = "rice_mill_1",
  project_fuel_energy_gj = 520,
  project_fuel_emission_factor_tco2_per_gj = 0.03,
  project_electricity_mwh = 150,
  project_electricity_emission_factor_tco2_per_mwh = 0.61
)
estimate_emission_reductions_ams_iif(baseline, project, group_cols = "facility_id")
}
