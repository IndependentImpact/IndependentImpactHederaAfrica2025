% Generated manually because roxygen2 is unavailable
\name{estimate_emission_reductions_ams_iiid}
\alias{estimate_emission_reductions_ams_iiid}
\title{Estimate emission reductions for AMS-III.D projects}
\description{Compose equation helpers to compute emission reductions using baseline, project, recovery, and leakage data.}
\usage{
estimate_emission_reductions_ams_iiid(
  baseline_data,
  project_data,
  recovery_data = NULL,
  leakage_data = NULL,
  group_cols = NULL,
  volatile_solids_col = "volatile_solids_kg_per_day",
  methane_potential_col = "methane_potential_m3_per_kg_vs",
  baseline_mcf_col = "baseline_mcf_fraction",
  project_mcf_col = "project_mcf_fraction",
  capture_efficiency_col = "capture_efficiency_fraction",
  destruction_efficiency_col = "destruction_efficiency_fraction",
  days_col = "days_in_period",
  methane_recovered_col = "methane_recovered_m3",
  leakage_col = "leakage_emissions_tco2e",
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
  \item{baseline_data}{Tibble containing baseline manure management data.}
  \item{project_data}{Tibble containing project monitoring data.}
  \item{recovery_data}{Optional tibble containing methane recovery data.}
  \item{leakage_data}{Optional tibble containing leakage emissions in tCO2e.}
  \item{group_cols}{Optional character vector of grouping columns.}
  \item{volatile_solids_col}{Column storing volatile solids generated per day.}
  \item{methane_potential_col}{Column storing methane producing potential.}
  \item{baseline_mcf_col}{Column storing the baseline methane conversion factor.}
  \item{project_mcf_col}{Column storing the project methane conversion factor.}
  \item{capture_efficiency_col}{Column storing methane capture efficiency.}
  \item{destruction_efficiency_col}{Column storing destruction efficiency for captured methane.}
  \item{days_col}{Column storing the number of days per monitoring record.}
  \item{methane_recovered_col}{Column storing recovered methane in cubic metres.}
  \item{leakage_col}{Column storing leakage emissions in tCO2e.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing emission reductions in tCO2e per group.}
\examples{
baseline <- tibble::tibble(
  farm_id = "A",
  volatile_solids_kg_per_day = 40,
  methane_potential_m3_per_kg_vs = 0.25,
  baseline_mcf_fraction = 0.85,
  days_in_period = 365
)
project <- tibble::tibble(
  farm_id = "A",
  volatile_solids_kg_per_day = 40,
  methane_potential_m3_per_kg_vs = 0.25,
  project_mcf_fraction = 0.6,
  capture_efficiency_fraction = 0.9,
  destruction_efficiency_fraction = 0.98,
  days_in_period = 365,
  methane_recovered_m3 = 15000
)
estimate_emission_reductions_ams_iiid(baseline, project, group_cols = "farm_id")
}
