% Generated manually for cdmAmsIIg
\name{calculate_baseline_thermal_energy_iig}
\alias{calculate_baseline_thermal_energy_iig}
\alias{calculate_project_thermal_energy_iig}
\title{Convert non-renewable biomass into thermal energy}
\description{Implements the AMS-II.G equations that translate non-renewable biomass into useful thermal energy by applying the appropriate net calorific values (NCV) for the baseline and project scenarios.}
\usage{
calculate_baseline_thermal_energy_iig(
  non_renewable_biomass,
  biomass_col = "baseline_non_renewable_biomass_tonnes",
  baseline_data,
  ncv_col = "baseline_net_calorific_value_mj_per_tonne",
  group_cols = NULL,
  output_col = "baseline_thermal_energy_mj"
)

calculate_project_thermal_energy_iig(
  non_renewable_biomass,
  biomass_col = "project_non_renewable_biomass_tonnes",
  project_data,
  ncv_col = "project_net_calorific_value_mj_per_tonne",
  group_cols = NULL,
  output_col = "project_thermal_energy_mj"
)
}
\arguments{
\item{non_renewable_biomass}{Tibble returned by the corresponding non-renewable biomass helper.}
\item{biomass_col}{Column storing non-renewable biomass quantities.}
\item{baseline_data}{Tibble containing baseline NCV observations.}
\item{project_data}{Tibble containing project NCV observations.}
\item{ncv_col}{Column storing net calorific values in MJ per tonne.}
\item{group_cols}{Optional character vector of grouping columns.}
\item{output_col}{Name of the column storing thermal energy in MJ.}
}
\value{A tibble containing thermal energy totals by grouping structure.}
\examples{
non_renewable <- tibble::tibble(
  site_id = c("A", "B"),
  baseline_non_renewable_biomass_tonnes = c(12, 15)
)
inputs <- tibble::tibble(
  site_id = c("A", "B"),
  baseline_net_calorific_value_mj_per_tonne = c(15, 14)
)
calculate_baseline_thermal_energy_iig(
  non_renewable,
  baseline_data = inputs,
  group_cols = "site_id"
)
}
