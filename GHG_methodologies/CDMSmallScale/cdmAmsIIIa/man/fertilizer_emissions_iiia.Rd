% Generated manually
\name{fertilizer_emissions_iiia}
\alias{calculate_baseline_fertilizer_emissions_iiia}
\alias{calculate_project_residual_fertilizer_emissions_iiia}
\alias{calculate_project_inoculant_emissions_iiia}
\alias{calculate_leakage_emissions_iiia}
\title{Fertilizer and inoculant emission calculations for AMS-III.A}
\description{
Helpers that translate the AMS-III.A equations covering baseline fertilizer emissions, project-period residual fertilizer use, inoculant emissions, and leakage.
}
\usage{
calculate_baseline_fertilizer_emissions_iiia(data,
  fertilizer_use_col = "synthetic_n_applied_kg",
  production_emission_factor_col = "production_ef_tco2_per_kg",
  field_emission_factor_col = "field_ef_tco2_per_kg",
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e")

calculate_project_residual_fertilizer_emissions_iiia(data,
  fertilizer_use_col = "synthetic_n_applied_kg",
  production_emission_factor_col = "production_ef_tco2_per_kg",
  field_emission_factor_col = "field_ef_tco2_per_kg",
  group_cols = NULL,
  output_col = "project_fertilizer_emissions_tco2e")

calculate_project_inoculant_emissions_iiia(data,
  inoculant_rate_col = "inoculant_rate_kg_per_ha",
  area_planted_col = "legume_area_ha",
  inoculant_emission_factor_col = "inoculant_ef_tco2_per_kg",
  group_cols = NULL,
  output_col = "project_inoculant_emissions_tco2e")

calculate_leakage_emissions_iiia(data,
  leakage_col = "leakage_emissions_tco2e",
  group_cols = NULL)
}
\arguments{
\item{data}{Tibble containing monitoring data.}

\item{fertilizer_use_col}{Column storing synthetic nitrogen use (kg N).}

\item{production_emission_factor_col}{Column storing emission factors for fertilizer production (tCO2e per kg N).}

\item{field_emission_factor_col}{Column storing direct soil emission factors (tCO2e per kg N).}

\item{group_cols}{Optional grouping columns.}

\item{output_col}{Name of the resulting emissions column.}

\item{inoculant_rate_col}{Column storing inoculant application rates (kg/ha).}

\item{area_planted_col}{Column storing the legume area planted (ha).}

\item{inoculant_emission_factor_col}{Column storing inoculant emission factors (tCO2e per kg).}

\item{leakage_col}{Column storing leakage emissions (tCO2e).}
}
\value{
Tibbles with baseline, project, inoculant, or leakage emissions aggregated by group when supplied.
}
\examples{
baseline <- tibble::tibble(
  synthetic_n_applied_kg = c(120, 80),
  production_ef_tco2_per_kg = 0.004,
  field_ef_tco2_per_kg = 0.01
)
calculate_baseline_fertilizer_emissions_iiia(baseline)

project <- tibble::tibble(
  synthetic_n_applied_kg = c(20, 10),
  production_ef_tco2_per_kg = 0.004,
  field_ef_tco2_per_kg = 0.01
)
calculate_project_residual_fertilizer_emissions_iiia(project)

data <- tibble::tibble(
  inoculant_rate_kg_per_ha = c(0.5, 0.4),
  legume_area_ha = c(30, 25),
  inoculant_ef_tco2_per_kg = 0.002
)
calculate_project_inoculant_emissions_iiia(data)
