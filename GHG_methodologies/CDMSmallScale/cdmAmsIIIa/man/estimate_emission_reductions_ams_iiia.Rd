% Generated manually
\name{estimate_emission_reductions_ams_iiia}
\alias{estimate_emission_reductions_ams_iiia}
\title{Run the AMS-III.A emission reduction workflow}
\description{
Chains the AMS-III.A equation helpers to convert baseline fertilizer use, project monitoring data, and leakage observations into emission reductions.
}
\usage{
estimate_emission_reductions_ams_iiia(baseline_data, project_data, leakage_data = NULL,
  group_cols = NULL,
  baseline_fertilizer_col = "baseline_synthetic_n_kg",
  baseline_production_ef_col = "baseline_production_ef_tco2_per_kg",
  baseline_field_ef_col = "baseline_field_ef_tco2_per_kg",
  project_fertilizer_col = "project_synthetic_n_kg",
  project_production_ef_col = "project_production_ef_tco2_per_kg",
  project_field_ef_col = "project_field_ef_tco2_per_kg",
  inoculant_rate_col = "inoculant_rate_kg_per_ha",
  area_planted_col = "legume_area_ha",
  inoculant_emission_factor_col = "inoculant_ef_tco2_per_kg",
  leakage_col = "leakage_emissions_tco2e")
}
\arguments{
\item{baseline_data}{Tibble containing baseline fertilizer observations.}

\item{project_data}{Tibble containing project-period monitoring data.}

\item{leakage_data}{Optional tibble containing leakage emissions.}

\item{group_cols}{Optional grouping columns shared across inputs.}

\item{baseline_fertilizer_col}{Column storing baseline synthetic nitrogen use (kg N).}

\item{baseline_production_ef_col}{Column storing baseline fertilizer production emission factors (tCO2e per kg N).}

\item{baseline_field_ef_col}{Column storing baseline direct soil emission factors (tCO2e per kg N).}

\item{project_fertilizer_col}{Column storing residual synthetic nitrogen applied during the project (kg N).}

\item{project_production_ef_col}{Column storing project fertilizer production emission factors (tCO2e per kg N).}

\item{project_field_ef_col}{Column storing project direct soil emission factors (tCO2e per kg N).}

\item{inoculant_rate_col}{Column storing inoculant application rates (kg/ha).}

\item{area_planted_col}{Column storing the legume area planted (ha).}

\item{inoculant_emission_factor_col}{Column storing inoculant emission factors (tCO2e per kg).}

\item{leakage_col}{Column storing leakage emissions (tCO2e).}
}
\value{
Tibble containing baseline emissions, project emissions, leakage, and emission reductions.
}
\examples{
baseline <- tibble::tibble(
  farm_id = c("A", "B"),
  baseline_synthetic_n_kg = c(120, 90),
  baseline_production_ef_tco2_per_kg = 0.004,
  baseline_field_ef_tco2_per_kg = 0.01
)
project <- tibble::tibble(
  farm_id = c("A", "B"),
  project_synthetic_n_kg = c(20, 15),
  project_production_ef_tco2_per_kg = 0.004,
  project_field_ef_tco2_per_kg = 0.01,
  inoculant_rate_kg_per_ha = c(0.5, 0.4),
  legume_area_ha = c(30, 25),
  inoculant_ef_tco2_per_kg = 0.002
)
estimate_emission_reductions_ams_iiia(
  baseline,
  project,
  group_cols = "farm_id"
)
}
