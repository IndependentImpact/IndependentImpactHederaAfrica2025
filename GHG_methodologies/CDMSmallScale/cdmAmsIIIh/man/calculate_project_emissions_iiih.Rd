\name{calculate_project_emissions_iiih}
\alias{calculate_project_emissions_iiih}
\title{Calculate project emissions for AMS-III.H projects}
\usage{
calculate_project_emissions_iiih(
  data,
  influent_flow_col = "influent_flow_m3_per_day",
  cod_concentration_col = "cod_mg_l",
  degradable_cod_fraction_col = "biodegradable_fraction",
  project_capture_efficiency_col = "project_capture_efficiency_fraction",
  destruction_efficiency_col = "destruction_efficiency_fraction",
  project_oxidation_fraction_col = "project_oxidation_fraction",
  fugitive_leakage_fraction_col = "fugitive_leakage_fraction",
  days_col = "days_in_period",
  electricity_consumption_col = "electricity_consumption_mwh",
  electricity_emission_factor_col = "electricity_ef_tco2_per_mwh",
  thermal_energy_consumption_col = "thermal_energy_consumption_gj",
  thermal_energy_emission_factor_col = "thermal_energy_ef_tco2_per_gj",
  group_cols = NULL,
  output_col = "project_emissions_tco2e",
  methane_conversion_factor_m3_per_kg_cod = 0.35,
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
\item{data}{Tibble containing project monitoring data.}

\item{influent_flow_col}{Column storing influent wastewater flow in m3 per day.}

\item{cod_concentration_col}{Column storing influent COD concentration in mg/L.}

\item{degradable_cod_fraction_col}{Optional column storing the biodegradable COD fraction.}

\item{project_capture_efficiency_col}{Column storing methane capture efficiency under the project scenario.}

\item{destruction_efficiency_col}{Column storing destruction efficiency of the flare or utilisation equipment.}

\item{project_oxidation_fraction_col}{Optional column storing oxidation fractions for uncaptured methane.}

\item{fugitive_leakage_fraction_col}{Optional column storing additional leakage fractions.}

\item{days_col}{Optional column storing the number of days represented by each record.}

\item{electricity_consumption_col}{Optional column storing electricity use in MWh.}

\item{electricity_emission_factor_col}{Optional column storing emission factors in tCO2 per MWh.}

\item{thermal_energy_consumption_col}{Optional column storing supplemental fuel use in GJ.}

\item{thermal_energy_emission_factor_col}{Optional column storing emission factors in tCO2 per GJ.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the output column for project emissions in tCO2e.}

\item{methane_conversion_factor_m3_per_kg_cod}{Methane generation potential in m3 CH4 per kg COD.}

\item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}

\item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped project emissions in tCO2e.}
\description{
Converts methane balances and optional energy inputs into project emissions for
AMS-III.H wastewater recovery systems.
}
\examples{
project <- tibble::tibble(
  site_id = c("WW1", "WW2"),
  influent_flow_m3_per_day = c(5500, 3200),
  cod_mg_l = c(3200, 1800),
  project_capture_efficiency_fraction = c(0.75, 0.65),
  destruction_efficiency_fraction = c(0.985, 0.97)
)
calculate_project_emissions_iiih(project, group_cols = "site_id")
}
