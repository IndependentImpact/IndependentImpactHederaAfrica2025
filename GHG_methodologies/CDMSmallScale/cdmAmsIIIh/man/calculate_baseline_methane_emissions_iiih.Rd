\name{calculate_baseline_methane_emissions_iiih}
\alias{calculate_baseline_methane_emissions_iiih}
\title{Calculate baseline methane emissions for AMS-III.H projects}
\usage{
calculate_baseline_methane_emissions_iiih(
  data,
  influent_flow_col = "influent_flow_m3_per_day",
  cod_concentration_col = "cod_mg_l",
  degradable_cod_fraction_col = "biodegradable_fraction",
  baseline_capture_efficiency_col = "baseline_capture_efficiency_fraction",
  oxidation_fraction_col = "baseline_oxidation_fraction",
  days_col = "days_in_period",
  group_cols = NULL,
  output_col = "baseline_emissions_tco2e",
  methane_conversion_factor_m3_per_kg_cod = 0.35,
  methane_density_t_per_m3 = 0.00067,
  gwp_ch4 = 28
)
}
\arguments{
\item{data}{Tibble containing baseline wastewater monitoring data.}

\item{influent_flow_col}{Column storing influent wastewater flow in m3 per day.}

\item{cod_concentration_col}{Column storing influent COD concentration in mg/L.}

\item{degradable_cod_fraction_col}{Optional column storing the biodegradable COD fraction.}

\item{baseline_capture_efficiency_col}{Optional column storing baseline capture efficiency.}

\item{oxidation_fraction_col}{Optional column storing oxidation fractions for residual methane.}

\item{days_col}{Optional column storing the number of days represented by each record.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the output column for baseline emissions in tCO2e.}

\item{methane_conversion_factor_m3_per_kg_cod}{Methane generation potential in m3 CH4 per kg COD.}

\item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}

\item{gwp_ch4}{Global warming potential of methane.}
}
\value{Tibble containing grouped baseline methane emissions in tCO2e.}
\description{
Estimates methane generation from COD loads, applies optional baseline capture
and oxidation fractions, and converts to tCO2e.
}
\examples{
baseline <- tibble::tibble(
  site_id = c("WW1", "WW2"),
  influent_flow_m3_per_day = c(5500, 3200),
  cod_mg_l = c(3200, 1800),
  biodegradable_fraction = c(0.8, 0.65)
)
calculate_baseline_methane_emissions_iiih(baseline, group_cols = "site_id")
}
