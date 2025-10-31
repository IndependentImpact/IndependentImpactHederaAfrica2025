% Generated manually to mirror roxygen documentation
\name{acm0010_equations}
\alias{convert_methane_volume_to_mass}
\alias{calculate_cod_removed_acm0010}
\alias{calculate_baseline_emissions_acm0010}
\alias{calculate_project_emissions_acm0010}
\alias{calculate_leakage_emissions_acm0010}
\alias{calculate_net_emission_reductions_acm0010}
\title{ACM0010 equation helpers}
\description{Reusable helpers for ACM0010 methane accounting equations.}
\usage{
convert_methane_volume_to_mass(volume_m3, methane_density = 0.0007168)

calculate_cod_removed_acm0010(cod_in_mg_l, cod_out_mg_l, flow_m3)

calculate_baseline_emissions_acm0010(cod_removed_kg, methane_conversion_factor, methane_fraction = 0.67, gwp_ch4 = 28)

calculate_project_emissions_acm0010(methane_recovered_m3, combustion_efficiency, methane_density = 0.0007168, gwp_ch4 = 28)

calculate_leakage_emissions_acm0010(electricity_consumed_mwh, grid_emission_factor, additional_leakage = 0)

calculate_net_emission_reductions_acm0010(baseline, project, leakage)
}
\arguments{
  \item{volume_m3}{Methane volume in cubic metres.}
  \item{methane_density}{Density of methane in tonnes per cubic metre.}
  \item{cod_in_mg_l}{Influent COD concentration in mg/L.}
  \item{cod_out_mg_l}{Effluent COD concentration in mg/L.}
  \item{flow_m3}{Daily wastewater flow in cubic metres.}
  \item{cod_removed_kg}{COD removed in kilograms per day.}
  \item{methane_conversion_factor}{Fraction of degradable COD converted to methane.}
  \item{methane_fraction}{Fraction of methane in biogas.}
  \item{gwp_ch4}{Global warming potential for methane.}
  \item{methane_recovered_m3}{Volume of methane recovered in cubic metres.}
  \item{combustion_efficiency}{Fraction of recovered methane combusted.}
  \item{electricity_consumed_mwh}{Grid electricity consumption in MWh.}
  \item{grid_emission_factor}{Grid emission factor in tCO2e/MWh.}
  \item{additional_leakage}{Additional leakage emissions in tonnes CO2e.}
  \item{baseline}{Baseline emissions in tonnes CO2e.}
  \item{project}{Project emissions in tonnes CO2e.}
  \item{leakage}{Leakage emissions in tonnes CO2e.}
}
\value{Numeric vectors representing the requested quantity.}
\examples{
convert_methane_volume_to_mass(100)
calculate_cod_removed_acm0010(4000, 800, 250)
calculate_baseline_emissions_acm0010(50, 0.8)
calculate_project_emissions_acm0010(1200, 0.95)
calculate_leakage_emissions_acm0010(10, 0.8, 2)
calculate_net_emission_reductions_acm0010(100, 5, 3)
}
