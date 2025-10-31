\name{calculate_baseline_electricity_emissions_acm0012}
\alias{calculate_baseline_electricity_emissions_acm0012}
\alias{calculate_baseline_thermal_emissions_acm0012}
\alias{calculate_baseline_emissions_acm0012}
\alias{calculate_project_electricity_emissions_acm0012}
\alias{calculate_auxiliary_fuel_emissions_acm0012}
\alias{calculate_methane_leakage_emissions_acm0012}
\alias{calculate_project_emissions_acm0012}
\alias{calculate_leakage_emissions_acm0012}
\alias{calculate_emission_reductions_acm0012}
\title{Equation helpers for ACM0012}
\description{
Set of equation-level helpers implementing ACM0012 baseline, project, leakage,
and emission reduction calculations for waste energy recovery projects.
}
\usage{
calculate_baseline_electricity_emissions_acm0012(electricity_export_mwh, baseline_grid_ef_t_per_mwh)

calculate_baseline_thermal_emissions_acm0012(thermal_export_gj, baseline_thermal_ef_t_per_gj)

calculate_baseline_emissions_acm0012(baseline_electricity_emissions, baseline_thermal_emissions, flare_gas_displacement_emissions = 0)

calculate_project_electricity_emissions_acm0012(electricity_import_mwh, project_grid_ef_t_per_mwh)

calculate_auxiliary_fuel_emissions_acm0012(auxiliary_fuel_tj, auxiliary_ef_t_per_tj)

calculate_methane_leakage_emissions_acm0012(methane_leakage_nm3, methane_density_t_per_nm3 = 0.000716, gwp_ch4 = 28)

calculate_project_emissions_acm0012(project_electricity_emissions, auxiliary_fuel_emissions, methane_leakage_emissions = 0)

calculate_leakage_emissions_acm0012(leakage_energy_mwh, leakage_ef_t_per_mwh)

calculate_emission_reductions_acm0012(baseline_emissions, project_emissions, leakage_emissions = 0)
}
\arguments{
  \item{electricity_export_mwh}{Electricity exported to the grid in megawatt-hours.}
  \item{baseline_grid_ef_t_per_mwh}{Baseline grid emission factor in tonnes CO2e per MWh.}
  \item{thermal_export_gj}{Useful thermal energy exported in gigajoules.}
  \item{baseline_thermal_ef_t_per_gj}{Baseline thermal emission factor in tonnes CO2e per GJ.}
  \item{baseline_electricity_emissions}{Baseline electricity emissions computed via \code{calculate_baseline_electricity_emissions_acm0012}.}
  \item{baseline_thermal_emissions}{Baseline thermal emissions computed via \code{calculate_baseline_thermal_emissions_acm0012}.}
  \item{flare_gas_displacement_emissions}{Optional additional baseline emissions from flare gas displacement.}
  \item{electricity_import_mwh}{Electricity imported for project operations in megawatt-hours.}
  \item{project_grid_ef_t_per_mwh}{Emission factor for imported electricity in tonnes CO2e per MWh.}
  \item{auxiliary_fuel_tj}{Auxiliary fossil fuel consumption in terajoules.}
  \item{auxiliary_ef_t_per_tj}{Emission factor for auxiliary fuel in tonnes CO2e per terajoule.}
  \item{methane_leakage_nm3}{Methane leakage in normal cubic metres.}
  \item{methane_density_t_per_nm3}{Methane density in tonnes per normal cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
  \item{project_electricity_emissions}{Project electricity emissions in tonnes CO2e.}
  \item{auxiliary_fuel_emissions}{Auxiliary fuel emissions in tonnes CO2e.}
  \item{methane_leakage_emissions}{Methane leakage emissions in tonnes CO2e.}
  \item{leakage_energy_mwh}{Energy subject to leakage in megawatt-hours.}
  \item{leakage_ef_t_per_mwh}{Leakage emission factor in tonnes CO2e per megawatt-hour.}
  \item{baseline_emissions}{Baseline emissions in tonnes CO2e.}
  \item{project_emissions}{Project emissions in tonnes CO2e.}
  \item{leakage_emissions}{Leakage emissions in tonnes CO2e.}
}
\value{
Numeric vector representing emissions or emission reductions expressed in tonnes CO2e.
}
\examples{
calculate_baseline_electricity_emissions_acm0012(100, 0.8)
calculate_baseline_thermal_emissions_acm0012(200, 0.055)
calculate_baseline_emissions_acm0012(80, 10, 5)
calculate_project_electricity_emissions_acm0012(5, 0.7)
calculate_auxiliary_fuel_emissions_acm0012(0.1, 56)
calculate_methane_leakage_emissions_acm0012(120)
calculate_project_emissions_acm0012(4, 2, 1)
calculate_leakage_emissions_acm0012(10, 0.1)
calculate_emission_reductions_acm0012(100, 20, 5)
}
