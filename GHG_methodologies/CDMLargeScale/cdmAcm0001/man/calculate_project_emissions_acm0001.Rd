% Generated manually
\name{calculate_project_emissions_acm0001}
\alias{calculate_project_emissions_acm0001}
\title{Calculate ACM0001 project emissions}
\usage{calculate_project_emissions_acm0001(methane_captured_m3, destruction_efficiency, methane_density_t_per_m3 = 0.000716, gwp_ch4 = 28, auxiliary_fuel_tj = 0, auxiliary_ef_t_per_tj = 0, electricity_import_mwh = 0, import_ef_t_per_mwh = 0)}
\arguments{
  \item{methane_captured_m3}{Methane captured by the project in cubic metres.}
  \item{destruction_efficiency}{Fraction of captured methane destroyed.}
  \item{methane_density_t_per_m3}{Methane density in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential of methane.}
  \item{auxiliary_fuel_tj}{Auxiliary fossil fuel consumption in terajoules.}
  \item{auxiliary_ef_t_per_tj}{Emission factor for auxiliary fuel in tonnes CO2e per terajoule.}
  \item{electricity_import_mwh}{Electricity imported from the grid in megawatt-hours.}
  \item{import_ef_t_per_mwh}{Emission factor for imported electricity in tonnes CO2e per megawatt-hour.}
}
\value{Numeric vector of project emissions in tonnes CO2e.}
\description{Computes project emissions from methane slip, auxiliary fossil fuel use, and electricity imports.}
