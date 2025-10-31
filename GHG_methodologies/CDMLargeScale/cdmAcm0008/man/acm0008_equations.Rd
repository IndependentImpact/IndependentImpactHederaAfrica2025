% Generated manually
\name{acm0008_equations}
\alias{calculate_methane_volume_acm0008}
\alias{calculate_baseline_emissions_acm0008}
\alias{calculate_project_emissions_acm0008}
\alias{calculate_leakage_emissions_acm0008}
\alias{calculate_net_emission_reductions_acm0008}
\title{Equation helpers for ACM0008}
\description{Functions translating the core numbered equations of ACM0008 into reusable R helpers.}
\usage{
calculate_methane_volume_acm0008(flow_rate_m3_per_h, operating_hours)

calculate_baseline_emissions_acm0008(recovered_volume_m3, methane_fraction, methane_density_t_per_m3 = 0.000716, gwp_ch4 = 28)

calculate_project_emissions_acm0008(recovered_volume_m3, methane_fraction, oxidation_efficiency, methane_density_t_per_m3 = 0.000716, gwp_ch4 = 28)

calculate_leakage_emissions_acm0008(electricity_use_mwh, grid_emission_factor_t_per_mwh)

calculate_net_emission_reductions_acm0008(baseline_emissions_tco2e, project_emissions_tco2e, leakage_emissions_tco2e)
}
\arguments{
\item{flow_rate_m3_per_h}{Numeric vector of methane flow rates in cubic metres per hour.}

\item{operating_hours}{Numeric vector of operating hours for each observation.}

\item{recovered_volume_m3}{Numeric vector of methane volumes recovered.}

\item{methane_fraction}{Numeric vector of methane fractions.}

\item{methane_density_t_per_m3}{Methane density in tonnes per cubic metre.}

\item{gwp_ch4}{Global warming potential of methane.}

\item{oxidation_efficiency}{Numeric vector describing the share of methane destroyed.}

\item{electricity_use_mwh}{Numeric vector of auxiliary electricity consumption.}

\item{grid_emission_factor_t_per_mwh}{Numeric vector of grid emission factors.}

\item{baseline_emissions_tco2e}{Numeric vector of baseline emissions.}

\item{project_emissions_tco2e}{Numeric vector of project emissions.}

\item{leakage_emissions_tco2e}{Numeric vector of leakage emissions.}
}
\value{Numeric vectors containing the calculated emissions quantities.}
\examples{
volume <- calculate_methane_volume_acm0008(500, 720)
baseline <- calculate_baseline_emissions_acm0008(volume, 0.35)
project <- calculate_project_emissions_acm0008(volume, 0.35, 0.95)
leakage <- calculate_leakage_emissions_acm0008(5, 0.7)
calculate_net_emission_reductions_acm0008(baseline, project, leakage)
}
