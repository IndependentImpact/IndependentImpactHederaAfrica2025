% Generated manually
\name{acm0019_equations}
\alias{calculate_baseline_emissions_acm0019}
\alias{calculate_project_emissions_acm0019}
\alias{calculate_leakage_emissions_acm0019}
\alias{calculate_net_emission_reductions_acm0019}
\title{Equation helpers for ACM0019}
\description{Functions translating ACM0019 baseline, project, leakage, and net reduction equations into reusable R helpers.}
\usage{
calculate_baseline_emissions_acm0019(production_tonnes, baseline_ef_kg_per_tonne, gwp_n2o = 265)

calculate_project_emissions_acm0019(production_tonnes, project_ef_kg_per_tonne, gwp_n2o = 265)

calculate_leakage_emissions_acm0019(electricity_mwh = 0, grid_ef_t_per_mwh = 0, steam_tonnes = 0, steam_ef_t_per_tonne = 0)

calculate_net_emission_reductions_acm0019(baseline_emissions_tco2e, project_emissions_tco2e, leakage_emissions_tco2e = 0)
}
\arguments{
\item{production_tonnes}{Numeric vector of nitric acid production in tonnes.}

\item{baseline_ef_kg_per_tonne}{Numeric vector of baseline N2O emission factors in kilograms per tonne of nitric acid.}

\item{gwp_n2o}{Global warming potential applied to nitrous oxide emissions.}

\item{project_ef_kg_per_tonne}{Numeric vector of project emission factors in kilograms per tonne of nitric acid.}

\item{electricity_mwh}{Numeric vector of auxiliary electricity consumption.}

\item{grid_ef_t_per_mwh}{Numeric vector of grid emission factors in tonnes of CO2 per megawatt-hour.}

\item{steam_tonnes}{Numeric vector of imported steam quantities.}

\item{steam_ef_t_per_tonne}{Numeric vector of steam emission factors in tonnes of CO2 per tonne of steam.}

\item{baseline_emissions_tco2e}{Numeric vector of baseline emissions in tonnes of CO2 equivalent.}

\item{project_emissions_tco2e}{Numeric vector of project emissions in tonnes of CO2 equivalent.}

\item{leakage_emissions_tco2e}{Numeric vector of leakage emissions in tonnes of CO2 equivalent.}
}
\value{Numeric vectors containing the calculated emissions quantities in tonnes of CO2 equivalent.}
\examples{
baseline <- calculate_baseline_emissions_acm0019(1500, 9.5)
project <- calculate_project_emissions_acm0019(1500, 1.2)
leakage <- calculate_leakage_emissions_acm0019(120, 0.7, 50, 0.08)
calculate_net_emission_reductions_acm0019(baseline, project, leakage)
}
