% Generated manually
\name{simulate_ams_iiia_dataset}
\alias{simulate_ams_iiia_dataset}
\title{Simulate AMS-III.A monitoring data}
\description{
Generates synthetic monitoring observations for AMS-III.A projects that replace synthetic nitrogen fertilizer with inoculants.
}
\usage{
simulate_ams_iiia_dataset(n_farms = 10, n_periods = 3, start_year = 2024,
  seed = NULL, baseline_n_mean = 90, replacement_fraction = 0.7,
  legume_area_mean = 22, inoculant_rate_mean = 0.45,
  fertilizer_production_ef = 0.004, fertilizer_field_ef = 0.01,
  inoculant_ef = 0.002)
}
\arguments{
\item{n_farms}{Number of farms to simulate.}

\item{n_periods}{Number of monitoring periods per farm.}

\item{start_year}{Calendar year of the first monitoring period.}

\item{seed}{Optional seed for reproducibility.}

\item{baseline_n_mean}{Mean baseline synthetic nitrogen use per period (kg N).}

\item{replacement_fraction}{Expected fractional reduction in fertilizer use due to inoculants.}

\item{legume_area_mean}{Mean legume area planted per farm (ha).}

\item{inoculant_rate_mean}{Mean inoculant application rate (kg/ha).}

\item{fertilizer_production_ef}{Emission factor for fertilizer production (tCO2e per kg N).}

\item{fertilizer_field_ef}{Direct soil emission factor (tCO2e per kg N).}

\item{inoculant_ef}{Emission factor for inoculant production and application (tCO2e per kg).}
}
\value{
Tibble containing simulated baseline, project, inoculant, and leakage data with monitoring metadata.
}
\examples{
simulate_ams_iiia_dataset(n_farms = 3, n_periods = 2, seed = 42)
}
