\name{simulate_ams_iif_dataset}
\alias{simulate_ams_iif_dataset}
\title{Simulate a dataset compliant with AMS-II.F}
\usage{
simulate_ams_iif_dataset(n_facilities = 6,
                                     n_periods = 12,
                                     start_year = 2023,
                                     start_month = 1,
                                     baseline_electricity_mean = 140,
                                     baseline_fuel_mean = 420,
                                     electricity_savings = 0.22,
                                     fuel_savings = 0.38,
                                     fuel_switch_reduction = 0.35)
}
\arguments{
  \item{n_facilities}{Number of agricultural facilities to simulate.}
  \item{n_periods}{Number of monitoring periods per facility (default monthly).}
  \item{start_year}{Calendar year for the first monitoring period.}
  \item{start_month}{Calendar month (1-12) for the first monitoring period.}
  \item{baseline_electricity_mean}{Mean baseline electricity consumption per}
  \item{baseline_fuel_mean}{Mean baseline thermal fuel energy demand per period}
  \item{electricity_savings}{Expected fractional reduction in electricity demand}
  \item{fuel_savings}{Expected fractional reduction in fuel demand under the}
  \item{fuel_switch_reduction}{Expected fractional reduction in thermal emission}
}
\value{A tibble containing facility IDs, monitoring metadata, baseline and}
\description{Generates tidy monitoring data for agricultural facilities implementing energy
efficiency and fuel switching measures under AMS-II.F. The simulation produces
multi-period observations with baseline and project fuel/electricity use,
emission factors, production proxies, leakage placeholders, and monitoring
metadata.
  period (MWh).
  (GJ).
  under the project (0-1).
  project (0-1).
  factors due to fuel switching (0-1).
  project energy parameters, production proxies, and leakage placeholders.}
\examples{
simulate_ams_iif_dataset(n_facilities = 3)
}
