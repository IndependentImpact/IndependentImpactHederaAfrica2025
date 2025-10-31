% Generated manually
\name{aggregate_monitoring_periods}
\alias{aggregate_monitoring_periods}
\title{Aggregate monitoring periods for ACM0001}
\usage{aggregate_monitoring_periods(monitoring_data)}
\arguments{
  \item{monitoring_data}{Tibble or data frame containing landfill gas monitoring
  observations with ACM0001-required columns. The data must include at least
  `period`, `methane_generation_m3`, `baseline_capture_efficiency`,
  `methane_captured_m3`, `destruction_efficiency`, `auxiliary_fuel_tj`,
  `auxiliary_ef_t_per_tj`, `electricity_import_mwh`, `import_ef_t_per_mwh`, and
  `leakage_fraction`. Optional columns `methane_density_t_per_m3`, `gwp_ch4`, and
  `oxidation_fraction` override default parameter values.}
}
\value{A tibble summarising each monitoring period with emissions and methane destruction metrics.}
\description{Summarises landfill gas monitoring data into period-level totals following the ACM0001 baseline, project, leakage,
and emission reduction workflow, including methane destruction mass and CO2e.}
\examples{
data <- simulate_acm0001_dataset(3, seed = 42)
aggregate_monitoring_periods(data)
}
