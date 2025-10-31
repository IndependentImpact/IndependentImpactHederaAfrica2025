% Generated manually for cdmAmsIIIe
\name{aggregate_monitoring_periods_iiie}
\alias{aggregate_monitoring_periods_iiie}
\title{Aggregate monitoring periods for AMS-III.E}
\usage{
aggregate_monitoring_periods_iiie(data, group_cols, monitoring_col = "monitoring_period")
}
\arguments{
  \item{data}{Tibble containing period-level emissions.}
  \item{group_cols}{Character vector identifying grouping columns.}
  \item{monitoring_col}{Column storing the monitoring period identifier.}
}
\value{Tibble summarising emissions and monitoring period counts by group.}
\description{
Summarise baseline, project, and leakage emissions across monitoring periods at
the grouping level required for verification.
}
\examples{
period_results <- tibble::tibble(
  plant_id = rep("A", 2),
  period = 1:2,
  baseline_emissions_tco2e = c(12, 11),
  project_emissions_tco2e = c(4, 3.5),
  leakage_emissions_tco2e = c(0.4, 0.3)
)
aggregate_monitoring_periods_iiie(period_results, group_cols = "plant_id", monitoring_col = "period")
}
