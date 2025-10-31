% Generated manually for cdmAmsIIIf
\name{aggregate_monitoring_periods_iiif}
\alias{aggregate_monitoring_periods_iiif}
\title{Aggregate monitoring periods for AMS-III.F}
\usage{
aggregate_monitoring_periods_iiif(
  data,
  group_cols,
  monitoring_col = "monitoring_period"
)
}
\arguments{
  \item{data}{Tibble containing period-level emissions output.}
  \item{group_cols}{Character vector identifying grouping columns.}
  \item{monitoring_col}{Column storing the monitoring period identifier.}
}
\value{Tibble summarising emissions by group with monitoring period counts.}
\description{
Aggregate baseline, project, and leakage emissions across monitoring periods and
return totals alongside the number of monitoring periods per group.
}
\examples{
period_results <- tibble::tibble(
  site_id = rep("A", 2),
  period = 1:2,
  baseline_emissions_tco2e = c(20, 19),
  project_emissions_tco2e = c(5, 4.5),
  leakage_emissions_tco2e = c(0.5, 0.4)
)
aggregate_monitoring_periods_iiif(period_results, group_cols = "site_id", monitoring_col = "period")
}
