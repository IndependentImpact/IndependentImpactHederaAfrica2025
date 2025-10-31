\name{aggregate_monitoring_periods_iiih}
\alias{aggregate_monitoring_periods_iiih}
\title{Aggregate monitoring periods for AMS-III.H}
\usage{
aggregate_monitoring_periods_iiih(
  data,
  group_cols,
  monitoring_col = "monitoring_period"
)
}
\arguments{
\item{data}{Tibble containing period-level emissions.}

\item{group_cols}{Character vector identifying grouping columns.}

\item{monitoring_col}{Column storing the monitoring period identifier.}
}
\value{Tibble summarising emissions by group with monitoring period counts.}
\description{
Aggregates period-level emissions outputs and counts distinct monitoring periods
per grouping combination.
}
\examples{
period_results <- tibble::tibble(
  site_id = rep("WW1", 2),
  period = 1:2,
  baseline_emissions_tco2e = c(1500, 1480),
  project_emissions_tco2e = c(420, 410),
  leakage_emissions_tco2e = c(30, 28)
)
aggregate_monitoring_periods_iiih(period_results, group_cols = "site_id", monitoring_col = "period")
}
