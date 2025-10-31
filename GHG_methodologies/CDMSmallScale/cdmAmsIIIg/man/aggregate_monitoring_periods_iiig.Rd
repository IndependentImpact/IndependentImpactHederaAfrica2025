% Generated manually for cdmAmsIIIg
\name{aggregate_monitoring_periods_iiig}
\alias{aggregate_monitoring_periods_iiig}
\title{Aggregate monitoring periods for AMS-III.G}
\description{
Summarise period-level emissions outputs and count distinct monitoring
periods for each grouping combination.
}
\usage{
aggregate_monitoring_periods_iiig(data, group_cols, monitoring_col = "monitoring_period")
}
\arguments{
\item{data}{Tibble containing period-level emissions.}

\item{group_cols}{Character vector identifying grouping columns.}

\item{monitoring_col}{Column storing the monitoring period identifier.}
}
\value{
Tibble summarising emissions by group with monitoring period counts.
}
\examples{
period_results <- tibble::tibble(
  site_id = rep("LF1", 2),
  period = 1:2,
  baseline_emissions_tco2e = c(1500, 1480),
  project_emissions_tco2e = c(350, 340),
  leakage_emissions_tco2e = c(20, 25)
)
aggregate_monitoring_periods_iiig(period_results, group_cols = "site_id", monitoring_col = "period")
}
