% Generated manually
\name{aggregate_monitoring_periods_iiia}
\alias{aggregate_monitoring_periods_iiia}
\title{Aggregate AMS-III.A monitoring periods}
\description{
Summarises period-level baseline and project emissions for AMS-III.A monitoring datasets.
}
\usage{
aggregate_monitoring_periods_iiia(data, group_cols, monitoring_col = "monitoring_period")
}
\arguments{
\item{data}{Tibble containing period-level emissions.}

\item{group_cols}{Character vector of grouping columns (e.g. farm identifiers).}

\item{monitoring_col}{Column storing the monitoring period identifier.}
}
\value{
Tibble aggregated to the supplied grouping columns with summed emissions.
}
\examples{
results <- tibble::tibble(
  farm_id = rep("A", 2),
  monitoring_period = 1:2,
  baseline_emissions_tco2e = c(3, 2.5),
  project_fertilizer_emissions_tco2e = c(0.4, 0.5),
  project_inoculant_emissions_tco2e = c(0.2, 0.2)
)
aggregate_monitoring_periods_iiia(results, group_cols = "farm_id")
}
