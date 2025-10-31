\name{aggregate_monitoring_periods_iif}
\alias{aggregate_monitoring_periods_iif}
\title{Aggregate monitoring periods for AMS-II.F projects}
\usage{
aggregate_monitoring_periods_iif(monitoring_data,
                                             period_col = "monitoring_period",
                                             group_cols = NULL,
                                             summarise_cols = NULL,
                                             output_col = "monitoring_period")
}
\arguments{
  \item{monitoring_data}{Tibble containing monitoring records.}
  \item{period_col}{Column containing the monitoring period label (e.g. season or}
  \item{group_cols}{Character vector of columns describing facility identifiers.}
  \item{summarise_cols}{Optional character vector of numeric columns to sum. If}
  \item{output_col}{Name of the column storing the concatenated period labels.}
}
\value{A tibble aggregated by `group_cols` with numeric columns summed across}
\description{Agricultural monitoring data may be captured for crop seasons, irrigation
cycles, or monthly production runs. This helper aggregates tidy monitoring
records to the reporting period while concatenating the period labels and
summing numeric metrics such as energy consumption, service proxies, and
production levels.
  month).
  `NULL`, all numeric columns are aggregated.
  the monitoring periods.}
\examples{
monitoring <- tibble::tibble(
  facility_id = rep("rice_mill_1", 3),
  season = c("Early", "Mid", "Late"),
  project_total_energy_mwh = c(12.5, 11.8, 11.2),
  processed_grain_tonnes = c(140, 150, 138)
)
aggregate_monitoring_periods_iif(monitoring, period_col = season, group_cols = "facility_id")
}
