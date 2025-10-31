% Generated manually because roxygen2 is unavailable
\name{aggregate_monitoring_periods_iiid}
\alias{aggregate_monitoring_periods_iiid}
\title{Aggregate monitoring periods for AMS-III.D datasets}
\description{Roll up period-level monitoring data to the grouping level used in AMS-III.D calculations.}
\usage{
aggregate_monitoring_periods_iiid(
  data,
  group_cols,
  sum_cols = c("volatile_solids_kg_per_day", "methane_recovered_m3", "leakage_emissions_tco2e", "days_in_period"),
  mean_cols = c("methane_potential_m3_per_kg_vs", "baseline_mcf_fraction", "project_mcf_fraction", "capture_efficiency_fraction", "destruction_efficiency_fraction"),
  na_rm = TRUE
)
}
\arguments{
  \item{data}{Tibble containing period-level monitoring data.}
  \item{group_cols}{Character vector of grouping columns (e.g. \code{"farm_id"}).}
  \item{sum_cols}{Numeric columns to sum across monitoring periods.}
  \item{mean_cols}{Numeric columns to average across monitoring periods.}
  \item{na_rm}{Logical indicating whether to remove missing values during aggregation.}
}
\value{Tibble aggregated to the specified grouping columns.}
\examples{
monitoring <- tibble::tibble(
  farm_id = rep("A", 2),
  monitoring_period = c(1, 2),
  volatile_solids_kg_per_day = c(40, 42),
  methane_recovered_m3 = c(1200, 1300),
  days_in_period = c(30, 30),
  methane_potential_m3_per_kg_vs = c(0.23, 0.24),
  project_mcf_fraction = c(0.6, 0.58)
)
aggregate_monitoring_periods_iiid(monitoring, group_cols = "farm_id")
}
