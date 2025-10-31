% Generated manually for cdmAmsIIIg
\name{check_applicability_monitoring_framework_iiig}
\alias{check_applicability_monitoring_framework_iiig}
\title{Check monitoring framework for AMS-III.G}
\description{
Confirm that monitoring plans include the minimum number of flow and methane
fraction measurements along with annual calibration events required by
AMS-III.G.
}
\usage{
check_applicability_monitoring_framework_iiig(
  data,
  flow_monitoring_col = "flow_measurements_per_week",
  methane_fraction_monitoring_col = "methane_measurements_per_week",
  calibration_events_col = "calibration_events_per_year",
  minimum_flow_measurements = 3,
  minimum_methane_measurements = 3,
  minimum_calibrations = 4,
  group_cols = NULL,
  output_col = "monitoring_applicable"
)
}
\arguments{
\item{data}{Tibble containing monitoring framework metadata.}

\item{flow_monitoring_col}{Column storing the number of flow measurements
per week.}

\item{methane_fraction_monitoring_col}{Column storing the number of methane
fraction measurements per week.}

\item{calibration_events_col}{Column storing the number of calibration
events per year.}

\item{minimum_flow_measurements}{Minimum acceptable flow measurements per
week.}

\item{minimum_methane_measurements}{Minimum acceptable methane fraction
measurements per week.}

\item{minimum_calibrations}{Minimum calibration events per year.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the logical output column summarising
compliance.}
}
\value{
Tibble indicating whether monitoring frameworks satisfy AMS-III.G.
}
\examples{
monitoring <- tibble::tibble(
  site_id = c("LF1", "LF2"),
  flow_measurements_per_week = c(7, 2),
  methane_measurements_per_week = c(5, 1),
  calibration_events_per_year = c(6, 2)
)
check_applicability_monitoring_framework_iiig(monitoring, group_cols = "site_id")
}
