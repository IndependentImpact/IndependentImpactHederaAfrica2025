\name{check_applicability_monitoring_framework_iiih}
\alias{check_applicability_monitoring_framework_iiih}
\title{Check monitoring frameworks for AMS-III.H}
\usage{
check_applicability_monitoring_framework_iiih(
  data,
  flow_monitoring_col = "flow_measurements_per_week",
  methane_fraction_monitoring_col = "methane_measurements_per_week",
  cod_sampling_col = "cod_samples_per_month",
  minimum_flow_measurements = 7,
  minimum_methane_measurements = 3,
  minimum_cod_samples = 4,
  calibration_events_col = "calibration_events_per_year",
  minimum_calibrations = 4,
  group_cols = NULL,
  output_col = "monitoring_applicable"
)
}
\arguments{
\item{data}{Tibble containing monitoring framework metadata.}

\item{flow_monitoring_col}{Column storing the number of flow measurements per week.}

\item{methane_fraction_monitoring_col}{Column storing methane fraction measurements per week.}

\item{cod_sampling_col}{Column storing the number of influent COD samples per month.}

\item{minimum_flow_measurements}{Minimum flow measurements per week.}

\item{minimum_methane_measurements}{Minimum methane fraction measurements per week.}

\item{minimum_cod_samples}{Minimum influent COD samples per month.}

\item{calibration_events_col}{Column storing the number of instrument calibrations per year.}

\item{minimum_calibrations}{Minimum calibration events per year.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the logical output column summarising compliance.}
}
\value{Tibble indicating whether monitoring plans satisfy AMS-III.H.}
\description{
Validates that monitoring frequencies and calibration intervals meet
methodological minimums.
}
\examples{
monitoring <- tibble::tibble(
  site_id = c("WW1", "WW2"),
  flow_measurements_per_week = c(14, 4),
  methane_measurements_per_week = c(5, 1),
  cod_samples_per_month = c(6, 2),
  calibration_events_per_year = c(5, 2)
)
check_applicability_monitoring_framework_iiih(monitoring, group_cols = "site_id")
}
