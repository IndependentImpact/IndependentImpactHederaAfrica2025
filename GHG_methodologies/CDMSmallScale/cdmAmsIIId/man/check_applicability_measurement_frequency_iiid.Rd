% Generated manually because roxygen2 is unavailable
\name{check_applicability_measurement_frequency_iiid}
\alias{check_applicability_measurement_frequency_iiid}
\title{Check methane monitoring frequency for AMS-III.D}
\description{Ensure methane recovery is monitored at or above the required frequency.}
\usage{
check_applicability_measurement_frequency_iiid(
  data,
  frequency_col = "measurements_per_month",
  minimum_frequency = 4,
  group_cols = NULL,
  output_col = "measurement_frequency_applicable"
)
}
\arguments{
  \item{data}{Tibble containing monitoring system data.}
  \item{frequency_col}{Column storing measurement frequency per month.}
  \item{minimum_frequency}{Minimum acceptable number of measurements per month.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column.}
}
\value{Tibble indicating whether each group satisfies the measurement frequency requirement.}
\examples{
monitoring <- tibble::tibble(
  digester_id = c("D1", "D2"),
  measurements_per_month = c(8, 2)
)
check_applicability_measurement_frequency_iiid(monitoring, group_cols = "digester_id")
}
