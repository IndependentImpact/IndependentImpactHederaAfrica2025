% Generated manually for cdmAmsIIIf
\name{check_applicability_monitoring_framework_iiif}
\alias{check_applicability_monitoring_framework_iiif}
\title{Check monitoring framework for AMS-III.F}
\usage{
check_applicability_monitoring_framework_iiif(
  data,
  organic_sampling_col = "organic_samples_per_year",
  temperature_monitoring_col = "temperature_checks_per_week",
  moisture_monitoring_col = "moisture_checks_per_week",
  minimum_organic_samples = 12,
  minimum_temperature_measurements = 3,
  minimum_moisture_measurements = 2,
  group_cols = NULL,
  output_col = "monitoring_framework_applicable"
)
}
\arguments{
  \item{data}{Tibble containing monitoring framework metadata.}
  \item{organic_sampling_col}{Column storing the number of organic samples per
  year.}
  \item{temperature_monitoring_col}{Column storing temperature measurements per
  week.}
  \item{moisture_monitoring_col}{Column storing moisture measurements per week.}
  \item{minimum_organic_samples}{Minimum acceptable number of organic samples per
  year.}
  \item{minimum_temperature_measurements}{Minimum temperature readings per
  week.}
  \item{minimum_moisture_measurements}{Minimum moisture measurements per week.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column summarising compliance.}
}
\value{Tibble indicating whether monitoring plans satisfy AMS-III.F.}
\description{
Confirm that monitoring plans include organic content sampling, temperature
checks, and moisture measurements at the frequencies required by AMS-III.F.
}
\examples{
monitoring <- tibble::tibble(
  site_id = c("A", "B"),
  organic_samples_per_year = c(16, 8),
  temperature_checks_per_week = c(4, 2),
  moisture_checks_per_week = c(3, 1)
)
check_applicability_monitoring_framework_iiif(monitoring, group_cols = "site_id")
}
