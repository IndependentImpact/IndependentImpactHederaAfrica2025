% Generated manually for cdmAmsIIIe
\name{check_applicability_monitoring_practices_iiie}
\alias{check_applicability_monitoring_practices_iiie}
\title{Check monitoring practices for AMS-III.E}
\usage{
check_applicability_monitoring_practices_iiie(
  data,
  energy_measurements_col = "energy_measurements_per_month",
  operating_hours_col = "operating_hours_per_period",
  feedstock_sampling_col = "feedstock_samples_per_month",
  minimum_energy_measurements = 4,
  minimum_operating_hours = 400,
  minimum_feedstock_samples = 2,
  group_cols = NULL,
  output_col = "monitoring_practices_applicable"
)
}
\arguments{
  \item{data}{Tibble containing monitoring plan metadata.}
  \item{energy_measurements_col}{Column storing energy measurement counts.}
  \item{operating_hours_col}{Column storing operating hours per period.}
  \item{feedstock_sampling_col}{Column storing feedstock sampling counts.}
  \item{minimum_energy_measurements}{Minimum acceptable energy measurements per
  month.}
  \item{minimum_operating_hours}{Minimum required operating hours per period.}
  \item{minimum_feedstock_samples}{Minimum acceptable feedstock sampling events
  per month.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column.}
}
\value{Tibble indicating whether monitoring practices satisfy the methodology.}
\description{
Ensure monitoring plans capture energy output, operating hours, and feedstock
sampling at the frequency required by AMS-III.E.
}
\examples{
monitoring <- tibble::tibble(
  plant_id = c("A", "B"),
  energy_measurements_per_month = c(6, 3),
  operating_hours_per_period = c(450, 320),
  feedstock_samples_per_month = c(3, 1)
)
check_applicability_monitoring_practices_iiie(monitoring, group_cols = "plant_id")
}
