\name{check_applicability_monitoring_iif}
\alias{check_applicability_monitoring_iif}
\title{Check AMS-II.F monitoring readiness}
\usage{
check_applicability_monitoring_iif(monitoring_data,
                                               required_cols = c(
                                                 "project_total_energy_mwh",
                                                 "service_level_indicator",
                                                 "operating_hours"
                                               ))
}
\arguments{
  \item{monitoring_data}{Tibble containing monitoring period records.}
  \item{required_cols}{Character vector of columns that must be present.}
}
\value{Logical scalar indicating whether the monitoring dataset satisfies the}
\description{AMS-II.F monitoring requires consistent records of project energy use,
agricultural output proxies, and operating hours. This helper verifies the
presence of required columns and ensures they are complete.
  completeness requirements.}
\examples{
monitoring <- tibble::tibble(
  facility_id = "rice_mill_1",
  project_total_energy_mwh = 12.5,
  service_level_indicator = 0.35,
  operating_hours = 220
)
check_applicability_monitoring_iif(monitoring,
  required_cols = c("project_total_energy_mwh", "service_level_indicator", "operating_hours")
)
}
