\name{check_applicability_recovery_system_iiih}
\alias{check_applicability_recovery_system_iiih}
\title{Check recovery system design for AMS-III.H applicability}
\usage{
check_applicability_recovery_system_iiih(
  data,
  capture_system_col = "gas_capture_installed",
  destruction_technology_col = "destruction_technology",
  eligible_technologies = c(
    "enclosed flare",
    "boiler firing",
    "electricity generation",
    "biogas upgrading"
  ),
  operating_hours_col = "operating_hours_per_week",
  minimum_operating_hours = 25,
  redundancy_col = "redundancy_installed",
  utilisation_documented_col = "utilisation_documented",
  group_cols = NULL,
  output_col = "recovery_applicable"
)
}
\arguments{
\item{data}{Tibble containing recovery system metadata.}

\item{capture_system_col}{Column indicating whether a gas-tight cover or digester is installed.}

\item{destruction_technology_col}{Column describing the destruction or utilisation technology.}

\item{eligible_technologies}{Technologies eligible under AMS-III.H.}

\item{operating_hours_col}{Column storing weekly operating hours.}

\item{minimum_operating_hours}{Minimum weekly operating hours required for applicability.}

\item{redundancy_col}{Optional column confirming redundant destruction equipment or storage.}

\item{utilisation_documented_col}{Optional column confirming utilisation routes are documented.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the logical output column summarising compliance.}
}
\value{Tibble indicating whether recovery systems satisfy AMS-III.H requirements.}
\description{
Validates methane capture, destruction technology, and operating schedules for
wastewater methane recovery projects.
}
\examples{
recovery <- tibble::tibble(
  site_id = c("WW1", "WW2"),
  gas_capture_installed = c(TRUE, FALSE),
  destruction_technology = c("enclosed flare", "flare"),
  operating_hours_per_week = c(45, 12)
)
check_applicability_recovery_system_iiih(recovery, group_cols = "site_id")
}
