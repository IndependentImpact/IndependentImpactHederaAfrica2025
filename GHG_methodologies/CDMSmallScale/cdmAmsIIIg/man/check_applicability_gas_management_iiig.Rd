% Generated manually for cdmAmsIIIg
\name{check_applicability_gas_management_iiig}
\alias{check_applicability_gas_management_iiig}
\title{Check gas management practices for AMS-III.G applicability}
\description{
Assess landfill gas destruction technology, minimum operating hours, and
redundancy of key equipment to confirm compliance with AMS-III.G.
}
\usage{
check_applicability_gas_management_iiig(
  data,
  destruction_technology_col = "destruction_technology",
  eligible_technologies = c("enclosed flare", "electricity generation", "direct thermal use"),
  operating_hours_col = "operating_hours_per_week",
  minimum_operating_hours = 30,
  redundancy_col = "redundancy_installed",
  group_cols = NULL,
  output_col = "gas_management_applicable"
)
}
\arguments{
\item{data}{Tibble containing gas management metadata.}

\item{destruction_technology_col}{Column storing the destruction or
utilisation technology description.}

\item{eligible_technologies}{Character vector listing eligible gas
management technologies.}

\item{operating_hours_col}{Column storing weekly operating hours.}

\item{minimum_operating_hours}{Minimum weekly operating hours required for
applicability.}

\item{redundancy_col}{Optional column storing logical indicators confirming
redundant equipment.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the logical output column summarising
compliance.}
}
\value{
Tibble indicating whether gas management practices satisfy AMS-III.G.
}
\examples{
gas <- tibble::tibble(
  site_id = c("LF1", "LF2"),
  destruction_technology = c("enclosed flare", "open flare"),
  operating_hours_per_week = c(40, 12),
  redundancy_installed = c(TRUE, FALSE)
)
check_applicability_gas_management_iiig(gas, group_cols = "site_id")
}
