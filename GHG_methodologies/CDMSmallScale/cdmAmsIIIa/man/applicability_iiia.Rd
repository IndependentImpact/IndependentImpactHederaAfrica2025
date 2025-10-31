% Generated manually
\name{applicability_iiia}
\alias{check_applicability_legume_share_iiia}
\alias{check_applicability_inoculant_registration_iiia}
\alias{check_applicability_monitoring_practices_iiia}
\title{Applicability helpers for AMS-III.A}
\description{
Helpers that evaluate AMS-III.A applicability conditions covering legume area shares, inoculant registration, and monitoring sufficiency.
}
\usage{
check_applicability_legume_share_iiia(data,
  legume_area_col = "legume_area_ha",
  total_area_col = "total_area_ha",
  min_legume_share = 0.2,
  group_cols = NULL)

check_applicability_inoculant_registration_iiia(data,
  registration_col = "inoculant_registered",
  group_cols = NULL)

check_applicability_monitoring_practices_iiia(data,
  monitoring_unit_cols,
  min_periods = 3)
}
\arguments{
\item{data}{Tibble containing monitoring records.}

\item{legume_area_col}{Column storing legume area (ha).}

\item{total_area_col}{Column storing total cropped area (ha).}

\item{min_legume_share}{Minimum allowable legume area share.}

\item{group_cols}{Optional grouping columns identifying management units.}

\item{registration_col}{Column storing logical inoculant registration flags.}

\item{monitoring_unit_cols}{Character vector identifying monitoring unit columns.}

\item{min_periods}{Minimum number of monitoring periods per unit.}
}
\value{
Tibbles indicating whether each applicability condition is satisfied.
}
\examples{
library(tibble)
fields <- tibble(
  farm_id = c("A", "B"),
  legume_area_ha = c(24, 6),
  total_area_ha = c(30, 40)
)
check_applicability_legume_share_iiia(fields, "legume_area_ha", "total_area_ha")

records <- tibble(
  farm_id = c("A", "A", "B"),
  monitoring_period = c(1, 2, 1),
  inoculant_registered = c(TRUE, TRUE, FALSE)
)
check_applicability_inoculant_registration_iiia(records, "inoculant_registered", "farm_id")

monitoring <- tibble(
  farm_id = c("A", "A", "A", "B"),
  season = c(1, 2, 3, 1)
)
check_applicability_monitoring_practices_iiia(monitoring, "farm_id")
}
