% Generated manually because roxygen2 is unavailable
\name{check_applicability_system_type_iiid}
\alias{check_applicability_system_type_iiid}
\title{Check manure management system eligibility for AMS-III.D}
\description{Verify that monitoring groups report eligible anaerobic manure management system types.}
\usage{
check_applicability_system_type_iiid(
  data,
  system_type_col = "system_type",
  allowed_types = c("anaerobic lagoon", "uncovered digester", "liquid slurry store"),
  group_cols = NULL,
  output_col = "system_type_applicable"
)
}
\arguments{
  \item{data}{Tibble containing project metadata.}
  \item{system_type_col}{Column storing system types.}
  \item{allowed_types}{Character vector of eligible system types.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column.}
}
\value{Tibble indicating whether each group satisfies the system type criterion.}
\examples{
metadata <- tibble::tibble(
  farm_id = c("A", "B"),
  system_type = c("anaerobic lagoon", "solid compost")
)
check_applicability_system_type_iiid(metadata, group_cols = "farm_id")
}
