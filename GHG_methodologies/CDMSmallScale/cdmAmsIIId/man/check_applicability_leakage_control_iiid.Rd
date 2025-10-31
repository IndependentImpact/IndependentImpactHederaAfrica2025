% Generated manually because roxygen2 is unavailable
\name{check_applicability_leakage_control_iiid}
\alias{check_applicability_leakage_control_iiid}
\title{Check methane leakage controls for AMS-III.D}
\description{Confirm that methane capture systems include leakage control measures.}
\usage{
check_applicability_leakage_control_iiid(
  data,
  control_indicator_col = "leakage_controls_in_place",
  group_cols = NULL,
  output_col = "leakage_controls_applicable"
)
}
\arguments{
  \item{data}{Tibble containing leakage control records.}
  \item{control_indicator_col}{Column storing logical indicators of leakage controls.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column.}
}
\value{Tibble indicating whether leakage controls are documented for each group.}
\examples{
controls <- tibble::tibble(
  farm_id = c("A", "B"),
  leakage_controls_in_place = c(TRUE, FALSE)
)
check_applicability_leakage_control_iiid(controls, group_cols = "farm_id")
}
