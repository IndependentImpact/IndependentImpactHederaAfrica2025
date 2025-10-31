% Generated manually for cdmAmsIIIf
\name{check_applicability_composting_practices_iiif}
\alias{check_applicability_composting_practices_iiif}
\title{Check composting system practices for AMS-III.F}
\usage{
check_applicability_composting_practices_iiif(
  data,
  aeration_method_col = "aeration_method",
  compliant_aeration_methods = c("forced aeration", "turned windrow", "in-vessel"),
  retention_time_col = "retention_days",
  minimum_retention_days = 42,
  leachate_management_col = "leachate_managed",
  curing_phase_col = "curing_phase",
  group_cols = NULL,
  output_col = "composting_practices_applicable"
)
}
\arguments{
  \item{data}{Tibble containing composting system metadata.}
  \item{aeration_method_col}{Column storing the aeration method description.}
  \item{compliant_aeration_methods}{Character vector of compliant aeration
  methods.}
  \item{retention_time_col}{Column storing the minimum residence time (days).}
  \item{minimum_retention_days}{Minimum required residence time.}
  \item{leachate_management_col}{Column storing logical indicators that leachate
  is managed.}
  \item{curing_phase_col}{Optional column storing logical indicators confirming a
  curing phase.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column summarising compliance.}
}
\value{Tibble indicating whether composting practices satisfy AMS-III.F.}
\description{
Evaluate aeration method, retention time, leachate management, and curing phase
requirements specified by AMS-III.F.
}
\examples{
systems <- tibble::tibble(
  site_id = c("A", "B"),
  aeration_method = c("forced aeration", "static pile"),
  retention_days = c(56, 30),
  leachate_managed = c(TRUE, TRUE)
)
check_applicability_composting_practices_iiif(systems, group_cols = "site_id")
}
