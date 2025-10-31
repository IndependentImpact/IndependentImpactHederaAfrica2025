% Generated manually for cdmAmsIIIe
\name{check_applicability_feedstock_characteristics_iiie}
\alias{check_applicability_feedstock_characteristics_iiie}
\title{Check feedstock characteristics for AMS-III.E applicability}
\usage{
check_applicability_feedstock_characteristics_iiie(
  data,
  feedstock_type_col = "feedstock_type",
  eligible_feedstock = c("agricultural residues", "forest residues", "industrial biomass"),
  moisture_content_col = "moisture_fraction",
  maximum_moisture = 0.6,
  group_cols = NULL,
  output_col = "feedstock_applicable"
)
}
\arguments{
  \item{data}{Tibble containing feedstock metadata.}
  \item{feedstock_type_col}{Column storing feedstock types.}
  \item{eligible_feedstock}{Character vector listing eligible feedstock types.}
  \item{moisture_content_col}{Column storing feedstock moisture content.}
  \item{maximum_moisture}{Maximum allowed moisture fraction.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column.}
}
\value{Tibble indicating whether each group satisfies feedstock criteria.}
\description{
Verify that feedstock types are eligible and moisture content remains within the
limits established by AMS-III.E.
}
\examples{
metadata <- tibble::tibble(
  plant_id = c("A", "B"),
  feedstock_type = c("agricultural residues", "municipal waste"),
  moisture_fraction = c(0.45, 0.75)
)
check_applicability_feedstock_characteristics_iiie(metadata, group_cols = "plant_id")
}
