% Generated manually for cdmAmsIIIf
\name{check_applicability_feedstock_management_iiif}
\alias{check_applicability_feedstock_management_iiif}
\title{Check feedstock management for AMS-III.F applicability}
\usage{
check_applicability_feedstock_management_iiif(
  data,
  waste_type_col = "waste_type",
  eligible_waste = c(
    "municipal organics",
    "agricultural residues",
    "market waste",
    "food processing waste"
  ),
  source_segregated_col = "source_segregated",
  contamination_rate_col = "contamination_fraction",
  maximum_contamination = 0.1,
  group_cols = NULL,
  output_col = "feedstock_applicable"
)
}
\arguments{
  \item{data}{Tibble containing feedstock metadata.}
  \item{waste_type_col}{Column storing the waste type or category.}
  \item{eligible_waste}{Character vector listing eligible waste categories.}
  \item{source_segregated_col}{Column storing logical indicators confirming
  source segregation.}
  \item{contamination_rate_col}{Column storing the contamination fraction.}
  \item{maximum_contamination}{Maximum allowable contamination fraction.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column summarising compliance.}
}
\value{Tibble indicating whether each group satisfies feedstock requirements.}
\description{
Verify that incoming waste streams are eligible, source segregated, and below
contamination limits required by AMS-III.F.
}
\examples{
metadata <- tibble::tibble(
  site_id = c("A", "B"),
  waste_type = c("municipal organics", "mixed waste"),
  source_segregated = c(TRUE, FALSE),
  contamination_fraction = c(0.08, 0.25)
)
check_applicability_feedstock_management_iiif(metadata, group_cols = "site_id")
}
