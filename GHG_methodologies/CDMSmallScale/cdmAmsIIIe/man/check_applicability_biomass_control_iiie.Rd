% Generated manually for cdmAmsIIIe
\name{check_applicability_biomass_control_iiie}
\alias{check_applicability_biomass_control_iiie}
\title{Check biomass control evidence for AMS-III.E}
\usage{
check_applicability_biomass_control_iiie(
  data,
  anaerobic_baseline_col = "anaerobic_baseline",
  control_plan_col = "biomass_control_plan",
  group_cols = NULL,
  output_col = "biomass_control_applicable"
)
}
\arguments{
  \item{data}{Tibble containing applicability evidence.}
  \item{anaerobic_baseline_col}{Column storing indicators that the biomass would
  decay anaerobically in the baseline scenario.}
  \item{control_plan_col}{Column storing indicators that biomass collection is
  controlled.}
  \item{group_cols}{Optional character vector specifying grouping columns.}
  \item{output_col}{Name of the logical output column.}
}
\value{Tibble indicating whether biomass control evidence is documented.}
\description{
Confirm that biomass would have decayed anaerobically without the project and
that collection is controlled in line with AMS-III.E requirements.
}
\examples{
applicability <- tibble::tibble(
  plant_id = c("A", "B"),
  anaerobic_baseline = c(TRUE, TRUE),
  biomass_control_plan = c(TRUE, FALSE)
)
check_applicability_biomass_control_iiie(applicability, group_cols = "plant_id")
}
