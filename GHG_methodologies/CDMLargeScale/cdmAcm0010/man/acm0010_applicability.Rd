% Generated manually to mirror roxygen documentation
\name{acm0010_applicability}
\alias{check_applicability_feedstock_mix}
\alias{check_applicability_baseline_system}
\title{ACM0010 applicability helpers}
\description{Functions that validate ACM0010 applicability conditions for project feedstock mixes and baseline systems.}
\usage{
check_applicability_feedstock_mix(feedstock, eligible = 0.7)

check_applicability_baseline_system(baseline_system)
}
\arguments{
  \item{feedstock}{Character vector of feedstock categories supplied to the anaerobic digestion system.}
  \item{eligible}{Minimum proportion of eligible categories required for applicability.}
  \item{baseline_system}{Character vector describing the baseline treatment technology (e.g. \code{"open lagoon"}, \code{"composting"}).}
}
\value{\code{check_applicability_feedstock_mix()} returns a logical scalar. \code{check_applicability_baseline_system()} returns a logical vector.}
\examples{
check_applicability_feedstock_mix(c("manure", "wastewater"))
check_applicability_baseline_system(c("open lagoon", "composting"))
}
