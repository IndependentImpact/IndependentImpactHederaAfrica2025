% Generated manually
\name{ams_ie_applicability}
\alias{check_applicability_capacity_limit}
\alias{check_applicability_non_renewable_fraction}
\alias{check_applicability_project_renewable_fraction}
\title{AMS-I.E applicability helpers}
\description{Logical checks that document whether an activity satisfies core AMS-I.E applicability conditions.}
\usage{
check_applicability_capacity_limit(thermal_capacity_mw)

check_applicability_non_renewable_fraction(non_renewable_fraction)

check_applicability_project_renewable_fraction(renewable_fraction, threshold = 0.9)
}
\arguments{
  \item{thermal_capacity_mw}{Thermal capacity of the project in MWth.}
  \item{non_renewable_fraction}{Vector of non-renewable biomass fractions.}
  \item{renewable_fraction}{Vector describing the renewable fraction of project energy.}
  \item{threshold}{Minimum acceptable renewable fraction.}
}
\value{Logical scalar indicating whether the condition is met.}
\examples{
check_applicability_capacity_limit(thermal_capacity_mw = 20)
check_applicability_non_renewable_fraction(c(0.7, 0.85))
check_applicability_project_renewable_fraction(renewable_fraction = 0.95)
}
