% Generated manually
\name{acm0019_applicability}
\alias{check_applicability_monitoring_coverage}
\alias{check_applicability_catalyst_configuration}
\alias{check_applicability_operating_regime}
\title{Applicability checks for ACM0019}
\description{Helpers that evaluate monitoring coverage, catalyst configurations, and operating regimes required by ACM0019.}
\usage{
check_applicability_monitoring_coverage(data_capture_rate, minimum_coverage = 0.9)

check_applicability_catalyst_configuration(catalyst_configuration, allowed_configurations = c("secondary", "tertiary"))

check_applicability_operating_regime(plant_regime, has_continuous_monitoring)
}
\arguments{
\item{data_capture_rate}{Numeric vector with monitoring data capture rates between 0 and 1.}

\item{minimum_coverage}{Minimum acceptable capture threshold.}

\item{catalyst_configuration}{Character vector describing installed abatement configurations.}

\item{allowed_configurations}{Character vector of allowed configurations.}

\item{plant_regime}{Character scalar describing the nitric acid operating regime.}

\item{has_continuous_monitoring}{Logical scalar indicating whether continuous monitoring systems operate reliably.}
}
\value{Logical vector indicating whether the applicability condition holds.}
\examples{
check_applicability_monitoring_coverage(c(0.92, 0.88))
check_applicability_catalyst_configuration(c("secondary", "tertiary"))
check_applicability_operating_regime("weak-acid", TRUE)
}
