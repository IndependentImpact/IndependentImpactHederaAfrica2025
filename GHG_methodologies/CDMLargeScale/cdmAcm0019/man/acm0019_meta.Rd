% Generated manually
\name{acm0019_meta}
\alias{aggregate_monitoring_periods_acm0019}
\alias{estimate_emission_reductions_acm0019}
\title{Monitoring aggregation and reporting helpers for ACM0019}
\description{Wrappers that aggregate monitoring data and calculate total emission reductions for ACM0019 nitric acid projects.}
\usage{
aggregate_monitoring_periods_acm0019(monitoring_data)

estimate_emission_reductions_acm0019(monitoring_data)
}
\arguments{
\item{monitoring_data}{Tibble containing period identifiers, production, emission factors, and auxiliary energy use.}
}
\value{Tibbles summarising period-level or total emission results.}
\examples{
monitoring <- simulate_acm0019_dataset(4, seed = 99)
aggregate_monitoring_periods_acm0019(monitoring)
estimate_emission_reductions_acm0019(monitoring)
}
