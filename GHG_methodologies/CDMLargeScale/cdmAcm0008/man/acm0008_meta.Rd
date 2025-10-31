% Generated manually
\name{acm0008_meta}
\alias{aggregate_monitoring_periods_acm0008}
\alias{estimate_emission_reductions_acm0008}
\title{Monitoring aggregation and total emission reductions for ACM0008}
\description{Wrappers that aggregate monitoring records and calculate total baseline, project, leakage, and net emission reductions for coal mine methane projects.}
\usage{
aggregate_monitoring_periods_acm0008(monitoring_data)

estimate_emission_reductions_acm0008(monitoring_data)
}
\arguments{
\item{monitoring_data}{Tibble or data frame containing the monitoring variables required by the ACM0008 helpers.}
}
\value{Tibbles summarising period-level or total emission accounting results.}
\examples{
data <- simulate_acm0008_dataset(2, observations_per_period = 5, seed = 21)
aggregate_monitoring_periods_acm0008(data)
estimate_emission_reductions_acm0008(data)
}
