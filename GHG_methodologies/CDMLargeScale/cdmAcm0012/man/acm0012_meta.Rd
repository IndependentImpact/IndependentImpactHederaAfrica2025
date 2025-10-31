\name{aggregate_monitoring_periods}
\alias{aggregate_monitoring_periods}
\alias{estimate_emission_reductions_acm0012}
\title{Meta-functions for ACM0012 monitoring data}
\description{
Helpers that aggregate ACM0012 monitoring data into period summaries and total
emission reduction estimates.
}
\usage{
aggregate_monitoring_periods(monitoring_data)

estimate_emission_reductions_acm0012(monitoring_data)
}
\arguments{
  \item{monitoring_data}{Data frame or tibble containing ACM0012 monitoring data.}
}
\value{
Tibble summarising emissions by period or a single-row tibble of total emissions.
}
\examples{
data <- simulate_acm0012_dataset(2, observations_per_period = 3, seed = 42)
period_summary <- aggregate_monitoring_periods(data)
estimate_emission_reductions_acm0012(data)
}
