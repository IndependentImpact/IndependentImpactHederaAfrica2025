% Generated manually to mirror roxygen documentation
\name{acm0013_meta}
\alias{aggregate_monitoring_periods_acm0013}
\alias{estimate_emission_reductions_acm0013}
\alias{simulate_acm0013_dataset}
\title{ACM0013 meta workflow tools}
\description{Meta-functions that simulate ACM0013 data, estimate emissions, and aggregate monitoring results.}
\usage{
aggregate_monitoring_periods_acm0013(data, period_col = "period")

estimate_emission_reductions_acm0013(data)

simulate_acm0013_dataset(periods = 12, seed = NULL)
}
\arguments{
  \item{data}{Tibble of monitoring observations or emission results.}
  \item{period_col}{Name of the monitoring period column used for aggregation.}
  \item{periods}{Number of monitoring periods to simulate.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{\code{aggregate_monitoring_periods_acm0013()} and \code{estimate_emission_reductions_acm0013()} return tibbles and lists; \code{simulate_acm0013_dataset()} returns a tibble of simulated monitoring data.}
\examples{
monitoring <- simulate_acm0013_dataset(periods = 3, seed = 12)
results <- estimate_emission_reductions_acm0013(monitoring)
aggregate_monitoring_periods_acm0013(results$period_results)
}
