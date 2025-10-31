% Generated manually to mirror roxygen documentation
\name{acm0010_meta}
\alias{aggregate_monitoring_periods_acm0010}
\alias{estimate_emission_reductions_acm0010}
\alias{simulate_acm0010_dataset}
\title{ACM0010 meta workflow tools}
\description{Meta-functions that simulate ACM0010 data, estimate emissions, and aggregate results.}
\usage{
aggregate_monitoring_periods_acm0010(data, period_col = "period")

estimate_emission_reductions_acm0010(data, gwp_ch4 = 28, methane_density = 0.0007168)

simulate_acm0010_dataset(periods = 12, seed = NULL)
}
\arguments{
  \item{data}{Tibble of monitoring observations or emission results.}
  \item{period_col}{Name of the period column to aggregate by.}
  \item{gwp_ch4}{Global warming potential for methane.}
  \item{methane_density}{Density of methane in tonnes per cubic metre.}
  \item{periods}{Number of monitoring periods to simulate.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{\code{aggregate_monitoring_periods_acm0010()} and \code{estimate_emission_reductions_acm0010()} return tibbles and lists; \code{simulate_acm0010_dataset()} returns a tibble of simulated monitoring data.}
\examples{
monitoring <- simulate_acm0010_dataset(periods = 3, seed = 12)
results <- estimate_emission_reductions_acm0010(monitoring)
aggregate_monitoring_periods_acm0010(results$period_results)
}
