% Generated manually to mirror roxygen documentation
\name{acm0013_applicability}
\alias{check_applicability_acm0013}
\title{ACM0013 applicability checks}
\description{Helper to confirm that a monitoring dataset satisfies the key ACM0013 applicability conditions for new, grid-connected, lower-emission fossil fuel plants.}
\usage{
check_applicability_acm0013(data, efficiency_improvement_threshold = 0.1)
}
\arguments{
  \item{data}{Tibble or data frame with the columns \code{is_new_plant}, \code{grid_connected}, \code{technology_emission_factor_tco2_per_mwh}, and \code{baseline_emission_factor_tco2_per_mwh}.}
  \item{efficiency_improvement_threshold}{Minimum proportional improvement in emission factor required for applicability.}
}
\value{Logical scalar indicating whether the dataset meets ACM0013 applicability conditions.}
\examples{
dataset <- simulate_acm0013_dataset(periods = 1, seed = 99)
check_applicability_acm0013(dataset)
}
