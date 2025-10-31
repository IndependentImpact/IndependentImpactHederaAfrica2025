\name{check_waste_energy_fraction_acm0012}
\alias{check_waste_energy_fraction_acm0012}
\alias{check_baseline_continuation_acm0012}
\alias{check_metering_uncertainty_acm0012}
\title{Applicability helpers for ACM0012}
\description{
Quantitative applicability checks derived from ACM0012 requirements.
}
\usage{
check_waste_energy_fraction_acm0012(waste_energy_fraction, threshold = 0.75)

check_baseline_continuation_acm0012(baseline_operating_hours, project_operating_hours)

check_metering_uncertainty_acm0012(measurement_uncertainty)
}
\arguments{
  \item{waste_energy_fraction}{Fraction of exported energy from qualifying waste sources.}
  \item{threshold}{Minimum allowable waste energy fraction.}
  \item{baseline_operating_hours}{Expected annual operating hours under the baseline scenario.}
  \item{project_operating_hours}{Observed project operating hours.}
  \item{measurement_uncertainty}{Combined metering uncertainty expressed as a decimal.}
}
\value{
Logical vector indicating whether each applicability condition is satisfied.
}
\examples{
check_waste_energy_fraction_acm0012(c(0.8, 0.7))
check_baseline_continuation_acm0012(7800, 7500)
check_metering_uncertainty_acm0012(0.01)
}
