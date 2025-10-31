\name{assess_ams_iif_applicability}
\alias{assess_ams_iif_applicability}
\title{Assess AMS-II.F applicability conditions collectively}
\usage{
assess_ams_iif_applicability(baseline_data,
                                         project_data,
                                         monitoring_data,
                                         intensity_args = list(),
                                         fuel_switch_args = list(),
                                         monitoring_args = list())
}
\arguments{
  \item{baseline_data}{Tibble containing baseline inputs.}
  \item{project_data}{Tibble containing project inputs.}
  \item{monitoring_data}{Tibble containing monitoring dataset.}
  \item{intensity_args}{Optional list of arguments passed to}
  \item{fuel_switch_args}{Optional list of arguments passed to}
  \item{monitoring_args}{Optional list of arguments passed to}
}
\value{A tibble with the columns `criterion` and `is_met`.}
\description{Bundles the individual applicability checks into a single helper that returns a
tidy summary of whether each criterion is satisfied. This wrapper is useful for
pre-screening project concepts prior to detailed calculations.
  [check_applicability_energy_intensity_iif()].
  [check_applicability_fuel_switching_iif()].
  [check_applicability_monitoring_iif()].}
\examples{
baseline <- tibble::tibble(
  baseline_total_energy_mwh = 220,
  service_level_indicator = 420,
  baseline_thermal_energy_gj = 320,
  baseline_thermal_emission_factor_tco2_per_gj = 0.072
)
project <- tibble::tibble(
  project_total_energy_mwh = 150,
  service_level_indicator = 420,
  project_thermal_energy_gj = 220,
  project_thermal_emission_factor_tco2_per_gj = 0.045
)
monitoring <- tibble::tibble(
  project_total_energy_mwh = 12.5,
  service_level_indicator = 0.35,
  operating_hours = 220
)
assess_ams_iif_applicability(baseline, project, monitoring)
}
