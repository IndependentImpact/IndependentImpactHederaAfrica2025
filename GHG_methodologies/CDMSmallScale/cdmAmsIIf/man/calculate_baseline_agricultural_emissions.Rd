\name{calculate_baseline_agricultural_emissions}
\alias{calculate_baseline_agricultural_emissions}
\title{Calculate baseline agricultural emissions (Equation 1)}
\usage{
calculate_baseline_agricultural_emissions(baseline_data,
                                                      fuel_energy_col = "baseline_fuel_energy_gj",
                                                      fuel_emission_factor_col = "baseline_fuel_emission_factor_tco2_per_gj",
                                                      electricity_col = "baseline_electricity_mwh",
                                                      electricity_emission_factor_col = "baseline_electricity_emission_factor_tco2_per_mwh",
                                                      group_cols = NULL,
                                                      output_col = "baseline_emissions_tco2e")
}
\arguments{
  \item{baseline_data}{Tidy tibble describing baseline energy use for the}
  \item{fuel_energy_col}{Column containing baseline fossil fuel energy in}
  \item{fuel_emission_factor_col}{Column storing baseline fossil fuel emission}
  \item{electricity_col}{Column storing baseline electricity consumption in}
  \item{electricity_emission_factor_col}{Column storing baseline electricity}
  \item{group_cols}{Optional character vector of grouping columns to retain in}
  \item{output_col}{Name of the column that will contain baseline emissions in}
}
\value{A tibble containing baseline emissions aggregated by the requested}
\description{Implements Equation (1) of AMS-II.F by converting baseline fossil fuel and
electricity consumption for an agricultural processing facility into
greenhouse gas emissions. The helper multiplies each energy stream by the
corresponding emission factor and aggregates the results using optional
grouping columns.
  facility or process line.
  gigajoules (GJ). Set to `NULL` when fossil fuels are absent.
  factors in tCO2e per GJ. Required when `fuel_energy_col` is provided.
  megawatt-hours (MWh). Set to `NULL` when electricity is not displaced.
  emission factors in tCO2e per MWh. Required when `electricity_col` is
  provided.
  the output.
  tonnes of CO2e. Defaults to `"baseline_emissions_tco2e"`.
  grouping structure.}
\examples{
baseline <- tibble::tibble(
  facility_id = c("rice_mill_1", "rice_mill_2"),
  baseline_fuel_energy_gj = c(980, 1120),
  baseline_fuel_emission_factor_tco2_per_gj = c(0.074, 0.071),
  baseline_electricity_mwh = c(180, 205),
  baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.58)
)
calculate_baseline_agricultural_emissions(baseline, group_cols = "facility_id")
}
