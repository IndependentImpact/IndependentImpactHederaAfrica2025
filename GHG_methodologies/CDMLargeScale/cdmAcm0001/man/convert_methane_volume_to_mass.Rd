% Generated manually
\name{convert_methane_volume_to_mass}
\alias{convert_methane_volume_to_mass}
\title{Convert methane volume to mass}
\usage{convert_methane_volume_to_mass(methane_volume_m3, methane_density_t_per_m3 = 0.000716)}
\arguments{
  \item{methane_volume_m3}{Numeric vector of methane volumes in cubic metres.}
  \item{methane_density_t_per_m3}{Density of methane in tonnes per cubic metre.}
}
\value{Numeric vector of methane mass in tonnes.}
\description{Translates captured landfill gas methane volumes into tonnes of methane using the ACM0001 default density.}
