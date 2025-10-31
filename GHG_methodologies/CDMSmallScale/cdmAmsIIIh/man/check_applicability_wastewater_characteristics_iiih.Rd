\name{check_applicability_wastewater_characteristics_iiih}
\alias{check_applicability_wastewater_characteristics_iiih}
\title{Check wastewater characteristics for AMS-III.H applicability}
\usage{
check_applicability_wastewater_characteristics_iiih(
  data,
  wastewater_type_col = "wastewater_type",
  eligible_types = c("industrial", "agro-industrial", "domestic"),
  cod_concentration_col = "cod_mg_l",
  minimum_cod_mg_l = 2000,
  anaerobic_fraction_col = "anaerobic_fraction",
  minimum_anaerobic_fraction = 0.5,
  baseline_system_col = "baseline_system",
  excluded_baseline_systems = c(
    "covered anaerobic lagoon",
    "closed digester",
    "anaerobic reactor with flare"
  ),
  group_cols = NULL,
  output_col = "wastewater_applicable"
)
}
\arguments{
\item{data}{Tibble containing wastewater facility metadata.}

\item{wastewater_type_col}{Column storing the wastewater classification.}

\item{eligible_types}{Wastewater types that fall within the AMS-III.H scope.}

\item{cod_concentration_col}{Column storing influent COD concentration in mg/L.}

\item{minimum_cod_mg_l}{Minimum COD concentration required for applicability.}

\item{anaerobic_fraction_col}{Column storing the fraction of flow to anaerobic treatment.}

\item{minimum_anaerobic_fraction}{Minimum anaerobic fraction required for applicability.}

\item{baseline_system_col}{Column describing the baseline treatment system.}

\item{excluded_baseline_systems}{Baseline systems that already capture or destroy methane.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the logical output column summarising compliance.}
}
\value{Tibble summarising whether each grouping combination meets wastewater requirements.}
\description{
Confirms wastewater characteristics, baseline configuration, and organic loading
support AMS-III.H methane recovery projects.
}
\examples{
facilities <- tibble::tibble(
  site_id = c("WW1", "WW2"),
  wastewater_type = c("agro-industrial", "domestic"),
  cod_mg_l = c(3200, 1800),
  anaerobic_fraction = c(0.7, 0.3),
  baseline_system = c("open anaerobic lagoon", "covered anaerobic lagoon")
)
check_applicability_wastewater_characteristics_iiih(
  facilities,
  group_cols = "site_id"
)
}
