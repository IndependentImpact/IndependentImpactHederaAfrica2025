% Generated manually for cdmAmsIIIg
\name{check_applicability_landfill_characteristics_iiig}
\alias{check_applicability_landfill_characteristics_iiig}
\title{Check landfill characteristics for AMS-III.G applicability}
\description{
Verify landfill management status, biodegradable fraction, and readiness for
gas collection to confirm AMS-III.G applicability.
}
\usage{
check_applicability_landfill_characteristics_iiig(
  data,
  landfill_status_col = "landfill_status",
  eligible_statuses = c("open dump", "controlled landfill", "sanitary landfill"),
  biodegradable_fraction_col = "biodegradable_fraction",
  minimum_biodegradable_fraction = 0.4,
  gas_collection_ready_col = "gas_collection_ready",
  group_cols = NULL,
  output_col = "landfill_applicable"
)
}
\arguments{
\item{data}{Tibble containing landfill metadata.}

\item{landfill_status_col}{Column storing the landfill management status.}

\item{eligible_statuses}{Character vector listing eligible landfill
statuses.}

\item{biodegradable_fraction_col}{Column storing the biodegradable
fraction of incoming waste.}

\item{minimum_biodegradable_fraction}{Minimum biodegradable fraction
required for applicability.}

\item{gas_collection_ready_col}{Column storing logical indicators that gas
collection infrastructure can be installed.}

\item{group_cols}{Optional character vector specifying grouping columns.}

\item{output_col}{Name of the logical output column summarising
compliance.}
}
\value{
Tibble indicating whether each grouping combination satisfies landfill
requirements.
}
\examples{
metadata <- tibble::tibble(
  site_id = c("LF1", "LF2"),
  landfill_status = c("open dump", "sanitary landfill"),
  biodegradable_fraction = c(0.55, 0.32),
  gas_collection_ready = c(TRUE, FALSE)
)
check_applicability_landfill_characteristics_iiig(metadata, group_cols = "site_id")
}
