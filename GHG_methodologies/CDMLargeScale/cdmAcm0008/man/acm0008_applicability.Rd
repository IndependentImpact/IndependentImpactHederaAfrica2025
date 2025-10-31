% Generated manually
\name{acm0008_applicability}
\alias{check_applicability_mine_type}
\alias{check_applicability_methane_content}
\alias{check_applicability_utilisation_pathway}
\title{Applicability checks for ACM0008}
\description{Helpers that validate mine configurations, methane concentration, and utilisation pathways for coal mine methane projects.}
\usage{
check_applicability_mine_type(mine_type, requires_drainage, allowed_types = c("underground", "surface", "post-closure"))

check_applicability_methane_content(methane_fraction, minimum_fraction = 0.3)

check_applicability_utilisation_pathway(pathway, enclosed_combustion, supported = c("electricity", "thermal", "flaring", "oxidation"))
}
\arguments{
\item{mine_type}{Character string naming the coal mine classification.}

\item{requires_drainage}{Logical flag indicating that methane drainage infrastructure is available.}

\item{allowed_types}{Character vector overriding the recognised mine categories.}

\item{methane_fraction}{Numeric vector of methane fractions expressed between 0 and 1.}

\item{minimum_fraction}{Numeric scalar giving the minimum admissible methane fraction.}

\item{pathway}{Character vector describing the utilisation or oxidation option.}

\item{enclosed_combustion}{Logical vector signalling whether combustion equipment is enclosed and monitored.}

\item{supported}{Character vector listing pathways supported by the methodology.}
}
\value{Logical vector(s) indicating whether the applicability condition is satisfied.}
\examples{
check_applicability_mine_type("underground", TRUE)
check_applicability_methane_content(c(0.32, 0.35))
check_applicability_utilisation_pathway(c("electricity", "venting"), c(TRUE, TRUE))
}
