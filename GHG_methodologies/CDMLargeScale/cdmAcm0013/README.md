
# cdmAcm0013

`cdmAcm0013` implements the Clean Development Mechanism (CDM) large-scale methodology **ACM0013 New grid-connected fossil fuel plants using a less greenhouse gas intensive technology**. The package follows tidyverse idioms and provides applicability helpers, fossil generation emission equations, simulation tools, and end-to-end estimators for efficient fossil-fuel power projects replacing more carbon intensive grid generation.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/CDMLargeScale/cdmAcm0013")
```

## Getting Started

```r
library(cdmAcm0013)

dataset <- simulate_acm0013_dataset(periods = 3, seed = 42)

if (check_applicability_acm0013(dataset)) {
  period_summary <- aggregate_monitoring_periods_acm0013(dataset)
  totals <- estimate_emission_reductions_acm0013(dataset)
}
```

For a step-by-step walkthrough of the methodology equations consult the vignette in `vignettes/cdmAcm0013-methodology.Rmd`.
