# cdmAcm0002

`cdmAcm0002` implements the Clean Development Mechanism (CDM) large-scale methodology **ACM0002 Grid-connected electricity generation from renewable sources**. The package follows tidyverse design principles and exposes applicability checks, equation-level helpers, and meta-calculation wrappers to reproduce emission reduction estimates for grid-connected renewable power plants.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/CDMLargeScale/cdmAcm0002")
```

## Getting Started

```r
library(cdmAcm0002)

data <- simulate_acm0002_dataset(6, seed = 123)

if (check_applicability_grid_connection("grid-connected", export_share = 0.95) &&
    check_applicability_renewable_technology("wind")) {
  period_summary <- aggregate_monitoring_periods(data)
  totals <- estimate_emission_reductions_acm0002(data)
}
```

For a full walk-through see the vignette in `vignettes/cdmAcm0002-methodology.Rmd`.
