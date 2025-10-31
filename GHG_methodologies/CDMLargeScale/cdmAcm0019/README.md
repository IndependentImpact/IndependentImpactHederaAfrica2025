# cdmAcm0019

`cdmAcm0019` implements the Clean Development Mechanism (CDM) large-scale methodology **ACM0019 N2O abatement from nitric acid production**. The package offers tidyverse-friendly applicability checks, equation helpers, simulation utilities, and wrappers for estimating emission reductions for nitric acid plants equipped with secondary or tertiary abatement technologies.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/CDMLargeScale/cdmAcm0019")
```

## Getting Started

```r
library(cdmAcm0019)

monitoring <- simulate_acm0019_dataset(periods = 3, seed = 123)

if (check_applicability_monitoring_coverage(monitoring$data_capture_rate) &&
    all(check_applicability_catalyst_configuration(monitoring$catalyst_configuration)) &&
    check_applicability_operating_regime("weak-acid", TRUE)) {
  period_summary <- aggregate_monitoring_periods_acm0019(monitoring)
  totals <- estimate_emission_reductions_acm0019(monitoring)
}
```

For a step-by-step walkthrough of the methodology equations consult the vignette in `doc/cdmAcm0019-methodology.Rmd`.
