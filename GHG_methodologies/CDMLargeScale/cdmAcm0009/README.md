# cdmAcm0009

The goal of `cdmAcm0009` is to provide a lightweight, tidyverse-oriented reference implementation of the CDM large-scale methodology **ACM0009** for fuel switching projects that convert existing coal or petroleum fired boilers and turbines to natural gas. The package includes:

- Applicability helpers to document baseline and project fuel eligibility.
- Equation functions translating the ACM0009 greenhouse gas accounting steps into reusable R code.
- Meta-estimators that aggregate monitoring period data and report total emission reductions.
- Simulation utilities for generating synthetic monitoring datasets suitable for demonstrations and testing.

## Installation

This repository is intended for analytical workflows and is not published to CRAN. You can install it using `devtools::install()` once the repository is cloned locally.

```r
# install.packages("devtools")
devtools::install("CDMLargeScale/cdmAcm0009")
```

## Example

```r
library(cdmAcm0009)

data <- simulate_acm0009_dataset(periods = 6, seed = 2024)
results <- estimate_emission_reductions_acm0009(data)
results$total_emission_reductions
```

Run `Rscript ../run_tests.R` from the `CDMLargeScale` folder to execute package tests across all large-scale methodologies.
