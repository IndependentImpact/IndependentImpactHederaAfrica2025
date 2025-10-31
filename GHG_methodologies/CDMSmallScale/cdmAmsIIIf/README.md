# cdmAmsIIIf

`cdmAmsIIIf` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-III.F Avoidance of methane emissions through composting**. The package follows tidyverse design principles and exposes equation-level helpers, applicability diagnostics, and workflow orchestration utilities to reproduce emission reduction estimates for composting facilities.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIIf")
```

## Getting Started

```r
library(cdmAmsIIIf)
library(dplyr)

simulated <- simulate_ams_iiif_dataset(n_sites = 2, n_periods = 3, seed = 2024)

applicability <- check_applicability_feedstock_management_iiif(simulated$applicability, group_cols = "site_id") |>
  left_join(
    check_applicability_composting_practices_iiif(simulated$applicability, group_cols = "site_id"),
    by = "site_id"
  ) |>
  left_join(
    check_applicability_monitoring_framework_iiif(simulated$applicability, group_cols = "site_id"),
    by = "site_id"
  )

reductions <- estimate_emission_reductions_ams_iiif(
  baseline_data = simulated$baseline,
  project_data = simulated$project,
  leakage_data = simulated$leakage,
  group_cols = "site_id",
  baseline_args = list(oxidation_factor_col = "baseline_oxidation_fraction"),
  project_args = list(compost_oxidation_col = "compost_oxidation_fraction")
)

applicability
reductions
```

For a more detailed walk-through see the vignette in `vignettes/cdmAmsIIIf-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-III.F requirements before emission reductions can be claimed. Use the package helpers to document each criterion:

- `check_applicability_feedstock_management_iiif()` – confirms waste types are eligible, source segregated, and within contamination limits.
- `check_applicability_composting_practices_iiif()` – validates that composting systems maintain aerobic conditions, retention time, and leachate management.
- `check_applicability_monitoring_framework_iiif()` – verifies monitoring covers organic content, temperature, and moisture checks.

## Key Equations

`cdmAmsIIIf` translates the numbered equations from AMS-III.F into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_methane_emissions_iiif()` | Estimates baseline methane generation from unmanaged disposal of organic waste. |
| (2) | `calculate_project_emissions_iiif()` | Converts controlled composting performance and energy inputs into project emissions. |
| (3) | `calculate_leakage_emissions_iiif()` | Aggregates transport, residual waste treatment, and fertiliser displacement leakage. |
| (4) | `calculate_emission_reductions_iiif()` | Determines net emission reductions after project and leakage deductions. |

The meta-wrapper `estimate_emission_reductions_ams_iiif()` chains these helpers together for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods_iiif()` summarises measured data across reporting periods while preserving entity identifiers.
- `simulate_ams_iiif_dataset()` generates example datasets with monitoring metadata to support tests, demos, and onboarding.
