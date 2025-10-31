# cdmAmsIIIh

`cdmAmsIIIh` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-III.H Methane recovery in wastewater treatment**. The package follows tidyverse design principles and exposes equation-level helpers, applicability diagnostics, and workflow orchestration utilities to reproduce emission reduction estimates for anaerobic wastewater projects.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIIh")
```

## Getting Started

```r
library(cdmAmsIIIh)
library(dplyr)

simulated <- simulate_ams_iiih_dataset(n_sites = 2, n_periods = 3, seed = 2025)

applicability <- check_applicability_wastewater_characteristics_iiih(simulated$applicability, group_cols = "site_id") %>%
  left_join(
    check_applicability_recovery_system_iiih(simulated$applicability, group_cols = "site_id"),
    by = "site_id"
  ) %>%
  left_join(
    check_applicability_monitoring_framework_iiih(simulated$applicability, group_cols = "site_id"),
    by = "site_id"
  )

reductions <- estimate_emission_reductions_ams_iiih(
  baseline_data = simulated$baseline,
  project_data = simulated$project,
  leakage_data = simulated$leakage,
  group_cols = "site_id",
  baseline_args = list(days_col = "days_in_period"),
  project_args = list(days_col = "days_in_period")
)

applicability
reductions
```

For a more detailed walk-through see the vignette in `vignettes/cdmAmsIIIh-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-III.H requirements before emission reductions can be claimed. Use the package helpers to document each criterion:

- `check_applicability_wastewater_characteristics_iiih()` – verifies wastewater type, COD concentration, anaerobic loading, and baseline system eligibility.
- `check_applicability_recovery_system_iiih()` – confirms compliant methane capture/destruction technologies, minimum operating hours, and supporting documentation.
- `check_applicability_monitoring_framework_iiih()` – ensures monitoring plans include sufficient flow, methane fraction, COD sampling, and calibration measurements.

## Key Equations

`cdmAmsIIIh` translates the numbered equations from AMS-III.H into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_methane_emissions_iiih()` | Estimates baseline methane emissions released without gas recovery. |
| (2) | `calculate_project_emissions_iiih()` | Converts project methane capture, destruction, and energy inputs into project emissions. |
| (3) | `calculate_leakage_emissions_iiih()` | Aggregates sludge transport, treatment, chemical use, and displacement impacts. |
| (4) | `calculate_emission_reductions_iiih()` | Computes net emission reductions after project and leakage adjustments. |

The meta-wrapper `estimate_emission_reductions_ams_iiih()` chains these helpers together for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods_iiih()` summarises measured data across reporting periods while preserving entity identifiers.
- `simulate_ams_iiih_dataset()` generates example datasets with monitoring metadata to support tests, demos, and onboarding.
