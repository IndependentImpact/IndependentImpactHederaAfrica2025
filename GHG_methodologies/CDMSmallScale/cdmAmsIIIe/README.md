# cdmAmsIIIe

`cdmAmsIIIe` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-III.E Avoidance of methane production from decay of biomass (combustion / gasification / mechanical-thermal treatment)**. The package follows tidyverse design principles and exposes equation-level helpers, applicability diagnostics, and workflow orchestration utilities to reproduce emission reduction estimates for biomass treatment facilities.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIIe")
```

## Getting Started

```r
library(cdmAmsIIIe)
library(dplyr)

simulated <- simulate_ams_iiie_dataset(n_plants = 2, n_periods = 3, seed = 2024)

applicability <- simulated$applicability |>
  check_applicability_feedstock_characteristics_iiie(group_cols = "plant_id") |>
  left_join(
    check_applicability_biomass_control_iiie(simulated$applicability, group_cols = "plant_id"),
    by = "plant_id"
  ) |>
  left_join(
    check_applicability_monitoring_practices_iiie(simulated$applicability, group_cols = "plant_id"),
    by = "plant_id"
  )

reductions <- estimate_emission_reductions_ams_iiie(
  baseline_data = simulated$baseline,
  project_data = simulated$project,
  leakage_data = simulated$leakage,
  group_cols = "plant_id"
)

applicability
reductions
```

For a more detailed walk-through see the vignette in `vignettes/cdmAmsIIIe-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-III.E requirements before emission reductions can be claimed. Use the package helpers to document each criterion:

- `check_applicability_feedstock_characteristics_iiie()` – confirms feedstock is eligible and sufficiently controlled.
- `check_applicability_biomass_control_iiie()` – validates that biomass would have decayed anaerobically without the project.
- `check_applicability_monitoring_practices_iiie()` – verifies monitoring covers energy output, moisture, and operating hours requirements.

## Key Equations

`cdmAmsIIIe` translates the numbered equations from AMS-III.E into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_methane_emissions_iiie()` | Estimates baseline methane generation from unmanaged biomass decay. |
| (2) | `calculate_project_emissions_iiie()` | Converts project treatment efficiency, auxiliary fuel, and methane slip into project emissions. |
| (3) | `calculate_leakage_emissions_iiie()` | Aggregates transport and alternative use leakage emissions. |
| (4) | `calculate_emission_reductions_iiie()` | Determines net emission reductions after project and leakage deductions. |

The meta-wrapper `estimate_emission_reductions_ams_iiie()` chains these helpers together for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods_iiie()` summarises measured data across reporting periods while preserving entity identifiers.
- `simulate_ams_iiie_dataset()` generates example datasets with monitoring metadata to support tests, demos, and onboarding.
