# cdmAmsId

`cdmAmsId` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-I.D Renewable electricity generation for a mini-grid**.
The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation
wrappers to reproduce emission reduction estimates for renewable captive grids.

## Installation

```
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsId")
```

## Getting Started

```
library(cdmAmsId)

applicable <- all(
  check_applicability_mini_grid_capacity(capacity_kw = 12000),
  check_applicability_renewable_penetration(renewable_fraction = 0.8),
  check_applicability_baseline_fossil_share(baseline_fossil_share = 0.7)
)

if (applicable) {
  supply <- tibble::tibble(grid_id = 1, electricity_mwh = 950)
  baseline <- calculate_baseline_electricity_supply(supply)
  emissions <- calculate_baseline_emissions(baseline, baseline_emission_factor = 0.7)
  project <- calculate_project_emissions(baseline, project_emission_factor = 0.05)
  emission_reductions <- calculate_emission_reductions(emissions, project)
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsId-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-I.D requirements before emission reductions can be claimed. Use the
package helpers to document each criterion:

- `check_applicability_mini_grid_capacity()` – verifies the system remains under the 15 MW small-scale
  threshold for Type I activities.
- `check_applicability_renewable_penetration()` – confirms that renewable resources supply the majority
  of electricity delivered to the mini-grid.
- `check_applicability_baseline_fossil_share()` – ensures the project displaces a meaningful share of
  fossil-based electricity in the baseline scenario.

## Key Equations

`cdmAmsId` translates the numbered equations from AMS-I.D into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_electricity_supply()` | Sums electricity delivered to the mini-grid during the baseline period. |
| (2) | `calculate_baseline_emissions()` | Applies the baseline emission factor to estimate displaced fossil electricity. |
| (3) | `calculate_project_emissions()` | Estimates project emissions from auxiliary fossil generation. |
| (4) | `calculate_emission_reductions()` | Derives emission reductions by subtracting project emissions from baseline emissions. |

The meta-wrapper `estimate_emission_reductions_ams_id()` chains these helpers together for
tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods()` summarises measured data across reporting periods while
  preserving grid-level identifiers and emission factors.
- `simulate_ams_id_dataset()` generates example datasets with monitoring metadata to support tests,
  demos, and onboarding.
