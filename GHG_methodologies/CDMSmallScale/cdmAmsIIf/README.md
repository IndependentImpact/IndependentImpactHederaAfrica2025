# cdmAmsIIf

`cdmAmsIIf` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-II.F Energy efficiency and fuel switching measures for agricultural facilities**.

The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation wrappers to reproduce emission reduction estimates for agricultural energy efficiency interventions.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIf")
```

## Getting Started

``` r
library(cdmAmsIIf)

monitoring <- simulate_ams_iif_dataset(n_facilities = 2, n_periods = 6)
annual_monitoring <- aggregate_monitoring_periods_iif(
  monitoring,
  period_col = monitoring_label,
  group_cols = "facility_id"
)

applicability <- assess_ams_iif_applicability(
  baseline_data = annual_monitoring,
  project_data = annual_monitoring,
  monitoring_data = annual_monitoring
)

if (all(applicability$is_met)) {
  baseline <- calculate_baseline_agricultural_emissions(annual_monitoring, group_cols = "facility_id")
  project <- calculate_project_agricultural_emissions(annual_monitoring, group_cols = "facility_id")
  leakage <- calculate_leakage_emissions_iif(annual_monitoring,
    group_cols = "facility_id",
    leakage_col = "leakage_emissions_tco2e"
  )
  emission_reductions <- calculate_emission_reductions_iif(
    baseline,
    project,
    leakage,
    group_cols = "facility_id"
  )
  emission_reductions_meta <- estimate_emission_reductions_ams_iif(
    baseline_data = annual_monitoring,
    project_data = annual_monitoring,
    leakage_data = annual_monitoring,
    group_cols = "facility_id",
    leakage_args = list(leakage_col = "leakage_emissions_tco2e")
  )
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIIf-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-II.F requirements before emission reductions can be claimed. Use the package helpers to document each criterion:

- `check_applicability_energy_intensity_iif()` – verifies the project delivers a meaningful reduction in specific energy consumption relative to an agricultural output proxy.
- `check_applicability_fuel_switching_iif()` – ensures the blended emission factor of the project thermal fuel mix is no higher than the baseline mix when fuel switching is part of the activity.
- `check_applicability_monitoring_iif()` – confirms monitoring datasets contain the required energy, service, and operating parameters without missing values.

## Key Equations

`cdmAmsIIf` translates the numbered equations from AMS-II.F into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_agricultural_emissions()` | Aggregates baseline fossil fuel and electricity emissions for each facility. |
| (2) | `calculate_project_agricultural_emissions()` | Aggregates project fossil fuel and electricity emissions after efficiency measures. |
| (3) | `calculate_leakage_emissions_iif()` | Sums leakage components from upstream or market effects. |
| (4) | `calculate_emission_reductions_iif()` | Combines baseline, project, and leakage emissions to obtain net emission reductions. |

The meta-wrapper `estimate_emission_reductions_ams_iif()` chains these helpers for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods_iif()` summarises measured data across reporting periods while preserving facility-level identifiers and numeric totals.
- `simulate_ams_iif_dataset()` generates example datasets with monitoring metadata, baseline and project parameters, and leakage placeholders to support tests, demos, and onboarding.
