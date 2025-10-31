# cdmAmsIIId

`cdmAmsIIId` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-III.D Methane recovery in animal manure management systems**. The package provides tidyverse-first equation helpers, applicability checks, and orchestration utilities to reproduce methane recovery baselines, project emissions, and emission reductions for livestock waste projects.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIId")
```

## Getting Started

``` r
library(cdmAmsIIId)

monitoring <- simulate_ams_iiid_dataset(n_farms = 2, n_periods = 4, seed = 2024)

applicable <- all(
  check_applicability_system_type_iiid(monitoring, group_cols = "farm_id")$system_type_applicable,
  check_applicability_measurement_frequency_iiid(monitoring, group_cols = "farm_id")$measurement_frequency_applicable,
  check_applicability_leakage_control_iiid(monitoring, group_cols = "farm_id")$leakage_controls_applicable
)

if (applicable) {
  reductions <- estimate_emission_reductions_ams_iiid(
    baseline_data = monitoring,
    project_data = monitoring,
    group_cols = "farm_id"
  )
  print(reductions)
}
```

Explore the vignette in `vignettes/cdmAmsIIId-methodology.Rmd` for a full walkthrough of the workflow.

## Applicability Conditions

Use the included helpers to document AMS-III.D applicability:

- `check_applicability_system_type_iiid()` – verifies that monitored systems are eligible anaerobic manure management technologies.
- `check_applicability_measurement_frequency_iiid()` – ensures methane recovery is monitored at least weekly.
- `check_applicability_leakage_control_iiid()` – confirms methane capture infrastructure includes leakage controls.

## Key Equations

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_methane_emissions_iiid()` | Baseline methane emissions from unmanaged manure treatment. |
| (2) | `calculate_project_methane_emissions_iiid()` | Project emissions after recovery and destruction efficiencies. |
| (3) | `calculate_recovered_methane_iiid()` | Converts monitored methane recovery into destroyed tCO2e. |
| (4) | `calculate_emission_reductions_iiid()` | Net emission reductions after leakage adjustments. |

The meta-wrapper `estimate_emission_reductions_ams_iiid()` combines the helpers for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods_iiid()` summarises monitoring data across periods while preserving grouping identifiers.
- `simulate_ams_iiid_dataset()` generates example datasets with methane recovery metadata for demonstrations and tests.
