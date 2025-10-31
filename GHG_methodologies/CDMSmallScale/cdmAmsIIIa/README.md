# cdmAmsIIIa

`cdmAmsIIIa` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-III.A Offsetting of synthetic nitrogen fertilizers by inoculant application in leguminous crops**. The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation wrappers to reproduce emission reduction estimates for biological nitrogen fixation programmes.

## Installation

```r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIIa")
```

## Getting Started

```r
library(cdmAmsIIIa)

monitoring <- simulate_ams_iiia_dataset(n_farms = 2, n_periods = 3, seed = 2024)

applicable <- all(
  check_applicability_legume_share_iiia(
    monitoring,
    legume_area_col = "legume_area_ha",
    total_area_col = "total_area_ha",
    group_cols = "farm_id"
  )$legume_share_applicable,
  check_applicability_inoculant_registration_iiia(
    monitoring,
    registration_col = "inoculant_registered",
    group_cols = "farm_id"
  )$inoculant_registration_applicable,
  check_applicability_monitoring_practices_iiia(
    monitoring,
    monitoring_unit_cols = "farm_id"
  )$monitoring_practices_applicable
)

if (applicable) {
  reductions <- estimate_emission_reductions_ams_iiia(
    baseline_data = monitoring,
    project_data = monitoring,
    leakage_data = monitoring,
    group_cols = "farm_id",
    leakage_col = "leakage_emissions_tco2e"
  )
  print(reductions)
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIIIa-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-III.A requirements before emission reductions can be claimed. Use the package helpers to document each criterion:

- `check_applicability_legume_share_iiia()` – confirms legumes occupy a sufficient share of cultivated area.
- `check_applicability_inoculant_registration_iiia()` – validates that inoculants are registered and quality-assured.
- `check_applicability_monitoring_practices_iiia()` – verifies the monitoring design captures at least three periods per management unit.

## Key Equations

`cdmAmsIIIa` translates the numbered equations from AMS-III.A into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_fertilizer_emissions_iiia()` | Multiplies synthetic nitrogen use by production and field emission factors. |
| (2) | `calculate_project_residual_fertilizer_emissions_iiia()` | Applies the same structure to residual fertilizer use during the project. |
| (3) | `calculate_project_inoculant_emissions_iiia()` | Converts inoculant application rates and areas into project emissions. |
| (4) | `calculate_leakage_emissions_iiia()` | Aggregates leakage emissions linked to fertilizer displacement. |
| (5) | `calculate_emission_reductions_iiia()` | Derives emission reductions after subtracting project and leakage emissions. |

The meta-wrapper `estimate_emission_reductions_ams_iiia()` chains these helpers together for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods_iiia()` summarises measured data across reporting periods while preserving entity identifiers.
- `simulate_ams_iiia_dataset()` generates example datasets with monitoring metadata to support tests, demos, and onboarding.
