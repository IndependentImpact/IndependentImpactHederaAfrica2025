# cdmAmsIe

`cdmAmsIe` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-I.E Switch from non-renewable biomass for thermal applications by the user**.
The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation wrappers to reproduce emission reduction estimates for renewable thermal technologies.

## Installation

```
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIe")
```

## Getting Started

```
library(cdmAmsIe)

applicable <- all(
  check_applicability_capacity_limit(thermal_capacity_mw = 20),
  check_applicability_non_renewable_fraction(c(0.8, 0.85)),
  check_applicability_project_renewable_fraction(renewable_fraction = 0.95)
)

if (applicable) {
  monitoring <- tibble::tibble(
    site_id = c("cook-1", "cook-1", "cook-2"),
    biomass_consumption_tonnes = c(10, 8, 12),
    non_renewable_fraction = c(0.85, 0.8, 0.9),
    project_energy_mj = c(100, 120, 130)
  )
  emission_reductions <- estimate_emission_reductions_ams_ie(
    monitoring,
    group_cols = "site_id",
    ncv = 15,
    emission_factor = 0.0001,
    project_energy_col = "project_energy_mj",
    project_emission_factor = 0.00009
  )
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIe-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-I.E requirements before emission reductions can be claimed. Use the
package helpers to document each criterion:

- `check_applicability_capacity_limit()` – enforces the 45 MWth Type I capacity threshold for renewable thermal activities.
- `check_applicability_non_renewable_fraction()` – validates that the baseline biomass contains a demonstrable non-renewable share.
- `check_applicability_project_renewable_fraction()` – confirms the project technology relies predominantly on renewable energy.

## Key Equations

`cdmAmsIe` translates the numbered equations from AMS-I.E into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_non_renewable_biomass()` | Applies the non-renewable fraction to baseline biomass consumption. |
| (2) | `calculate_baseline_energy_content()` | Converts non-renewable biomass into thermal energy using the NCV. |
| (3) | `calculate_baseline_emissions()` | Multiplies baseline energy by the baseline emission factor. |
| (4) | `calculate_project_emissions()` | Converts residual fossil energy into project emissions. |
| (5) | `calculate_emission_reductions()` | Derives emission reductions by subtracting project emissions from baseline emissions. |

The meta-wrapper `estimate_emission_reductions_ams_ie()` chains these helpers together for
tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `aggregate_monitoring_periods()` summarises measured data across reporting periods while preserving entity identifiers and emission factors.
- `simulate_ams_ie_dataset()` generates example datasets with monitoring metadata to support tests, demos, and onboarding.
