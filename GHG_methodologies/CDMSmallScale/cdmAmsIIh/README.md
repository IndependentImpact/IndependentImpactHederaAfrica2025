# cdmAmsIIh

`cdmAmsIIh` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-II.H Energy efficiency via centralization of utility provisions**. The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and simulation tools to reproduce emission reduction estimates for industrial facilities that consolidate steam, hot water, or chilled water production.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIIh")
```

## Getting Started

``` r
library(cdmAmsIIh)

inputs <- simulate_ams_iih_inputs(n_facilities = 2, seed = 123)

applicable <- all(
  check_applicability_centralization_scope_iih(
    baseline_summary = tibble::tibble(
      utility_service = c("steam", "hot_water"),
      baseline_unit_count = c(4, 3)
    ),
    project_summary = tibble::tibble(
      utility_service = c("steam", "hot_water"),
      project_unit_count = c(1, 1)
    )
  ),
  check_applicability_monitoring_iih(dplyr::bind_cols(inputs$baseline_data, inputs$project_data)),
  check_applicability_efficiency_improvement_iih(
    baseline_data = inputs$baseline_data,
    project_data = inputs$project_data,
    group_cols = "facility",
    minimum_improvement = 0.05
  )
)

if (applicable) {
  reductions <- estimate_emission_reductions_ams_iih(
    baseline_data = inputs$baseline_data,
    project_data = inputs$project_data,
    leakage_data = inputs$leakage_data,
    group_cols = "facility"
  )
  print(reductions)
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIIh-methodology.Rmd`.

## Applicability Conditions

Projects must satisfy core AMS-II.H requirements before emission reductions can be claimed. Use the package helpers to document each criterion:

- `check_applicability_centralization_scope_iih()` – confirms that decentralized utilities are consolidated into a central plant and that the number of generating units decreases.
- `check_applicability_monitoring_iih()` – verifies that the monitoring dataset contains the required baseline and project parameters without missing values.
- `check_applicability_efficiency_improvement_iih()` – confirms the centralized system delivers the minimum efficiency gain relative to the baseline.

## Key Equations

`cdmAmsIIh` translates the numbered equations from AMS-II.H into composable R functions:

| Equation | Function | Description |
|----------|----------|-------------|
| (1) | `calculate_baseline_decentralized_emissions_iih()` | Multiplies baseline fuel use by emission factors and reports optional specific energy diagnostics. |
| (2) | `calculate_project_central_emissions_iih()` | Computes emissions from the centralized utility plant and optional project specific energy. |
| (3) | `calculate_project_auxiliary_electricity_iih()` | Converts auxiliary electricity into emissions using grid emission factors. |
| (4) | `calculate_emission_reductions_iih()` | Derives emission reductions after subtracting project and leakage emissions. |

The meta-wrapper `estimate_emission_reductions_ams_iih()` chains these helpers together for tidyverse-friendly datasets.

## Monitoring and Simulation Utilities

- `simulate_ams_iih_inputs()` generates example baseline, project, and leakage datasets to support tests, demos, and onboarding.
