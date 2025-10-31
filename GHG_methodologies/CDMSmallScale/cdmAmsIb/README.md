# cdmAmsIb

`cdmAmsIb` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-I.B Mechanical energy for the user**.
The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation
wrappers to reproduce emission reduction estimates for renewable mechanical systems that displace fossil-fuel equipment.

## Installation

```
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIb")
```

## Getting Started

```
library(cdmAmsIb)

applicable <- check_applicability_mechanical_capacity(capacity_kw = 750) &&
  check_applicability_renewable_driver(renewable_fraction = 0.95)

if (applicable) {
  fuel <- tibble::tibble(machine_id = "pump-1", fuel_consumption = 250, net_calorific_value = 43)
  baseline <- calculate_baseline_energy_content(fuel, group_cols = "machine_id")
  emissions <- calculate_baseline_emissions(baseline, emission_factor = 0.00007)
  project_energy <- tibble::tibble(machine_id = "pump-1", project_energy_mj = 0)
  project <- calculate_project_emissions(project_energy, energy_col = "project_energy_mj")
  emission_reductions <- calculate_emission_reductions(emissions, project)
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIb-methodology.Rmd`.
