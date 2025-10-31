# cdmAmsIa

`cdmAmsIa` implements the Clean Development Mechanism (CDM) small-scale methodology **AMS-I.A Electricity generation by the user**.
The package follows tidyverse design principles and exposes equation-level helpers, applicability checks, and meta-calculation
wrappers to reproduce emission reduction estimates.

## Installation

```
# install.packages("devtools")
devtools::install_github("independent-impact/GHG_methodologies/cdmAmsIa")
```

## Getting Started

```
library(cdmAmsIa)

applicable <- check_applicability_installed_capacity(capacity_kw = 500, renewable_fraction = 1)
if (applicable) {
  baseline <- calculate_baseline_generation(tibble::tibble(user_id = 1, generation_kwh = 12000))
  emissions <- calculate_baseline_emissions(baseline, grid_emission_factor = 0.8)
  project <- calculate_project_emissions(baseline)
  emission_reductions <- calculate_emission_reductions(emissions, project)
}
```

For a full walk-through see the vignette in `vignettes/cdmAmsIa-methodology.Rmd`.
