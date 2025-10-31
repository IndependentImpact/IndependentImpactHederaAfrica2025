test_that("non-renewable biomass calculation handles fractions", {
  data <- tibble::tibble(
    site = c("A", "A", "B"),
    biomass = c(10, 5, 12),
    non_renewable_fraction = c(0.8, 0.75, 0.9)
  )
  result <- calculate_non_renewable_biomass(
    data,
    consumption_col = "biomass",
    fraction_col = "non_renewable_fraction",
    group_cols = "site"
  )
  expect_equal(result$non_renewable_biomass_tonnes[result$site == "A"], (10 * 0.8) + (5 * 0.75))
  expect_equal(result$non_renewable_biomass_tonnes[result$site == "B"], 12 * 0.9)
})

test_that("baseline energy and emissions follow equations", {
  biomass <- tibble::tibble(site = c("A", "B"), non_renewable_biomass_tonnes = c(12, 15))
  energy <- calculate_baseline_energy_content(biomass, ncv = c(15, 14))
  expect_equal(energy$baseline_energy_mj, c(180, 210))

  emissions <- calculate_baseline_emissions(energy, emission_factor = 0.0001)
  expect_equal(emissions$baseline_emissions_tco2e, c(0.018, 0.021))
})

test_that("project emissions and reductions combine correctly", {
  project_energy <- tibble::tibble(project_energy_mj = c(100, 200))
  project <- calculate_project_emissions(project_energy, project_emission_factor = 0.00009)
  expect_equal(project$project_emissions_tco2e, c(0.009, 0.018))

  baseline <- tibble::tibble(baseline_emissions_tco2e = c(0.05, 0.07))
  reductions <- calculate_emission_reductions(baseline, project)
  expect_equal(reductions$emission_reductions_tco2e, c(0.041, 0.052))
})

test_that("meta wrapper produces expected columns", {
  monitoring <- tibble::tibble(
    site_id = c("A", "A", "B"),
    biomass_consumption_tonnes = c(10, 6, 12),
    non_renewable_fraction = c(0.8, 0.75, 0.9),
    project_energy_mj = c(100, 120, 140)
  )

  reductions <- estimate_emission_reductions_ams_ie(
    monitoring,
    group_cols = "site_id",
    ncv = 15,
    emission_factor = 0.0001,
    project_energy_col = "project_energy_mj",
    project_emission_factor = 0.00009
  )

  expect_true(all(c("baseline_emissions_tco2e", "project_emissions_tco2e", "emission_reductions_tco2e") %in% names(reductions)))
  expect_equal(nrow(reductions), 2)
})
