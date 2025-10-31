library(tibble)

test_that("baseline emissions aggregate correctly", {
  data <- tibble(
    plant_id = c("A", "A"),
    biomass_tonnes = c(1000, 800),
    methane_potential_m3_per_tonne = c(120, 110),
    anaerobic_decay_fraction = 0.6,
    days_in_period = 365
  )
  result <- calculate_baseline_methane_emissions_iiie(data, group_cols = "plant_id")
  expected <- sum(data$biomass_tonnes * data$methane_potential_m3_per_tonne * 0.6 * 365 * 0.00067 * 28 / 365)
  expect_equal(result$baseline_emissions_tco2e, expected)
})

test_that("project emissions include methane slip and fuels", {
  data <- tibble(
    plant_id = "A",
    treated_biomass_tonnes = 900,
    methane_potential_m3_per_tonne = 115,
    methane_slip_fraction = 0.05,
    days_in_period = 365,
    auxiliary_fuel_consumption_tj = 1.1,
    auxiliary_fuel_ef_tco2_per_tj = 70,
    backup_consumption_tj = 0.2,
    backup_ef_tco2_per_tj = 72
  )
  result <- calculate_project_emissions_iiie(
    data,
    fossil_backup_consumption_col = "backup_consumption_tj",
    fossil_backup_ef_col = "backup_ef_tco2_per_tj"
  )
  methane_component <- 900 * 115 * 0.05 * 365 * 0.00067 * 28 / 365
  fuel_component <- 1.1 * 70 + 0.2 * 72
  expect_equal(result$project_emissions_tco2e, methane_component + fuel_component)
})

test_that("leakage emissions sum components", {
  data <- tibble(
    biomass_transported_tonnes = 500,
    transport_distance_km = 40,
    transport_ef_tco2_per_tkm = 0.0001,
    alternative_use_fraction = 0.1,
    alternative_use_ef_tco2_per_tonne = 0.5
  )
  result <- calculate_leakage_emissions_iiie(data)
  expected <- 500 * 40 * 0.0001 + 500 * 0.1 * 0.5
  expect_equal(result$leakage_emissions_tco2e, expected)
})

test_that("emission reduction helper merges datasets", {
  baseline <- tibble(plant_id = "A", baseline_emissions_tco2e = 20)
  project <- tibble(plant_id = "A", project_emissions_tco2e = 5)
  leakage <- tibble(plant_id = "A", leakage_emissions_tco2e = 1)
  reductions <- calculate_emission_reductions_iiie(
    baseline,
    project,
    leakage,
    group_cols = "plant_id"
  )
  expect_equal(reductions$net_emission_reductions_tco2e, 14)
})

test_that("meta workflow produces expected columns", {
  monitoring <- simulate_ams_iiie_dataset(n_plants = 2, n_periods = 2, seed = 99)
  reductions <- estimate_emission_reductions_ams_iiie(
    baseline_data = monitoring$baseline,
    project_data = monitoring$project,
    leakage_data = monitoring$leakage,
    group_cols = "plant_id"
  )
  expect_true(all(c(
    "baseline_emissions_tco2e",
    "project_emissions_tco2e",
    "leakage_emissions_tco2e",
    "net_emission_reductions_tco2e"
  ) %in% colnames(reductions)))
})
