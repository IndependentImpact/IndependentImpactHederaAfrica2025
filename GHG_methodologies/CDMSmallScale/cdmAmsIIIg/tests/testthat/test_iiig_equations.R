library(tibble)

test_that("baseline emissions aggregate residual methane", {
  data <- tibble(
    site_id = c("LF1", "LF1", "LF2"),
    waste_disposed_tonnes = c(1000, 1200, 900),
    methane_generation_potential_m3_per_tonne = c(90, 90, 85),
    baseline_collection_efficiency_fraction = c(0.05, 0.05, 0.08),
    oxidation_fraction = c(0.1, 0.1, 0.08),
    days_in_period = 30
  )
  result <- calculate_baseline_methane_emissions_iiig(
    data,
    group_cols = "site_id",
    days_col = "days_in_period"
  )
  expected <- c(
    LF1 = 95276.41200000001,
    LF2 = 36441.07488
  )
  expect_equal(result$baseline_emissions_tco2e, unname(expected[result$site_id]))
})

test_that("project emissions capture residual methane and energy inputs", {
  data <- tibble(
    site_id = c("LF1", "LF1", "LF2"),
    waste_disposed_tonnes = c(1000, 1200, 900),
    methane_generation_potential_m3_per_tonne = c(90, 90, 85),
    project_collection_efficiency_fraction = c(0.72, 0.7, 0.68),
    destruction_efficiency_fraction = c(0.98, 0.97, 0.97),
    oxidation_fraction = c(0.08, 0.07, 0.09),
    days_in_period = 30,
    electricity_consumption_mwh = c(25, 28, 21),
    electricity_ef_tco2_per_mwh = c(0.5, 0.48, 0.52),
    diesel_consumption_litres = c(900, 950, 780),
    diesel_ef_tco2_per_litre = c(0.0027, 0.0028, 0.00275)
  )
  result <- calculate_project_emissions_iiig(
    data,
    group_cols = "site_id",
    days_col = "days_in_period"
  )
  expected <- c(
    LF1 = 32043.094,
    LF2 = 13428.753720000002
  )
  expect_equal(result$project_emissions_tco2e, unname(expected[result$site_id]))
})

test_that("leakage emissions aggregate transport, residual, and displacement", {
  data <- tibble(
    site_id = c("LF1", "LF1", "LF2"),
    waste_transported_tonnes = c(400, 380, 320),
    transport_distance_km = c(30, 25, 28),
    transport_ef_tco2_per_tkm = c(0.0001, 0.00011, 0.00009),
    residual_waste_tonnes = c(150, 120, 110),
    residual_waste_ef_tco2_per_tonne = c(0.3, 0.28, 0.32),
    displaced_electricity_mwh = c(8, 7, 6),
    displaced_electricity_ef_tco2_per_mwh = c(0.5, 0.48, 0.46)
  )
  result <- calculate_leakage_emissions_iiig(data, group_cols = "site_id")
  expected <- c(
    LF1 = 73.48500000000001,
    LF2 = 33.2464
  )
  expect_equal(result$leakage_emissions_tco2e, unname(expected[result$site_id]))
})

test_that("emission reduction helper merges grouped outputs", {
  baseline <- tibble(site_id = "LF1", baseline_emissions_tco2e = 100)
  project <- tibble(site_id = "LF1", project_emissions_tco2e = 40)
  leakage <- tibble(site_id = "LF1", leakage_emissions_tco2e = 5)
  reductions <- calculate_emission_reductions_iiig(
    baseline,
    project,
    leakage,
    group_cols = "site_id"
  )
  expect_equal(reductions$net_emission_reductions_tco2e, 55)
})

test_that("meta workflow returns expected structure", {
  monitoring <- simulate_ams_iiig_dataset(n_sites = 2, n_periods = 2, seed = 123)
  reductions <- estimate_emission_reductions_ams_iiig(
    baseline_data = monitoring$baseline,
    project_data = monitoring$project,
    leakage_data = monitoring$leakage,
    group_cols = "site_id",
    baseline_args = list(days_col = "days_in_period"),
    project_args = list(days_col = "days_in_period")
  )
  expect_true(all(c(
    "baseline_emissions_tco2e",
    "project_emissions_tco2e",
    "leakage_emissions_tco2e",
    "net_emission_reductions_tco2e"
  ) %in% colnames(reductions)))
})
