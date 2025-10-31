test_that("simulation emits methane recovery monitoring columns", {
  data <- simulate_ams_iiid_dataset(n_farms = 2, n_periods = 3, seed = 321)
  expect_equal(nrow(data), 6)
  required <- c(
    "farm_id",
    "monitoring_period",
    "volatile_solids_kg_per_day",
    "methane_potential_m3_per_kg_vs",
    "baseline_mcf_fraction",
    "project_mcf_fraction",
    "capture_efficiency_fraction",
    "destruction_efficiency_fraction",
    "methane_recovered_m3",
    "leakage_emissions_tco2e",
    "system_type",
    "measurements_per_month",
    "leakage_controls_in_place"
  )
  expect_true(all(required %in% names(data)))
  expect_true(all(data$baseline_mcf_fraction > data$project_mcf_fraction))
})

test_that("aggregation helper rolls period data to farm level", {
  data <- simulate_ams_iiid_dataset(n_farms = 3, n_periods = 2, seed = 99)
  aggregated <- aggregate_monitoring_periods_iiid(data, group_cols = "farm_id")
  expect_equal(nrow(aggregated), 3)
  expect_true(all(aggregated$volatile_solids_kg_per_day > 0))
  expect_true(all(aggregated$methane_potential_m3_per_kg_vs > 0))
})

test_that("aggregated data feeds emission reduction workflow", {
  data <- simulate_ams_iiid_dataset(n_farms = 1, n_periods = 3, seed = 55)
  aggregated <- aggregate_monitoring_periods_iiid(data, group_cols = "farm_id")

  reductions <- estimate_emission_reductions_ams_iiid(
    baseline_data = aggregated,
    project_data = aggregated,
    recovery_data = aggregated,
    leakage_data = aggregated,
    group_cols = "farm_id"
  )

  expect_equal(nrow(reductions), 1)
  expect_true(all(!is.na(reductions$emission_reductions_tco2e)))
})
