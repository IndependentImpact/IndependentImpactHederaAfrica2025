test_that("simulation emits baseline, project, and leakage tables", {
  inputs <- simulate_ams_iih_inputs(n_facilities = 3, seed = 101)
  expect_named(inputs, c("baseline_data", "project_data", "leakage_data"))
  expect_equal(nrow(inputs$baseline_data), 3)
  expect_equal(nrow(inputs$project_data), 3)
  expect_equal(nrow(inputs$leakage_data), 3)

  expect_true(all(inputs$baseline_data$baseline_fuel_use_gj > 0))
  expect_true(all(inputs$project_data$project_fuel_use_gj > 0))
  expect_true(all(inputs$project_data$project_auxiliary_electricity_mwh >= 0))
  expect_true(all(inputs$leakage_data$leakage_emissions_tco2e >= 0))
})

test_that("simulation outputs feed the meta workflow", {
  inputs <- simulate_ams_iih_inputs(n_facilities = 2, seed = 202)

  reductions <- estimate_emission_reductions_ams_iih(
    baseline_data = inputs$baseline_data,
    project_data = inputs$project_data,
    leakage_data = inputs$leakage_data,
    group_cols = "facility"
  )

  expect_equal(nrow(reductions), 2)
  expect_true(all(!is.na(reductions$emission_reductions_tco2e)))
  expect_true(all(reductions$baseline_emissions_tco2e > reductions$project_emissions_tco2e))
})
