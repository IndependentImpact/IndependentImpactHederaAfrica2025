test_that("simulation produces expected columns", {
  inputs <- simulate_ams_iiq_inputs(n_buildings = 3, seed = 101)

  expect_named(inputs, c("baseline_data", "project_data", "leakage_data"))
  expect_equal(nrow(inputs$baseline_data), 3)
  expect_equal(nrow(inputs$project_data), 3)
  expect_equal(nrow(inputs$leakage_data), 3)

  expect_true(all(c(
    "building_id",
    "baseline_energy_use_mwh",
    "baseline_emission_factor_tco2_per_mwh",
    "baseline_service_output_mwh"
  ) %in% names(inputs$baseline_data)))

  expect_true(all(c(
    "building_id",
    "project_energy_use_mwh",
    "project_emission_factor_tco2_per_mwh",
    "project_service_output_mwh",
    "project_onsite_energy_gj",
    "project_onsite_emission_factor_tco2_per_gj"
  ) %in% names(inputs$project_data)))

  expect_true(all(c("building_id", "leakage_emissions_tco2e") %in% names(inputs$leakage_data)))
})

test_that("simulation integrates with meta workflow", {
  inputs <- simulate_ams_iiq_inputs(n_buildings = 2, seed = 202)

  reductions <- estimate_emission_reductions_ams_iiq(
    baseline_data = inputs$baseline_data,
    project_data = inputs$project_data,
    leakage_data = inputs$leakage_data,
    group_cols = "building_id"
  )

  expect_true("emission_reductions_tco2e" %in% names(reductions))
  expect_equal(nrow(reductions), 2)
})
