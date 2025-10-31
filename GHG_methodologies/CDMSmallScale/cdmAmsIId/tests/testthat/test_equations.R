test_that("baseline fossil emissions aggregate correctly", {
  data <- tibble::tibble(
    facility_id = c("A", "A", "B"),
    baseline_fuel_quantity = c(1200, 900, 800),
    baseline_ncv_gj_per_unit = c(0.038, 0.038, 0.041),
    baseline_emission_factor_tco2_per_gj = c(0.094, 0.094, 0.095),
    baseline_efficiency = c(0.72, 0.72, 0.7)
  )
  result <- calculate_baseline_fossil_emissions(data, group_cols = "facility_id")
  expect_equal(nrow(result), 2)
  expect_equal(
    result$baseline_emissions_tco2e[result$facility_id == "A"],
    sum((c(1200, 900) * 0.038 * 0.094) / 0.72),
    tolerance = 1e-10
  )
  expect_equal(
    result$baseline_emissions_tco2e[result$facility_id == "B"],
    (800 * 0.041 * 0.095) / 0.7,
    tolerance = 1e-10
  )
})

test_that("project fossil emissions include electricity", {
  data <- tibble::tibble(
    facility_id = c("A", "A", "B"),
    project_fuel_quantity = c(950, 710, 620),
    project_ncv_gj_per_unit = c(0.038, 0.038, 0.041),
    project_emission_factor_tco2_per_gj = c(0.084, 0.084, 0.086),
    project_efficiency = c(0.84, 0.84, 0.82),
    electricity_emissions_tco2e = c(0.6, 0.4, 0.5)
  )
  result <- calculate_project_fossil_emissions(
    data,
    group_cols = "facility_id",
    electricity_col = "electricity_emissions_tco2e"
  )
  expected_a <- sum((c(950, 710) * 0.038 * 0.084) / 0.84 + c(0.6, 0.4))
  expected_b <- (620 * 0.041 * 0.086) / 0.82 + 0.5
  expect_equal(result$project_emissions_tco2e[result$facility_id == "A"], expected_a, tolerance = 1e-10)
  expect_equal(result$project_emissions_tco2e[result$facility_id == "B"], expected_b, tolerance = 1e-10)
})

test_that("emission reductions subtract project and leakage", {
  baseline <- tibble::tibble(
    facility_id = c("A", "B"),
    baseline_emissions_tco2e = c(110, 95)
  )
  project <- tibble::tibble(
    facility_id = c("A", "B"),
    project_emissions_tco2e = c(70, 62)
  )
  leakage <- tibble::tibble(
    facility_id = c("A", "B"),
    leakage_emissions_tco2e = c(4, 6)
  )
  reductions <- estimate_emission_reductions(baseline, project, leakage, group_cols = "facility_id")
  expect_equal(reductions$emission_reductions_tco2e, c(36, 27))
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(
    facility_id = c("A", "A", "B", "B"),
    baseline_fuel_quantity = c(1200, 900, 1000, 850),
    baseline_ncv_gj_per_unit = c(0.038, 0.038, 0.041, 0.041),
    baseline_emission_factor_tco2_per_gj = c(0.094, 0.094, 0.095, 0.095),
    baseline_efficiency = c(0.72, 0.72, 0.7, 0.7),
    project_fuel_quantity = c(950, 720, 810, 690),
    project_ncv_gj_per_unit = c(0.038, 0.038, 0.041, 0.041),
    project_emission_factor_tco2_per_gj = c(0.084, 0.084, 0.086, 0.086),
    project_efficiency = c(0.84, 0.84, 0.82, 0.82),
    electricity_emissions_tco2e = c(0.6, 0.4, 0.5, 0.3),
    leakage_emissions_tco2e = c(5, 4, 6, 5)
  )
  baseline <- calculate_baseline_fossil_emissions(data, group_cols = "facility_id")
  project <- calculate_project_fossil_emissions(
    data,
    group_cols = "facility_id",
    electricity_col = "electricity_emissions_tco2e"
  )
  leakage <- calculate_leakage_emissions(
    data,
    group_cols = "facility_id",
    leakage_col = "leakage_emissions_tco2e"
  )
  manual <- estimate_emission_reductions(baseline, project, leakage, group_cols = "facility_id")
  wrapper <- estimate_emission_reductions_ams_iid(
    data,
    data,
    data,
    group_cols = "facility_id",
    project_args = list(electricity_col = "electricity_emissions_tco2e"),
    leakage_args = list(leakage_col = "leakage_emissions_tco2e")
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("monitoring aggregation preserves totals", {
  data <- simulate_ams_iid_dataset(n_facilities = 2, n_periods = 3)
  aggregated <- aggregate_monitoring_periods(
    data,
    period_col = monitoring_label,
    group_cols = "facility_id"
  )

  expect_true(all(c("facility_id", "monitoring_period") %in% names(aggregated)))
  expect_equal(sum(aggregated$baseline_fuel_quantity), sum(data$baseline_fuel_quantity))
  expect_equal(sum(aggregated$project_fuel_quantity), sum(data$project_fuel_quantity))
})
