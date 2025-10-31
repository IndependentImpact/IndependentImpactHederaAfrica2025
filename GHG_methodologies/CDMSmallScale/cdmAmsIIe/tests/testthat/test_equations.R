test_that("baseline building emissions aggregate correctly", {
  data <- tibble::tibble(
    building_id = c("A", "A", "B"),
    baseline_thermal_energy_gj = c(420, 380, 310),
    baseline_thermal_emission_factor_tco2_per_gj = c(0.058, 0.056, 0.06),
    baseline_electricity_mwh = c(120, 95, 88),
    baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.6)
  )
  result <- calculate_baseline_building_emissions(data, group_cols = "building_id")
  expect_equal(nrow(result), 2)
  expected_a <- sum(c(420, 380) * c(0.058, 0.056) + c(120, 95) * 0.62)
  expected_b <- 310 * 0.06 + 88 * 0.6
  expect_equal(result$baseline_emissions_tco2e[result$building_id == "A"], expected_a, tolerance = 1e-10)
  expect_equal(result$baseline_emissions_tco2e[result$building_id == "B"], expected_b, tolerance = 1e-10)
})

test_that("project building emissions aggregate correctly", {
  data <- tibble::tibble(
    building_id = c("A", "A", "B"),
    project_thermal_energy_gj = c(210, 195, 160),
    project_thermal_emission_factor_tco2_per_gj = c(0.04, 0.038, 0.041),
    project_electricity_mwh = c(86, 74, 68),
    project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.6)
  )
  result <- calculate_project_building_emissions(data, group_cols = "building_id")
  expected_a <- sum(c(210, 195) * c(0.04, 0.038) + c(86, 74) * 0.62)
  expected_b <- 160 * 0.041 + 68 * 0.6
  expect_equal(result$project_emissions_tco2e[result$building_id == "A"], expected_a, tolerance = 1e-10)
  expect_equal(result$project_emissions_tco2e[result$building_id == "B"], expected_b, tolerance = 1e-10)
})

test_that("emission reductions subtract project and leakage", {
  baseline <- tibble::tibble(
    building_id = c("A", "B"),
    baseline_emissions_tco2e = c(230, 190)
  )
  project <- tibble::tibble(
    building_id = c("A", "B"),
    project_emissions_tco2e = c(140, 120)
  )
  leakage <- tibble::tibble(
    building_id = c("A", "B"),
    leakage_emissions_tco2e = c(4, 7)
  )
  reductions <- estimate_emission_reductions(baseline, project, leakage, group_cols = "building_id")
  expect_equal(reductions$emission_reductions_tco2e, c(86, 63))
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(
    building_id = c("A", "A", "B", "B"),
    baseline_thermal_energy_gj = c(420, 380, 320, 300),
    baseline_thermal_emission_factor_tco2_per_gj = c(0.058, 0.056, 0.06, 0.059),
    baseline_electricity_mwh = c(120, 95, 90, 88),
    baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.6, 0.6),
    project_thermal_energy_gj = c(210, 195, 180, 170),
    project_thermal_emission_factor_tco2_per_gj = c(0.04, 0.038, 0.042, 0.041),
    project_electricity_mwh = c(86, 74, 70, 68),
    project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.6, 0.6),
    leakage_emissions_tco2e = c(4.2, 3.8, 3.5, 3.1)
  )
  baseline <- calculate_baseline_building_emissions(data, group_cols = "building_id")
  project <- calculate_project_building_emissions(data, group_cols = "building_id")
  leakage <- calculate_leakage_emissions(
    data,
    group_cols = "building_id",
    leakage_col = "leakage_emissions_tco2e"
  )
  manual <- estimate_emission_reductions(baseline, project, leakage, group_cols = "building_id")
  wrapper <- estimate_emission_reductions_ams_iie(
    data,
    data,
    data,
    group_cols = "building_id",
    leakage_args = list(leakage_col = "leakage_emissions_tco2e")
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("monitoring aggregation preserves totals", {
  data <- simulate_ams_iie_dataset(n_buildings = 2, n_periods = 3)
  aggregated <- aggregate_monitoring_periods(
    data,
    period_col = monitoring_label,
    group_cols = "building_id"
  )

  expect_true(all(c("building_id", "monitoring_period") %in% names(aggregated)))
  expect_equal(sum(aggregated$baseline_electricity_mwh), sum(data$baseline_electricity_mwh))
  expect_equal(sum(aggregated$project_electricity_mwh), sum(data$project_electricity_mwh))
})
