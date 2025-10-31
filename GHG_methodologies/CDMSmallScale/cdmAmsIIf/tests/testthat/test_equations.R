test_that("baseline agricultural emissions aggregate correctly", {
  data <- tibble::tibble(
    facility_id = c("A", "A", "B"),
    baseline_fuel_energy_gj = c(980, 1020, 860),
    baseline_fuel_emission_factor_tco2_per_gj = c(0.074, 0.072, 0.071),
    baseline_electricity_mwh = c(180, 195, 160),
    baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.61, 0.6)
  )
  result <- calculate_baseline_agricultural_emissions(data, group_cols = "facility_id")
  expect_equal(nrow(result), 2)
  expected_a <- sum(c(980, 1020) * c(0.074, 0.072) + c(180, 195) * c(0.62, 0.61))
  expected_b <- 860 * 0.071 + 160 * 0.6
  expect_equal(result$baseline_emissions_tco2e[result$facility_id == "A"], expected_a, tolerance = 1e-10)
  expect_equal(result$baseline_emissions_tco2e[result$facility_id == "B"], expected_b, tolerance = 1e-10)
})

test_that("project agricultural emissions aggregate correctly", {
  data <- tibble::tibble(
    facility_id = c("A", "A", "B"),
    project_fuel_energy_gj = c(520, 540, 480),
    project_fuel_emission_factor_tco2_per_gj = c(0.031, 0.03, 0.029),
    project_electricity_mwh = c(145, 150, 138),
    project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.61, 0.6)
  )
  result <- calculate_project_agricultural_emissions(data, group_cols = "facility_id")
  expected_a <- sum(c(520, 540) * c(0.031, 0.03) + c(145, 150) * c(0.62, 0.61))
  expected_b <- 480 * 0.029 + 138 * 0.6
  expect_equal(result$project_emissions_tco2e[result$facility_id == "A"], expected_a, tolerance = 1e-10)
  expect_equal(result$project_emissions_tco2e[result$facility_id == "B"], expected_b, tolerance = 1e-10)
})

test_that("emission reductions subtract project and leakage", {
  baseline <- tibble::tibble(
    facility_id = c("A", "B"),
    baseline_emissions_tco2e = c(420, 360)
  )
  project <- tibble::tibble(
    facility_id = c("A", "B"),
    project_emissions_tco2e = c(210, 190)
  )
  leakage <- tibble::tibble(
    facility_id = c("A", "B"),
    leakage_emissions_tco2e = c(6, 4)
  )
  reductions <- calculate_emission_reductions_iif(baseline, project, leakage, group_cols = "facility_id")
  expect_equal(reductions$emission_reductions_tco2e, c(204, 166))
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(
    facility_id = c("A", "A", "B", "B"),
    baseline_fuel_energy_gj = c(980, 1020, 860, 880),
    baseline_fuel_emission_factor_tco2_per_gj = c(0.074, 0.072, 0.071, 0.07),
    baseline_electricity_mwh = c(180, 195, 160, 158),
    baseline_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.61, 0.6, 0.6),
    project_fuel_energy_gj = c(520, 540, 480, 470),
    project_fuel_emission_factor_tco2_per_gj = c(0.031, 0.03, 0.029, 0.028),
    project_electricity_mwh = c(145, 150, 138, 135),
    project_electricity_emission_factor_tco2_per_mwh = c(0.62, 0.61, 0.6, 0.6),
    leakage_emissions_tco2e = c(6.2, 5.8, 4.9, 4.5)
  )
  baseline <- calculate_baseline_agricultural_emissions(data, group_cols = "facility_id")
  project <- calculate_project_agricultural_emissions(data, group_cols = "facility_id")
  leakage <- calculate_leakage_emissions_iif(
    data,
    group_cols = "facility_id",
    leakage_col = "leakage_emissions_tco2e"
  )
  manual <- calculate_emission_reductions_iif(baseline, project, leakage, group_cols = "facility_id")
  wrapper <- estimate_emission_reductions_ams_iif(
    data,
    data,
    data,
    group_cols = "facility_id",
    leakage_args = list(leakage_col = "leakage_emissions_tco2e")
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("monitoring aggregation preserves totals", {
  data <- simulate_ams_iif_dataset(n_facilities = 2, n_periods = 3)
  aggregated <- aggregate_monitoring_periods_iif(
    data,
    period_col = monitoring_label,
    group_cols = "facility_id"
  )

  expect_true(all(c("facility_id", "monitoring_period") %in% names(aggregated)))
  expect_equal(sum(aggregated$baseline_electricity_mwh), sum(data$baseline_electricity_mwh))
  expect_equal(sum(aggregated$project_electricity_mwh), sum(data$project_electricity_mwh))
})
