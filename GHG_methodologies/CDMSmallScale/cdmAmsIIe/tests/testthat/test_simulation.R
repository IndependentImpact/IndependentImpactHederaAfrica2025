test_that("simulation generates expected columns", {
  set.seed(123)
  data <- simulate_ams_iie_dataset(n_buildings = 3, n_periods = 2, start_year = 2024, start_month = 6)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "building_id", "monitoring_period", "year", "month", "day", "monitoring_date",
    "monitoring_label", "baseline_electricity_mwh", "baseline_thermal_energy_gj",
    "baseline_electricity_emission_factor_tco2_per_mwh", "baseline_thermal_emission_factor_tco2_per_gj",
    "baseline_total_energy_mwh", "project_electricity_mwh", "project_thermal_energy_gj",
    "project_electricity_emission_factor_tco2_per_mwh", "project_thermal_emission_factor_tco2_per_gj",
    "project_total_energy_mwh", "service_level_indicator", "operating_hours", "leakage_emissions_tco2e"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(unique(data$year), 2024)
  expect_equal(sort(unique(data$month)), c(6, 7))
})

test_that("aggregation collapses monitoring periods", {
  set.seed(321)
  data <- simulate_ams_iie_dataset(n_buildings = 2, n_periods = 3)
  summary <- aggregate_monitoring_periods(
    data,
    period_col = monitoring_label,
    group_cols = "building_id"
  )

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c("building_id", "monitoring_period") %in% names(summary)))
  expect_equal(nrow(summary), 2)
  expect_equal(sum(summary$baseline_electricity_mwh), sum(data$baseline_electricity_mwh))
})
