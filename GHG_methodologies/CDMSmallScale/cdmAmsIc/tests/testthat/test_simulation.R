test_that("simulation generates expected columns", {
  set.seed(123)
  data <- simulate_ams_ic_dataset(n_facilities = 5, n_periods = 2, start_year = 2024, start_month = 6)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "facility_id", "monitoring_period", "year", "month", "day", "monitoring_date",
    "monitoring_label", "thermal_energy_mwh", "baseline_emission_factor",
    "project_emission_factor", "baseline_emissions_tco2e", "emission_reductions_tco2e"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(unique(data$year), 2024)
  expect_equal(sort(unique(data$month)), c(6, 7))
})

test_that("monitoring aggregation returns period summaries", {
  set.seed(321)
  data <- simulate_ams_ic_dataset(n_facilities = 2, n_periods = 3)
  summary <- aggregate_monitoring_periods(
    thermal_data = data,
    monitoring_cols = c("monitoring_label"),
    group_cols = "facility_id",
    energy_col = "thermal_energy_mwh",
    baseline_factor_col = "baseline_emission_factor"
  )

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c(
    "facility_id", "monitoring_label", "baseline_thermal_output_mwh", "baseline_emissions_tco2e",
    "project_emissions_tco2e", "emission_reductions_tco2e", "baseline_emission_factor"
  ) %in% names(summary)))
  expect_equal(nrow(summary), 2 * 3)
})
