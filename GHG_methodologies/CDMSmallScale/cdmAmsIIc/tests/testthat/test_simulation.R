test_that("simulation generates expected columns", {
  set.seed(123)
  data <- simulate_ams_iic_dataset(n_sites = 3, n_periods = 2, start_year = 2024, start_month = 6)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "site_id", "monitoring_period", "year", "month", "day", "monitoring_date",
    "monitoring_label", "baseline_energy_mwh", "project_energy_mwh",
    "emission_factor_tco2e_mwh", "technology"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(unique(data$year), 2024)
  expect_equal(sort(unique(data$month)), c(6, 7))
})

test_that("aggregation aligns with simulated dataset", {
  set.seed(321)
  data <- simulate_ams_iic_dataset(n_sites = 2, n_periods = 3)
  summary <- aggregate_monitoring_periods(
    efficiency_data = data,
    monitoring_cols = c("monitoring_label"),
    group_cols = "site_id",
    baseline_energy_col = "baseline_energy_mwh",
    project_energy_col = "project_energy_mwh",
    emission_factor_col = "emission_factor_tco2e_mwh"
  )

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c(
    "site_id", "monitoring_label", "baseline_energy_mwh", "project_energy_mwh",
    "energy_savings_mwh", "emission_reductions_tco2e"
  ) %in% names(summary)))
  expect_equal(nrow(summary), 2 * 3)
})
