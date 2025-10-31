test_that("simulation generates expected columns", {
  set.seed(123)
  data <- simulate_ams_id_dataset(n_grids = 4, n_periods = 2, start_year = 2024, start_month = 6)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "grid_id", "monitoring_period", "year", "month", "day", "monitoring_date",
    "monitoring_label", "electricity_mwh", "baseline_emission_factor",
    "project_emission_factor", "baseline_emissions_tco2e", "emission_reductions_tco2e"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(unique(data$year), 2024)
  expect_equal(sort(unique(data$month)), c(6, 7))
})

test_that("monitoring aggregation returns period summaries", {
  set.seed(321)
  data <- simulate_ams_id_dataset(n_grids = 2, n_periods = 3)
  summary <- aggregate_monitoring_periods(
    supply_data = data,
    monitoring_cols = c("monitoring_label"),
    group_cols = "grid_id",
    electricity_col = "electricity_mwh",
    baseline_factor_col = "baseline_emission_factor"
  )

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c(
    "grid_id", "monitoring_label", "baseline_electricity_mwh", "baseline_emissions_tco2e",
    "project_emissions_tco2e", "emission_reductions_tco2e", "baseline_emission_factor"
  ) %in% names(summary)))
  expect_equal(nrow(summary), 2 * 3)
})
