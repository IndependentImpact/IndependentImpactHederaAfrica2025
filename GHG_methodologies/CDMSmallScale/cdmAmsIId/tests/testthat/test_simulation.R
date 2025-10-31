test_that("simulation generates expected columns", {
  set.seed(123)
  data <- simulate_ams_iid_dataset(n_facilities = 3, n_periods = 2, start_year = 2024, start_month = 6)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "facility_id", "monitoring_period", "year", "month", "day", "monitoring_date",
    "monitoring_label", "baseline_fuel_quantity", "baseline_ncv_gj_per_unit",
    "baseline_emission_factor_tco2_per_gj", "baseline_efficiency",
    "project_fuel_quantity", "project_ncv_gj_per_unit", "project_emission_factor_tco2_per_gj",
    "project_efficiency", "electricity_emissions_tco2e", "useful_heat_output",
    "leakage_emissions_tco2e"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(unique(data$year), 2024)
  expect_equal(sort(unique(data$month)), c(6, 7))
})

test_that("aggregation collapses monitoring periods", {
  set.seed(321)
  data <- simulate_ams_iid_dataset(n_facilities = 2, n_periods = 3)
  summary <- aggregate_monitoring_periods(
    data,
    period_col = monitoring_label,
    group_cols = "facility_id"
  )

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c("facility_id", "monitoring_period") %in% names(summary)))
  expect_equal(nrow(summary), 2)
  expect_equal(sum(summary$baseline_fuel_quantity), sum(data$baseline_fuel_quantity))
})
