test_that("simulation generates ACM0002 monitoring records", {
  data <- simulate_acm0002_dataset(3, seed = 101)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "period",
    "gross_generation_mwh",
    "auxiliary_consumption_mwh",
    "combined_margin_ef",
    "fossil_fuel_tj",
    "fossil_emission_factor",
    "electricity_import_mwh",
    "import_emission_factor",
    "leakage_emissions"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(nrow(data), 3)
  expect_true(all(data$gross_generation_mwh >= data$auxiliary_consumption_mwh))
})

test_that("aggregated simulation data returns non-negative reductions", {
  data <- simulate_acm0002_dataset(5, seed = 2024)
  summary <- aggregate_monitoring_periods(data)
  expect_true(all(summary$baseline_emissions >= summary$project_emissions))
  totals <- estimate_emission_reductions_acm0002(data)
  expect_true(totals$total_emission_reductions >= 0)
})
