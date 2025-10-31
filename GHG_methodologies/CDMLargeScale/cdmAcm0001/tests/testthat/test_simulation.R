test_that("simulation generates ACM0001 monitoring records", {
  data <- simulate_acm0001_dataset(3, seed = 101)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "period",
    "methane_generation_m3",
    "baseline_capture_efficiency",
    "methane_captured_m3",
    "destruction_efficiency",
    "auxiliary_fuel_tj",
    "auxiliary_ef_t_per_tj",
    "electricity_import_mwh",
    "import_ef_t_per_mwh",
    "leakage_fraction",
    "oxidation_fraction",
    "methane_density_t_per_m3",
    "gwp_ch4"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(nrow(data), 3)
  expect_true(all(data$methane_captured_m3 <= data$methane_generation_m3))
})

test_that("aggregated simulation data returns positive reductions", {
  data <- simulate_acm0001_dataset(5, seed = 2024)
  summary <- aggregate_monitoring_periods(data)
  expect_true(all(summary$baseline_emissions >= summary$project_emissions))
  expect_true(all(summary$methane_destroyed_t >= 0))
  totals <- estimate_emission_reductions_acm0001(data)
  expect_true(totals$total_emission_reductions >= 0)
  expect_equal(totals$total_methane_destroyed_co2e,
               sum(summary$methane_destroyed_co2e))
})
