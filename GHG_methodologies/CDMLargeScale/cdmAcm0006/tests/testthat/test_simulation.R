test_that("simulation produces expected columns", {
  data <- simulate_acm0006_dataset(3, seed = 123)
  expect_equal(nrow(data), 3)
  expect_true(all(c(
    "period",
    "heat_output_tj",
    "electricity_output_mwh",
    "baseline_heat_ef",
    "baseline_electricity_ef",
    "auxiliary_fossil_tj",
    "auxiliary_fossil_ef",
    "electricity_import_mwh",
    "import_emission_factor",
    "biomass_transport_tkm",
    "transport_emission_factor",
    "leakage_fraction"
  ) %in% names(data)))
})

test_that("aggregation and totals run on simulated data", {
  data <- simulate_acm0006_dataset(2, seed = 42)
  aggregated <- aggregate_monitoring_periods(data)
  totals <- estimate_emission_reductions_acm0006(data)

  expect_s3_class(aggregated, "tbl_df")
  expect_s3_class(totals, "tbl_df")
  expect_equal(nrow(aggregated), 2)
  expect_equal(nrow(totals), 1)
})
