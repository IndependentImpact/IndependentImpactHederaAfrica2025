test_that("simulation generates positive electricity output", {
  data <- simulate_acm0018_dataset(3, seed = 123)
  expect_s3_class(data, "tbl_df")
  expect_true(all(data$electricity_output_mwh >= 0))
  expect_named(
    data,
    c(
      "period",
      "electricity_output_mwh",
      "baseline_emission_factor",
      "auxiliary_fossil_tj",
      "auxiliary_fossil_ef",
      "onsite_generation_mwh",
      "onsite_emission_factor",
      "biomass_transport_tkm",
      "transport_emission_factor",
      "leakage_fraction"
    )
  )
})

test_that("aggregation produces period totals and emission reductions", {
  data <- simulate_acm0018_dataset(5, seed = 99)
  aggregated <- aggregate_monitoring_periods_acm0018(data)
  expect_true(all(c(
    "period",
    "baseline_emissions",
    "project_emissions",
    "emission_reductions"
  ) %in% names(aggregated)))
  totals <- estimate_emission_reductions_acm0018(data)
  expect_equal(nrow(totals), 1)
  expect_true(all(totals$total_emission_reductions >= 0))
})
