test_that("aggregate_monitoring_periods_acm0008 computes period totals", {
  data <- simulate_acm0008_dataset(periods = 2, observations_per_period = 5, seed = 321)
  summary <- aggregate_monitoring_periods_acm0008(data)

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 2)
  expect_true(all(summary$baseline_emissions >= summary$project_emissions))
  expect_true(all(summary$net_emission_reductions >= 0))
})

test_that("estimate_emission_reductions_acm0008 returns totals", {
  data <- simulate_acm0008_dataset(periods = 2, observations_per_period = 5, seed = 11)
  totals <- estimate_emission_reductions_acm0008(data)

  expect_named(
    totals,
    c(
      "total_recovered_volume_m3",
      "total_baseline_emissions",
      "total_project_emissions",
      "total_leakage_emissions",
      "total_net_emission_reductions"
    )
  )
  expect_gt(totals$total_baseline_emissions, totals$total_project_emissions)
  expect_gte(totals$total_net_emission_reductions, 0)
})
