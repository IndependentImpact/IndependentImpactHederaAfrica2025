test_that("monitoring aggregation produces expected columns", {
  monitoring <- simulate_acm0019_dataset(periods = 2, observations_per_period = 5, seed = 123)
  summary <- aggregate_monitoring_periods_acm0019(monitoring)
  expect_true(all(c(
    "period", "production_tonnes", "baseline_emissions", "project_emissions",
    "leakage_emissions", "net_emission_reductions"
  ) %in% names(summary)))
  expect_equal(nrow(summary), 2)
})

test_that("total estimator sums period-level values", {
  monitoring <- simulate_acm0019_dataset(periods = 3, observations_per_period = 4, seed = 44)
  period_summary <- aggregate_monitoring_periods_acm0019(monitoring)
  totals <- estimate_emission_reductions_acm0019(monitoring)
  expect_equal(totals$total_baseline_emissions, sum(period_summary$baseline_emissions))
  expect_equal(totals$total_net_emission_reductions, sum(period_summary$net_emission_reductions))
})
