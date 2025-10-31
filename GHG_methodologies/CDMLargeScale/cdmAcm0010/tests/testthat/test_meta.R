test_that("meta estimator returns expected structure", {
  monitoring <- simulate_acm0010_dataset(periods = 4, seed = 123)
  results <- estimate_emission_reductions_acm0010(monitoring)

  expect_named(results, c("period_results", "total_emission_reductions"))
  expect_s3_class(results$period_results, "tbl_df")
  expect_equal(nrow(results$period_results), 4)
  expect_true(all(c("baseline_emissions_tco2e", "project_emissions_tco2e", "net_emission_reductions_tco2e") %in%
                    names(results$period_results)))
  expect_s3_class(results$total_emission_reductions, "tbl_df")
  expect_equal(nrow(results$total_emission_reductions), 1)
})

test_that("aggregation collapses periods", {
  monitoring <- simulate_acm0010_dataset(periods = 6, seed = 99)
  results <- estimate_emission_reductions_acm0010(monitoring)
  aggregated <- aggregate_monitoring_periods_acm0010(results$period_results)

  expect_equal(nrow(aggregated), 6)
  expect_true("net_emission_reductions_tco2e" %in% names(aggregated))
})

