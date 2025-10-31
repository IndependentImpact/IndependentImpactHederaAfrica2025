test_that("estimate_emission_reductions_acm0009 returns consistent summaries", {
  data <- tibble::tibble(
    period = c("2023Q1", "2023Q1", "2023Q2"),
    baseline_fuel_quantity = c(1000, 1100, 900),
    baseline_ncv_tj_per_unit = c(0.025, 0.025, 0.026),
    baseline_emission_factor_tco2_per_tj = c(95, 95, 96),
    project_fuel_quantity = c(1200, 1150, 980),
    project_ncv_tj_per_unit = c(0.038, 0.038, 0.039),
    project_emission_factor_tco2_per_tj = c(55, 55, 54),
    methane_slip_m3 = c(100, 120, 80),
    additional_leakage_tco2e = c(2, 2, 1)
  )

  results <- estimate_emission_reductions_acm0009(data)
  expect_named(results, c("period_results", "total_emission_reductions"))

  period_results <- results$period_results
  expect_equal(nrow(period_results), nrow(data))

  aggregated <- aggregate_monitoring_periods_acm0009(period_results)
  expect_equal(nrow(aggregated), 2)

  expected_baseline_q1 <- sum(c(1000, 1100) * 0.025 * 95)
  expected_project_q1 <- sum(c(1200, 1150) * 0.038 * 55)
  expected_leakage_q1 <- sum(c(100, 120) * 0.0007168 * 28 + 2)

  q1_row <- aggregated[aggregated$period == "2023Q1", ]
  expect_equal(q1_row$baseline_emissions_tco2e, expected_baseline_q1)
  expect_equal(q1_row$project_emissions_tco2e, expected_project_q1)
  expect_equal(q1_row$leakage_emissions_tco2e, expected_leakage_q1)
  expect_equal(
    q1_row$net_emission_reductions_tco2e,
    expected_baseline_q1 - expected_project_q1 - expected_leakage_q1
  )

  totals <- results$total_emission_reductions
  expect_equal(
    totals$net_emission_reductions_tco2e,
    sum(period_results$net_emission_reductions_tco2e)
  )
})
