test_that("net electricity generation subtracts auxiliary use", {
  net <- calculate_net_electricity_generation(c(1000, 1100), c(50, 60))
  expect_equal(net, c(950, 1040))
})

test_that("baseline, project, and reductions chain correctly", {
  net <- calculate_net_electricity_generation(1000, 50)
  baseline <- calculate_baseline_emissions(net, 0.8)
  project <- calculate_project_emissions(0.1, 74, electricity_import_mwh = 5, import_emission_factor = 0.8)
  reductions <- calculate_emission_reductions(baseline, project)

  expect_true(baseline > 0)
  expect_true(project > 0)
  expect_equal(reductions, baseline - project)
})

test_that("aggregate_monitoring_periods returns period summaries", {
  data <- tibble::tibble(
    period = rep(c("P1", "P2"), each = 2),
    gross_generation_mwh = c(1500, 1600, 1400, 1550),
    auxiliary_consumption_mwh = c(60, 55, 50, 45),
    combined_margin_ef = 0.75,
    fossil_fuel_tj = c(0.05, 0.04, 0.03, 0.02),
    fossil_emission_factor = 74,
    electricity_import_mwh = c(10, 12, 9, 8),
    import_emission_factor = 0.75,
    leakage_emissions = 0
  )

  summary <- aggregate_monitoring_periods(data)
  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 2)
  expect_true(all(summary$net_electricity_mwh > 0))
  expect_true(all(summary$emission_reductions >= 0))
})

test_that("estimate_emission_reductions_acm0002 matches aggregated totals", {
  data <- simulate_acm0002_dataset(4, seed = 99)
  summary <- aggregate_monitoring_periods(data)
  totals <- estimate_emission_reductions_acm0002(data)

  expect_equal(totals$total_net_electricity_mwh, sum(summary$net_electricity_mwh))
  expect_equal(totals$total_emission_reductions, sum(summary$emission_reductions))
})
