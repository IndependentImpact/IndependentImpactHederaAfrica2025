test_that("baseline energy consumption aggregates correctly", {
  data <- tibble::tibble(site_id = c("A", "A", "B"), baseline_energy_mwh = c(12, 18, 25))
  result <- calculate_baseline_energy_consumption(data, group_cols = "site_id")
  expect_equal(nrow(result), 2)
  expect_equal(result$baseline_energy_mwh[result$site_id == "A"], 30)
  expect_equal(result$baseline_energy_mwh[result$site_id == "B"], 25)
})

test_that("project energy consumption aggregates correctly", {
  data <- tibble::tibble(site_id = c("A", "A", "B"), project_energy_mwh = c(7, 9, 12))
  result <- calculate_project_energy_consumption(data, group_cols = "site_id")
  expect_equal(result$project_energy_mwh[result$site_id == "A"], 16)
  expect_equal(result$project_energy_mwh[result$site_id == "B"], 12)
})

test_that("energy savings difference matches manual calculation", {
  baseline <- tibble::tibble(site_id = c("A", "B"), baseline_energy_mwh = c(46, 28))
  project <- tibble::tibble(site_id = c("A", "B"), project_energy_mwh = c(24, 18))
  savings <- calculate_energy_savings(baseline, project)
  expect_equal(savings$energy_savings_mwh, c(22, 10))
})

test_that("emission reductions scale with emission factor", {
  savings <- tibble::tibble(site_id = c("A", "B"), energy_savings_mwh = c(22, 10))
  reductions <- calculate_emission_reductions(savings, emission_factor = 0.65)
  expect_equal(reductions$emission_reductions_tco2e, c(14.3, 6.5))
})

test_that("meta-function matches manual workflow", {
  baseline <- tibble::tibble(site_id = c("A", "B"), baseline_energy_mwh = c(50, 32))
  project <- tibble::tibble(site_id = c("A", "B"), project_energy_mwh = c(20, 18))
  manual <- calculate_emission_reductions(
    calculate_energy_savings(
      baseline,
      project,
      baseline_col = "baseline_energy_mwh",
      project_col = "project_energy_mwh",
      output_col = "energy_savings_mwh"
    ),
    emission_factor = 0.72
  )
  wrapper <- estimate_emission_reductions_ams_iic(
    baseline,
    project,
    emission_factor = 0.72,
    group_cols = "site_id"
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("monitoring aggregation preserves totals", {
  data <- simulate_ams_iic_dataset(n_sites = 2, n_periods = 3)
  aggregated <- aggregate_monitoring_periods(
    data,
    monitoring_cols = c("year", "month"),
    group_cols = "site_id",
    baseline_energy_col = "baseline_energy_mwh",
    project_energy_col = "project_energy_mwh",
    emission_factor_col = "emission_factor_tco2e_mwh"
  )

  expect_true(all(c(
    "site_id", "year", "month", "baseline_energy_mwh", "project_energy_mwh",
    "energy_savings_mwh", "emission_reductions_tco2e"
  ) %in% names(aggregated)))
  expect_equal(
    sum(aggregated$baseline_energy_mwh),
    sum(data$baseline_energy_mwh)
  )
  expect_equal(
    sum(aggregated$project_energy_mwh),
    sum(data$project_energy_mwh)
  )
})
