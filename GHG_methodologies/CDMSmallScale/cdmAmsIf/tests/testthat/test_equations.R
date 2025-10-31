test_that("calculate_baseline_electricity_supply sums supply", {
  data <- tibble::tibble(grid_id = c("A", "A", "B"), electricity_mwh = c(120, 180, 200))
  result <- calculate_baseline_electricity_supply(data, group_cols = "grid_id")
  expect_equal(nrow(result), 2)
  expect_equal(result$baseline_electricity_mwh[result$grid_id == "A"], 300)
  expect_equal(result$baseline_electricity_mwh[result$grid_id == "B"], 200)
})

test_that("baseline and project emissions combine into reductions", {
  data <- tibble::tibble(grid_id = c("A", "B"), electricity_mwh = c(1000, 800))
  baseline <- calculate_baseline_electricity_supply(data, group_cols = "grid_id")
  be <- calculate_baseline_emissions(baseline, baseline_emission_factor = 0.7)
  pe <- calculate_project_emissions(baseline, project_emission_factor = 0.05)
  er <- calculate_emission_reductions(be, pe)

  expect_true(all(er$emission_reductions_tco2e <= be$baseline_emissions_tco2e))
  expect_equal(er$emission_reductions_tco2e, be$baseline_emissions_tco2e - pe$project_emissions_tco2e)
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(grid_id = c("A", "B"), electricity_mwh = c(1000, 800))
  manual <- calculate_emission_reductions(
    calculate_baseline_emissions(
      calculate_baseline_electricity_supply(data, group_cols = "grid_id"),
      baseline_emission_factor = 0.65
    ),
    calculate_project_emissions(
      calculate_baseline_electricity_supply(data, group_cols = "grid_id"),
      project_emission_factor = 0.03
    )
  )
  wrapper <- estimate_emission_reductions_ams_if(
    data,
    baseline_emission_factor = 0.65,
    project_emission_factor = 0.03,
    group_cols = "grid_id"
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("monitoring aggregation preserves totals", {
  data <- simulate_ams_if_dataset(n_grids = 2, n_periods = 4)
  aggregated <- aggregate_monitoring_periods(
    data,
    monitoring_cols = c("year", "month"),
    group_cols = "grid_id",
    electricity_col = "electricity_mwh",
    baseline_factor_col = "baseline_emission_factor",
    project_col = "project_emissions_tco2e"
  )

  expect_equal(nrow(aggregated), 8)
  expect_true(all(aggregated$baseline_emissions_tco2e >= aggregated$emission_reductions_tco2e))
})
