
test_that("calculate_baseline_generation sums generation", {
  data <- tibble::tibble(user_id = c("A", "A", "B"), generation_kwh = c(100, 150, 200))
  result <- calculate_baseline_generation(data, group_cols = "user_id")
  expect_equal(nrow(result), 2)
  expect_equal(result$baseline_generation_kwh[result$user_id == "A"], 250)
  expect_equal(result$baseline_generation_kwh[result$user_id == "B"], 200)
})

test_that("baseline and project emissions combine into reductions", {
  data <- tibble::tibble(user_id = c("A", "B"), generation_kwh = c(1000, 800))
  baseline <- calculate_baseline_generation(data, group_cols = "user_id")
  be <- calculate_baseline_emissions(baseline, grid_emission_factor = 0.9)
  pe <- calculate_project_emissions(baseline)
  er <- calculate_emission_reductions(be, pe)

  expect_true(all(er$emission_reductions_tco2e >= 0))
  expect_equal(er$emission_reductions_tco2e, be$baseline_emissions_tco2e)
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(user_id = c("A", "B"), generation_kwh = c(1000, 800))
  manual <- calculate_emission_reductions(
    calculate_baseline_emissions(
      calculate_baseline_generation(data, group_cols = "user_id"),
      grid_emission_factor = 0.8
    ),
    calculate_project_emissions(
      calculate_baseline_generation(data, group_cols = "user_id"),
      project_emission_factor = 0
    )
  )
  wrapper <- estimate_emission_reductions_ams_ia(
    data,
    grid_emission_factor = 0.8,
    project_emission_factor = 0,
    group_cols = "user_id"
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})
