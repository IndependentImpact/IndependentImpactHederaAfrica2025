test_that("calculate_baseline_thermal_output sums energy", {
  data <- tibble::tibble(facility_id = c("A", "A", "B"), thermal_energy_mwh = c(100, 150, 200))
  result <- calculate_baseline_thermal_output(data, group_cols = "facility_id")
  expect_equal(nrow(result), 2)
  expect_equal(result$baseline_thermal_output_mwh[result$facility_id == "A"], 250)
  expect_equal(result$baseline_thermal_output_mwh[result$facility_id == "B"], 200)
})

test_that("baseline and project emissions combine into reductions", {
  data <- tibble::tibble(facility_id = c("A", "B"), thermal_energy_mwh = c(1000, 800))
  baseline <- calculate_baseline_thermal_output(data, group_cols = "facility_id")
  be <- calculate_baseline_emissions(baseline, baseline_emission_factor = 0.25)
  pe <- calculate_project_emissions(baseline, project_emission_factor = 0.02)
  er <- calculate_emission_reductions(be, pe)

  expect_true(all(er$emission_reductions_tco2e <= be$baseline_emissions_tco2e))
  expect_equal(er$emission_reductions_tco2e, be$baseline_emissions_tco2e - pe$project_emissions_tco2e)
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(facility_id = c("A", "B"), thermal_energy_mwh = c(1000, 800))
  manual <- calculate_emission_reductions(
    calculate_baseline_emissions(
      calculate_baseline_thermal_output(data, group_cols = "facility_id"),
      baseline_emission_factor = 0.3
    ),
    calculate_project_emissions(
      calculate_baseline_thermal_output(data, group_cols = "facility_id"),
      project_emission_factor = 0.01
    )
  )
  wrapper <- estimate_emission_reductions_ams_ic(
    data,
    baseline_emission_factor = 0.3,
    project_emission_factor = 0.01,
    group_cols = "facility_id"
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})
