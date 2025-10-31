test_that("energy efficiency applicability requires reduction", {
  baseline <- tibble::tibble(baseline_fuel_quantity = c(1200, 900), baseline_efficiency = c(0.7, 0.68))
  project_good <- tibble::tibble(project_fuel_quantity = c(920, 720), project_efficiency = c(0.84, 0.82))
  project_bad <- tibble::tibble(project_fuel_quantity = c(1100, 820), project_efficiency = c(0.72, 0.7))

  expect_true(check_applicability_energy_efficiency(baseline, project_good))
  expect_false(check_applicability_energy_efficiency(baseline, project_bad))
})

test_that("fuel switching applicability compares weighted factors", {
  baseline <- tibble::tibble(
    baseline_fuel_quantity = c(800, 400),
    baseline_emission_factor_tco2_per_gj = c(0.094, 0.098)
  )
  project_good <- tibble::tibble(
    project_fuel_quantity = c(700, 200),
    project_emission_factor_tco2_per_gj = c(0.082, 0.082)
  )
  project_bad <- tibble::tibble(
    project_fuel_quantity = c(900, 300),
    project_emission_factor_tco2_per_gj = c(0.1, 0.099)
  )

  expect_true(check_applicability_fuel_switching(baseline, project_good))
  expect_false(check_applicability_fuel_switching(baseline, project_bad))
})

test_that("monitoring applicability verifies required columns", {
  monitoring <- tibble::tibble(
    project_fuel_quantity = c(80, 75),
    project_efficiency = c(0.84, 0.82),
    useful_heat_output = c(60, 58)
  )
  incomplete <- tibble::tibble(project_fuel_quantity = c(80, 75))

  expect_true(check_applicability_monitoring(monitoring))
  expect_false(check_applicability_monitoring(incomplete))
})

test_that("combined applicability summary includes all checks", {
  baseline <- tibble::tibble(
    baseline_fuel_quantity = c(1200, 900),
    baseline_efficiency = c(0.72, 0.68),
    baseline_emission_factor_tco2_per_gj = c(0.094, 0.094)
  )
  project <- tibble::tibble(
    project_fuel_quantity = c(920, 720),
    project_efficiency = c(0.84, 0.82),
    project_emission_factor_tco2_per_gj = c(0.082, 0.082)
  )
  monitoring <- tibble::tibble(
    project_fuel_quantity = c(80, 70),
    project_efficiency = c(0.84, 0.8),
    useful_heat_output = c(60, 55)
  )

  summary <- assess_ams_iid_applicability(baseline, project, monitoring)
  expect_s3_class(summary, "tbl_df")
  expect_named(summary, c("energy_efficiency", "fuel_switching", "monitoring_ready", "overall_applicable"))
  expect_true(summary$overall_applicable)

  summary_no_switch <- assess_ams_iid_applicability(baseline, project, monitoring, fuel_switch = FALSE)
  expect_true(summary_no_switch$fuel_switching)
})
