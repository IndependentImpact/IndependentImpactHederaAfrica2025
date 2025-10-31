test_that("energy intensity applicability requires meaningful reduction", {
  baseline <- tibble::tibble(
    baseline_total_energy_mwh = c(220, 195),
    service_level_indicator = c(420, 380)
  )
  project_good <- tibble::tibble(
    project_total_energy_mwh = c(150, 138),
    service_level_indicator = c(420, 380)
  )
  project_bad <- tibble::tibble(
    project_total_energy_mwh = c(210, 190),
    service_level_indicator = c(420, 380)
  )

  expect_true(check_applicability_energy_intensity_iif(
    baseline,
    project_good,
    energy_col = "baseline_total_energy_mwh",
    project_energy_col = "project_total_energy_mwh",
    output_col = "service_level_indicator",
    tolerance = 0.1
  ))
  expect_false(check_applicability_energy_intensity_iif(
    baseline,
    project_bad,
    energy_col = "baseline_total_energy_mwh",
    project_energy_col = "project_total_energy_mwh",
    output_col = "service_level_indicator",
    tolerance = 0.1
  ))
})

test_that("fuel switching applicability compares weighted factors", {
  baseline <- tibble::tibble(
    baseline_thermal_energy_gj = c(320, 180),
    baseline_thermal_emission_factor_tco2_per_gj = c(0.07, 0.072)
  )
  project_good <- tibble::tibble(
    project_thermal_energy_gj = c(280, 120),
    project_thermal_emission_factor_tco2_per_gj = c(0.045, 0.05)
  )
  project_bad <- tibble::tibble(
    project_thermal_energy_gj = c(340, 200),
    project_thermal_emission_factor_tco2_per_gj = c(0.075, 0.078)
  )

  expect_true(check_applicability_fuel_switching_iif(baseline, project_good))
  expect_false(check_applicability_fuel_switching_iif(baseline, project_bad))
})

test_that("monitoring applicability verifies required columns", {
  monitoring <- tibble::tibble(
    project_total_energy_mwh = c(12.5, 11.2),
    service_level_indicator = c(0.35, 0.32),
    operating_hours = c(220, 215)
  )
  incomplete <- tibble::tibble(project_total_energy_mwh = c(12.5, 11.2))

  expect_true(check_applicability_monitoring_iif(monitoring))
  expect_error(check_applicability_monitoring_iif(incomplete))
})

test_that("combined applicability summary includes all checks", {
  baseline <- tibble::tibble(
    baseline_total_energy_mwh = c(220, 195),
    baseline_thermal_energy_gj = c(320, 180),
    baseline_thermal_emission_factor_tco2_per_gj = c(0.07, 0.072),
    service_level_indicator = c(420, 380)
  )
  project <- tibble::tibble(
    project_total_energy_mwh = c(150, 138),
    project_thermal_energy_gj = c(280, 120),
    project_thermal_emission_factor_tco2_per_gj = c(0.045, 0.05),
    service_level_indicator = c(420, 380)
  )
  monitoring <- tibble::tibble(
    project_total_energy_mwh = c(12.5, 11.2),
    service_level_indicator = c(0.35, 0.32),
    operating_hours = c(220, 215)
  )

  summary <- assess_ams_iif_applicability(baseline, project, monitoring)
  expect_s3_class(summary, "tbl_df")
  expect_equal(summary$criterion, c("energy_intensity", "fuel_switching", "monitoring"))
  expect_true(all(summary$is_met))
})
