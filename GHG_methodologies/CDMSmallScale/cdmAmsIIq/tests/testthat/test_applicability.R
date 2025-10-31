test_that("service scope ensures all baseline services are covered", {
  baseline <- tibble::tibble(
    service = c("cooling", "lighting"),
    baseline_units = c(4, 200)
  )
  project_good <- tibble::tibble(
    service = c("cooling", "lighting"),
    project_units = c(2, 200)
  )
  project_bad <- tibble::tibble(
    service = c("cooling", "lighting"),
    project_units = c(5, 210)
  )
  missing_service <- tibble::tibble(
    service = c("cooling"),
    project_units = c(2)
  )

  expect_true(check_applicability_service_scope_iiq(baseline, project_good))
  expect_false(check_applicability_service_scope_iiq(baseline, project_bad))
  expect_false(check_applicability_service_scope_iiq(baseline, missing_service))
})

test_that("monitoring applicability checks required columns", {
  monitoring <- tibble::tibble(
    baseline_energy_use_mwh = c(400, 380),
    baseline_emission_factor_tco2_per_mwh = c(0.62, 0.62),
    project_energy_use_mwh = c(300, 280),
    project_emission_factor_tco2_per_mwh = c(0.58, 0.58)
  )

  expect_true(check_applicability_monitoring_iiq(monitoring))

  monitoring$project_emission_factor_tco2_per_mwh[1] <- NA_real_
  expect_false(check_applicability_monitoring_iiq(monitoring))

  expect_error(check_applicability_monitoring_iiq(monitoring, required_cols = c("missing_col")))
})

test_that("efficiency gain applicability enforces minimum improvement", {
  baseline <- tibble::tibble(
    building_id = c("A", "B"),
    baseline_energy_use_mwh = c(800, 600),
    baseline_service_output_mwh = c(720, 540)
  )
  project_good <- tibble::tibble(
    building_id = c("A", "B"),
    project_energy_use_mwh = c(520, 420),
    project_service_output_mwh = c(720, 540)
  )
  project_bad <- tibble::tibble(
    building_id = c("A", "B"),
    project_energy_use_mwh = c(780, 590),
    project_service_output_mwh = c(720, 540)
  )

  expect_true(check_applicability_efficiency_gain_iiq(baseline, project_good, group_cols = "building_id", minimum_improvement = 0.05))
  expect_false(check_applicability_efficiency_gain_iiq(baseline, project_bad, group_cols = "building_id", minimum_improvement = 0.05))
  expect_error(check_applicability_efficiency_gain_iiq(baseline, project_good, minimum_improvement = 1.5))
})
