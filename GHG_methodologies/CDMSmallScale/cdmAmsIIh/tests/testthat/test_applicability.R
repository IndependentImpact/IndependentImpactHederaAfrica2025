test_that("centralization scope requires unit reduction", {
  baseline <- tibble::tibble(
    utility_service = c("steam", "hot_water"),
    baseline_unit_count = c(4, 3)
  )
  project_good <- tibble::tibble(
    utility_service = c("steam", "hot_water"),
    project_unit_count = c(1, 1)
  )
  project_bad <- tibble::tibble(
    utility_service = c("steam", "hot_water"),
    project_unit_count = c(2, 1)
  )

  expect_true(check_applicability_centralization_scope_iih(baseline, project_good))
  expect_false(check_applicability_centralization_scope_iih(baseline, project_bad))

  missing_service <- tibble::tibble(
    utility_service = "steam",
    project_unit_count = 1
  )
  expect_false(check_applicability_centralization_scope_iih(baseline, missing_service))
})

test_that("monitoring completeness requires all fields", {
  monitoring <- tibble::tibble(
    baseline_fuel_use_gj = 4200,
    baseline_emission_factor_tco2_per_gj = 0.071,
    project_fuel_use_gj = 3000,
    project_emission_factor_tco2_per_gj = 0.068
  )

  expect_true(check_applicability_monitoring_iih(monitoring))
  monitoring$project_emission_factor_tco2_per_gj[1] <- NA_real_
  expect_false(check_applicability_monitoring_iih(monitoring))
  monitoring$project_emission_factor_tco2_per_gj[1] <- 0.068
  expect_error(check_applicability_monitoring_iih(monitoring, required_cols = c("missing_col")))
})

test_that("efficiency improvement enforces minimum reduction", {
  baseline <- tibble::tibble(
    facility = c("A", "B"),
    baseline_fuel_use_gj = c(4200, 3150),
    baseline_useful_output_gj = c(3600, 2700)
  )
  project_good <- tibble::tibble(
    facility = c("A", "B"),
    project_fuel_use_gj = c(3000, 2250),
    project_useful_output_gj = c(2880, 2160)
  )
  project_bad <- tibble::tibble(
    facility = c("A", "B"),
    project_fuel_use_gj = c(3500, 2800),
    project_useful_output_gj = c(3000, 2400)
  )

  expect_true(check_applicability_efficiency_improvement_iih(baseline, project_good, group_cols = "facility", minimum_improvement = 0.05))
  expect_false(check_applicability_efficiency_improvement_iih(baseline, project_bad, group_cols = "facility", minimum_improvement = 0.05))
  expect_error(check_applicability_efficiency_improvement_iih(baseline, project_good, minimum_improvement = 1.5))
})
