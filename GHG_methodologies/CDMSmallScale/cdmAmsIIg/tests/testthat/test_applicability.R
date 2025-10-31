test_that("efficiency improvement criterion validates reductions", {
  baseline <- tibble::tibble(
    baseline_biomass_consumption_tonnes = c(12, 14),
    baseline_non_renewable_fraction = c(0.85, 0.88)
  )
  project_good <- tibble::tibble(
    project_biomass_consumption_tonnes = c(7.2, 7.9),
    project_non_renewable_fraction = c(0.4, 0.38)
  )
  project_bad <- tibble::tibble(
    project_biomass_consumption_tonnes = c(11.5, 12.2),
    project_non_renewable_fraction = c(0.78, 0.76)
  )

  expect_true(check_applicability_efficiency_improvement_iig(baseline, project_good, tolerance = 0.2))
  expect_false(check_applicability_efficiency_improvement_iig(baseline, project_bad, tolerance = 0.2))
})

test_that("fraction bounds enforce [0, 1]", {
  monitoring <- tibble::tibble(
    baseline_non_renewable_fraction = c(0.8, 0.85),
    project_non_renewable_fraction = c(0.4, 0.38)
  )
  expect_true(check_applicability_fraction_bounds_iig(monitoring))
  monitoring$baseline_non_renewable_fraction[1] <- 1.2
  expect_false(check_applicability_fraction_bounds_iig(monitoring))
  monitoring$baseline_non_renewable_fraction[1] <- -0.1
  expect_false(check_applicability_fraction_bounds_iig(monitoring))
})

test_that("monitoring completeness requires all fields", {
  monitoring <- tibble::tibble(
    baseline_biomass_consumption_tonnes = 12,
    baseline_non_renewable_fraction = 0.85,
    baseline_net_calorific_value_mj_per_tonne = 15.2,
    baseline_emission_factor_tco2_per_mj = 0.00009,
    project_biomass_consumption_tonnes = 7.4,
    project_non_renewable_fraction = 0.4,
    project_net_calorific_value_mj_per_tonne = 15.4,
    project_emission_factor_tco2_per_mj = 0.00009
  )

  expect_true(check_applicability_monitoring_iig(monitoring))
  monitoring$project_non_renewable_fraction[1] <- NA_real_
  expect_false(check_applicability_monitoring_iig(monitoring))
  monitoring$project_non_renewable_fraction[1] <- 0.4
  expect_error(check_applicability_monitoring_iig(monitoring, required_cols = c("missing_col")))
})
