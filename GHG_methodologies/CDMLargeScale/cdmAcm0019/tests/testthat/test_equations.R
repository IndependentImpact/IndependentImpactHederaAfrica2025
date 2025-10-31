test_that("baseline and project emissions convert kg to tCO2e", {
  baseline <- calculate_baseline_emissions_acm0019(1000, 10, gwp_n2o = 265)
  project <- calculate_project_emissions_acm0019(1000, 2, gwp_n2o = 265)
  expect_equal(baseline, 1000 * 10 / 1000 * 265)
  expect_equal(project, 1000 * 2 / 1000 * 265)
})

test_that("leakage emissions sum electricity and steam pathways", {
  leakage <- calculate_leakage_emissions_acm0019(c(10, 12), c(0.7, 0.65), c(5, 6), c(0.09, 0.08))
  expected <- c(10 * 0.7 + 5 * 0.09, 12 * 0.65 + 6 * 0.08)
  expect_equal(leakage, expected)
})

test_that("net emission reductions recycle scalars", {
  baseline <- c(100, 110)
  project <- 20
  leakage <- c(5, 6)
  result <- calculate_net_emission_reductions_acm0019(baseline, project, leakage)
  expect_equal(result, baseline - project - leakage)
})

test_that("equation helpers validate inputs", {
  expect_error(calculate_baseline_emissions_acm0019(-1, 5))
  expect_error(calculate_project_emissions_acm0019(10, -3))
  expect_error(calculate_leakage_emissions_acm0019(1, c(0.5, 0.6)))
  expect_error(calculate_net_emission_reductions_acm0019(c(10, 12), 5, c(1, 2, 3)))
})
