test_that("energy content multiplies quantity by NCV", {
  quantity <- c(1000, 1200)
  ncv <- c(0.025, 0.026)
  expect_equal(calculate_energy_content_acm0009(quantity, ncv), quantity * ncv)
})

test_that("baseline and project emissions use emission factors", {
  quantity <- c(1000, 1200)
  ncv <- c(0.025, 0.026)
  ef_baseline <- c(94, 96)
  ef_project <- 55
  baseline <- calculate_baseline_emissions_acm0009(quantity, ncv, ef_baseline)
  project <- calculate_project_emissions_acm0009(quantity, ncv, ef_project)
  expect_equal(baseline, quantity * ncv * ef_baseline)
  expect_equal(project, quantity * ncv * ef_project)
})

test_that("leakage converts methane slip and adds additional sources", {
  methane <- c(100, 150)
  additional <- c(2, 3)
  result <- calculate_leakage_emissions_acm0009(methane_slip_m3 = methane, additional_leakage_tco2e = additional)
  expected <- methane * 0.0007168 * 28 + additional
  expect_equal(result, expected)
})

test_that("net emission reductions recycle shorter inputs", {
  baseline <- c(100, 105)
  project <- 60
  leakage <- c(2, 3)
  result <- calculate_net_emission_reductions_acm0009(baseline, project, leakage)
  expect_equal(result, baseline - project - leakage)
})

test_that("equation helpers validate input domains", {
  expect_error(calculate_energy_content_acm0009(-10, 0.02))
  expect_error(calculate_baseline_emissions_acm0009(100, 0.02, -1))
  expect_error(calculate_leakage_emissions_acm0009(methane_density = -1))
  expect_error(calculate_net_emission_reductions_acm0009(c(10, 12), 5, c(1, 2, 3)))
})
