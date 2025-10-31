test_that("baseline emissions combine heat and electricity", {
  baseline <- calculate_baseline_emissions_acm0006(10, 80, 500, 0.7)
  expect_equal(baseline, 10 * 80 + 500 * 0.7)
})

test_that("project emissions cover auxiliary, imports, and transport", {
  project <- calculate_project_emissions_acm0006(0.2, 74, 20, 0.7, 100, 0.0001)
  expect_equal(project, 0.2 * 74 + 20 * 0.7 + 100 * 0.0001)
})

test_that("leakage emissions apply fraction to baseline", {
  leakage <- calculate_leakage_emissions_acm0006(0.05, 1000)
  expect_equal(leakage, 50)
})

test_that("emission reductions subtract project and leakage", {
  reductions <- calculate_emission_reductions_acm0006(1200, 150, 30)
  expect_equal(reductions, 1020)
})
