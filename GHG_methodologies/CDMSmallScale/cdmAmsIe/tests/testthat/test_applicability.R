test_that("capacity limit enforces 45 MWth threshold", {
  expect_true(check_applicability_capacity_limit(thermal_capacity_mw = 10))
  expect_false(check_applicability_capacity_limit(thermal_capacity_mw = 50))
})

test_that("non-renewable fraction validation", {
  expect_true(check_applicability_non_renewable_fraction(c(0.6, 0.8, 0.9)))
  expect_false(check_applicability_non_renewable_fraction(numeric(0)))
  expect_false(check_applicability_non_renewable_fraction(c(0.4, 1.2)))
})

test_that("renewable project fraction must exceed threshold", {
  expect_true(check_applicability_project_renewable_fraction(0.95))
  expect_false(check_applicability_project_renewable_fraction(c(0.95, 0.85)))
})
