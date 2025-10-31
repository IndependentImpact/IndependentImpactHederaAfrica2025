test_that("thermal capacity applicability enforces thresholds", {
  expect_true(check_applicability_thermal_capacity(capacity_mwth = 20))
  expect_false(check_applicability_thermal_capacity(capacity_mwth = 60))
})

test_that("renewable supply applicability checks fractions", {
  expect_true(check_applicability_renewable_supply(renewable_fraction = 0.85))
  expect_false(check_applicability_renewable_supply(renewable_fraction = 0.5, minimum_fraction = 0.75))
})

test_that("fossil displacement applicability checks baseline share", {
  expect_true(check_applicability_fossil_displacement(fossil_heat_share = 0.8))
  expect_false(check_applicability_fossil_displacement(fossil_heat_share = 0.3))
})
