
test_that("mechanical capacity applicability enforces thresholds", {
  expect_true(check_applicability_mechanical_capacity(capacity_kw = 1000))
  expect_false(check_applicability_mechanical_capacity(capacity_kw = 16000))
  expect_error(check_applicability_mechanical_capacity(capacity_kw = -10), "non-negative")
})

test_that("renewable driver applicability enforces minimum share", {
  expect_true(check_applicability_renewable_driver(renewable_fraction = 0.95))
  expect_false(check_applicability_renewable_driver(renewable_fraction = 0.7))
  expect_error(check_applicability_renewable_driver(renewable_fraction = 1.2), "between 0 and 1")
})

test_that("service displacement applicability validates fossil share", {
  expect_true(check_applicability_service_displacement(fossil_service_share = 0.8))
  expect_false(check_applicability_service_displacement(fossil_service_share = 0.3))
  expect_error(check_applicability_service_displacement(fossil_service_share = -0.1), "between 0 and 1")
})
