test_that("mini-grid capacity applicability enforces thresholds", {
  expect_true(check_applicability_mini_grid_capacity(capacity_kw = 8000))
  expect_false(check_applicability_mini_grid_capacity(capacity_kw = 20000))
})

test_that("renewable penetration applicability checks fractions", {
  expect_true(check_applicability_renewable_penetration(renewable_fraction = 0.9))
  expect_false(check_applicability_renewable_penetration(renewable_fraction = 0.6, minimum_fraction = 0.8))
})

test_that("baseline fossil share applicability checks displacement", {
  expect_true(check_applicability_baseline_fossil_share(baseline_fossil_share = 0.85))
  expect_false(check_applicability_baseline_fossil_share(baseline_fossil_share = 0.3))
})
