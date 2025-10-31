test_that("captive capacity applicability enforces thresholds", {
  expect_true(check_applicability_captive_capacity(capacity_kw = 8000))
  expect_false(check_applicability_captive_capacity(capacity_kw = 20000))
})

test_that("renewable penetration applicability checks fractions", {
  expect_true(check_applicability_renewable_penetration(renewable_fraction = 0.9))
  expect_false(check_applicability_renewable_penetration(renewable_fraction = 0.6, minimum_fraction = 0.8))
})

test_that("captive use share applicability checks captive demand", {
  expect_true(check_applicability_captive_use_share(captive_use_share = 0.75))
  expect_false(check_applicability_captive_use_share(captive_use_share = 0.4))
})

test_that("combined applicability summary returns tibble", {
  summary <- assess_ams_if_applicability(
    capacity_kw = 9000,
    renewable_fraction = 0.85,
    captive_use_share = 0.7
  )

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$passes))
})
