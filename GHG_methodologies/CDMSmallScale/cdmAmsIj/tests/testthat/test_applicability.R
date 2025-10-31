test_that("solar thermal capacity applicability enforces thresholds", {
  expect_true(check_applicability_swh_capacity(capacity_mwth = 12))
  expect_false(check_applicability_swh_capacity(capacity_mwth = 60))
})

test_that("solar resource applicability checks irradiation", {
  expect_true(check_applicability_solar_resource(annual_irradiation_kwhm2 = 1800))
  expect_false(check_applicability_solar_resource(annual_irradiation_kwhm2 = 900, minimum_irradiation = 1000))
})

test_that("auxiliary fraction applicability limits backup energy", {
  expect_true(check_applicability_backup_fraction(backup_fraction = 0.1))
  expect_false(check_applicability_backup_fraction(backup_fraction = 0.35, maximum_fraction = 0.25))
})

test_that("combined applicability summary returns tibble", {
  summary <- assess_ams_ij_applicability(
    capacity_mwth = 20,
    annual_irradiation_kwhm2 = 1900,
    backup_fraction = 0.1
  )

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$passes))
})
