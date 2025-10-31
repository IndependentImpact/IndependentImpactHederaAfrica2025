test_that("energy savings applicability enforces threshold", {
  expect_true(check_applicability_energy_savings(annual_energy_savings_mwh = 42000))
  expect_false(check_applicability_energy_savings(annual_energy_savings_mwh = 72000))
})

test_that("technology applicability validates catalogue", {
  expect_true(check_applicability_technology(c("efficient_lighting", "efficient_motors")))
  expect_false(check_applicability_technology(c("efficient_lighting", "biomass_boiler")))
})

test_that("monitoring applicability requires approved methods", {
  expect_true(check_applicability_monitoring("continuous_metering"))
  expect_false(check_applicability_monitoring("engineering_estimate"))
})

test_that("combined applicability summary returns tidy tibble", {
  summary <- assess_ams_iic_applicability(
    annual_energy_savings_mwh = 45000,
    technologies = c("efficient_lighting", "hvac_optimization"),
    monitoring_approach = "periodic_sampling"
  )

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$passes))
})
