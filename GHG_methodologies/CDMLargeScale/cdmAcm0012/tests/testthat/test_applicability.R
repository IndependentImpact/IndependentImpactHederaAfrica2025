test_that("waste energy fraction must meet threshold", {
  expect_true(check_waste_energy_fraction_acm0012(0.8))
  expect_false(check_waste_energy_fraction_acm0012(0.7))
})

test_that("baseline continuation requires similar operating hours", {
  expect_true(check_baseline_continuation_acm0012(7800, 7500))
  expect_false(check_baseline_continuation_acm0012(6000, 7500))
})

test_that("metering uncertainty must be below 1.5%", {
  expect_true(all(check_metering_uncertainty_acm0012(c(0.01, 0.015))))
  expect_false(any(check_metering_uncertainty_acm0012(0.02)))
})
