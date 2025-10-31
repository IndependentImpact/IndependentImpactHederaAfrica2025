test_that("landfill management applicability enforces sanitary control", {
  expect_true(check_applicability_landfill_management("sanitary", TRUE))
  expect_false(check_applicability_landfill_management("controlled", FALSE))
  expect_false(check_applicability_landfill_management("open dump", TRUE))
})

test_that("gas collection applicability requires installation and efficiency", {
  expect_true(check_applicability_gas_collection(TRUE, 0.35, minimum_efficiency = 0.2))
  expect_false(check_applicability_gas_collection(TRUE, 0.1, minimum_efficiency = 0.2))
  expect_false(check_applicability_gas_collection(FALSE, 0.5, minimum_efficiency = 0.2))
})
