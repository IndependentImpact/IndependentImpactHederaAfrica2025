test_that("feedstock eligibility recognises renewable biomass", {
  feedstocks <- c("agricultural residues", "coal")
  result <- check_applicability_biomass_feedstock_acm0018(feedstocks)
  expect_equal(result, c(TRUE, FALSE))
})

test_that("power-only applicability requires electricity export and no heat", {
  result <- check_applicability_power_only_acm0018(
    exports_heat = c(FALSE, TRUE),
    generates_electricity = c(TRUE, TRUE)
  )
  expect_equal(result, c(TRUE, FALSE))
})

test_that("biomass fraction threshold defaults to 80 percent", {
  fractions <- c(0.85, 0.75)
  expect_equal(
    check_applicability_biomass_fraction_acm0018(fractions),
    c(TRUE, FALSE)
  )
})
