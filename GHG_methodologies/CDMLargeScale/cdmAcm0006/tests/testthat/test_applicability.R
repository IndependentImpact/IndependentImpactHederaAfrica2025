test_that("biomass feedstock applicability works", {
  expect_true(check_applicability_biomass_feedstock("agricultural residues"))
  expect_false(check_applicability_biomass_feedstock("coal"))
})

test_that("biomass fraction applicability works", {
  expect_true(check_applicability_biomass_fraction(0.95))
  expect_false(check_applicability_biomass_fraction(0.85))
  expect_error(check_applicability_biomass_fraction(1.2))
})
