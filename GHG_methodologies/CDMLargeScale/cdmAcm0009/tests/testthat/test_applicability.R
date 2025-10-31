test_that("baseline fuel mix must be dominated by eligible fuels", {
  expect_true(check_applicability_baseline_fuels(c("bituminous coal", "heavy fuel oil")))
  expect_false(check_applicability_baseline_fuels(character()))
  expect_false(check_applicability_baseline_fuels(c("bituminous coal", "natural gas")))
  expect_false(check_applicability_baseline_fuels(c("wood chips", "diesel"), min_share = 0.8))
})

test_that("project fuel applicability only accepts natural gas", {
  fuels <- c("natural gas", "LNG", "CNG", "coal")
  result <- check_applicability_project_fuel(fuels)
  expect_equal(result, c(TRUE, TRUE, TRUE, FALSE))
  expect_error(check_applicability_project_fuel(1:3))
})
