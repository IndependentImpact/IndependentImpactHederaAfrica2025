test_that("grid connection applicability requires sufficient export", {
  expect_true(check_applicability_grid_connection("grid-connected", export_share = 0.95))
  expect_false(check_applicability_grid_connection("grid-connected", export_share = 0.5))
  expect_false(check_applicability_grid_connection("isolated", export_share = 1))
})

test_that("renewable technology applicability restricts to ACM0002 list", {
  expect_true(check_applicability_renewable_technology("wind"))
  expect_true(check_applicability_renewable_technology("Solar"))
  expect_false(check_applicability_renewable_technology("coal"))
})
