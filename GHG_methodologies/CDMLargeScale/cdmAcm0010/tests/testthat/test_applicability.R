test_that("feedstock applicability enforces composition thresholds", {
  expect_true(check_applicability_feedstock_mix(c("manure", "wastewater")))
  expect_false(check_applicability_feedstock_mix(c("manure", "municipal solid waste")))
  expect_error(check_applicability_feedstock_mix(1:3))
  expect_error(check_applicability_feedstock_mix(c("manure", "wastewater"), eligible = 2))
})

test_that("baseline system applicability flags eligible configurations", {
  result <- check_applicability_baseline_system(c("open lagoon", "composting"))
  expect_equal(result, c(TRUE, FALSE))
  expect_error(check_applicability_baseline_system(1:2))
})
