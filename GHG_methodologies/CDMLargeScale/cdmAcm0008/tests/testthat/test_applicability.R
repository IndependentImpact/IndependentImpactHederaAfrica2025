test_that("mine type applicability validates inputs", {
  expect_true(check_applicability_mine_type("underground", TRUE))
  expect_false(check_applicability_mine_type("surface", FALSE))
  expect_error(check_applicability_mine_type(1, TRUE))
  expect_error(check_applicability_mine_type("underground", c(TRUE, FALSE)))
})

test_that("methane content thresholds are enforced", {
  expect_true(all(check_applicability_methane_content(c(0.35, 0.4))))
  expect_false(check_applicability_methane_content(0.2))
  expect_error(check_applicability_methane_content(c(0.5, NA)))
  expect_error(check_applicability_methane_content(0.4, minimum_fraction = 2))
})

test_that("utilisation pathways require enclosed combustion", {
  expect_equal(
    check_applicability_utilisation_pathway(
      c("electricity", "venting"),
      c(TRUE, TRUE)
    ),
    c(TRUE, FALSE)
  )
  expect_error(check_applicability_utilisation_pathway(1, TRUE))
  expect_error(check_applicability_utilisation_pathway("flaring", c(TRUE, FALSE)))
})
