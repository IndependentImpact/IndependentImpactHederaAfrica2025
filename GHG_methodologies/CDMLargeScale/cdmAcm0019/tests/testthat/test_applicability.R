test_that("monitoring coverage enforces threshold", {
  expect_true(all(check_applicability_monitoring_coverage(c(0.95, 0.91))))
  expect_false(check_applicability_monitoring_coverage(0.85))
  expect_error(check_applicability_monitoring_coverage(c("a")))
})

test_that("catalyst configuration limits to allowed set", {
  expect_true(all(check_applicability_catalyst_configuration(c("Secondary", "tertiary"))))
  expect_false(check_applicability_catalyst_configuration("primary"))
  expect_error(check_applicability_catalyst_configuration(1))
})

test_that("operating regime requires monitoring", {
  expect_true(check_applicability_operating_regime("weak-acid", TRUE))
  expect_false(check_applicability_operating_regime("weak-acid", FALSE))
  expect_false(check_applicability_operating_regime("batch", TRUE))
  expect_error(check_applicability_operating_regime("weak-acid", NA))
})
