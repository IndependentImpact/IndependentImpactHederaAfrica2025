
test_that("installed capacity applicability enforces thresholds", {
  expect_true(check_applicability_installed_capacity(capacity_kw = 1000, renewable_fraction = 1))
  expect_false(check_applicability_installed_capacity(capacity_kw = 16000, renewable_fraction = 1))
  expect_false(check_applicability_installed_capacity(capacity_kw = 1000, renewable_fraction = 0.9))
})

test_that("distributed generation applicability checks fossil displacement", {
  expect_true(check_applicability_distributed_generation(fossil_fraction_baseline = 0.8))
  expect_false(check_applicability_distributed_generation(fossil_fraction_baseline = 0.2))
})
