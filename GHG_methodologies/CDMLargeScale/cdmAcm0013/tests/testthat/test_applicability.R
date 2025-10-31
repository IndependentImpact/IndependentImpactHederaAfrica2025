test_that("applicability requires new grid-connected plants", {
  data <- simulate_acm0013_dataset(periods = 2, seed = 1)
  expect_true(check_applicability_acm0013(data))

  data$is_new_plant[1] <- FALSE
  expect_false(check_applicability_acm0013(data))
})

test_that("applicability fails when improvement threshold not met", {
  data <- tibble::tibble(
    is_new_plant = TRUE,
    grid_connected = TRUE,
    technology_emission_factor_tco2_per_mwh = c(0.85, 0.84),
    baseline_emission_factor_tco2_per_mwh = c(0.9, 0.89)
  )

  expect_false(check_applicability_acm0013(data, efficiency_improvement_threshold = 0.15))
  expect_error(check_applicability_acm0013("not a tibble"))
})
