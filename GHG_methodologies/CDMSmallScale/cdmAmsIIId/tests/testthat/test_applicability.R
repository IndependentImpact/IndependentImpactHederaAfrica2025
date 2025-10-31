test_that("system type applicability restricts to eligible technologies", {
  metadata <- tibble::tibble(
    farm_id = c("A", "B", "C"),
    system_type = c("Anaerobic Lagoon", "Uncovered Digester", "Composting")
  )
  result <- check_applicability_system_type_iiid(metadata, group_cols = "farm_id")
  expect_equal(result$system_type_applicable, c(TRUE, TRUE, FALSE))
})

test_that("measurement frequency requires at least weekly readings", {
  monitoring <- tibble::tibble(
    farm_id = c("A", "A", "B"),
    measurements_per_month = c(6, 5, 3)
  )
  result <- check_applicability_measurement_frequency_iiid(monitoring, group_cols = "farm_id")
  expect_equal(result$measurement_frequency_applicable, c(TRUE, FALSE))
})

test_that("leakage controls must be documented", {
  controls <- tibble::tibble(
    farm_id = c("A", "B", "C"),
    leakage_controls_in_place = c(TRUE, NA, FALSE)
  )
  result <- check_applicability_leakage_control_iiid(controls, group_cols = "farm_id")
  expect_equal(result$leakage_controls_applicable, c(TRUE, FALSE, FALSE))
})
