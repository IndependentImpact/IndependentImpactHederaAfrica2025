library(tibble)

test_that("material quality checks ensure contamination and standards", {
  data <- tibble(
    facility_id = c("F1", "F1", "F2"),
    material_type = c("paper", "plastics", "glass"),
    contamination_rate = c(0.05, 0.08, 0.12),
    specification_met = c(TRUE, TRUE, TRUE)
  )
  result <- check_applicability_material_quality_iiiaj(data, group_cols = "facility_id")
  expect_equal(
    result$material_types_compliant,
    unname(c(F1 = TRUE, F2 = TRUE)[result$facility_id])
  )
  expect_equal(
    result$contamination_compliant,
    unname(c(F1 = TRUE, F2 = FALSE)[result$facility_id])
  )
})

test_that("collection network checks enforce thresholds", {
  data <- tibble(
    facility_id = c("F1", "F2"),
    segregation_rate = c(0.72, 0.55),
    coverage_rate = c(0.82, 0.72),
    logistics_score = c(0.78, 0.68)
  )
  result <- check_applicability_collection_network_iiiaj(data, group_cols = "facility_id")
  expect_equal(
    result$segregation_compliant,
    unname(c(F1 = TRUE, F2 = FALSE)[result$facility_id])
  )
  expect_equal(
    result$coverage_compliant,
    unname(c(F1 = TRUE, F2 = TRUE)[result$facility_id])
  )
  expect_equal(
    result$logistics_compliant,
    unname(c(F1 = TRUE, F2 = FALSE)[result$facility_id])
  )
})

test_that("monitoring plan checks require calibration events", {
  data <- tibble(
    facility_id = c("F1", "F2"),
    throughput_monitoring = c(TRUE, TRUE),
    residual_tracking = c(TRUE, FALSE),
    calibration_events = c(2, 0)
  )
  result <- check_applicability_monitoring_plan_iiiaj(data, group_cols = "facility_id")
  expect_equal(
    result$throughput_compliant,
    unname(c(F1 = TRUE, F2 = TRUE)[result$facility_id])
  )
  expect_equal(
    result$residual_compliant,
    unname(c(F1 = TRUE, F2 = FALSE)[result$facility_id])
  )
  expect_equal(
    result$calibration_compliant,
    unname(c(F1 = TRUE, F2 = FALSE)[result$facility_id])
  )
})
