library(tibble)

test_that("landfill applicability requires eligible status and readiness", {
  data <- tibble(
    site_id = c("LF1", "LF2"),
    landfill_status = c("open dump", "sanitary landfill"),
    biodegradable_fraction = c(0.55, 0.32),
    gas_collection_ready = c(TRUE, FALSE)
  )
  result <- check_applicability_landfill_characteristics_iiig(data, group_cols = "site_id")
  expect_equal(result$landfill_applicable, c(TRUE, FALSE))
})

test_that("gas management applicability checks technology and hours", {
  data <- tibble(
    site_id = c("LF1", "LF2"),
    destruction_technology = c("enclosed flare", "open flare"),
    operating_hours_per_week = c(35, 12),
    redundancy_installed = c(TRUE, FALSE)
  )
  result <- check_applicability_gas_management_iiig(data, group_cols = "site_id")
  expect_equal(result$gas_management_applicable, c(TRUE, FALSE))
})

test_that("monitoring applicability validates sampling frequency", {
  data <- tibble(
    site_id = c("LF1", "LF2"),
    flow_measurements_per_week = c(6, 2),
    methane_measurements_per_week = c(5, 1),
    calibration_events_per_year = c(5, 3)
  )
  result <- check_applicability_monitoring_framework_iiig(data, group_cols = "site_id")
  expect_equal(result$monitoring_applicable, c(TRUE, FALSE))
})
