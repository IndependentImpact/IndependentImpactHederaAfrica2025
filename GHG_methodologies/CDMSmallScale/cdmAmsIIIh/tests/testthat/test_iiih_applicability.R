library(tibble)

test_that("wastewater applicability checks COD and baseline system", {
  data <- tibble(
    site_id = c("WW1", "WW2"),
    wastewater_type = c("industrial", "domestic"),
    cod_mg_l = c(3200, 1800),
    anaerobic_fraction = c(0.7, 0.3),
    baseline_system = c("open anaerobic lagoon", "covered anaerobic lagoon")
  )
  result <- check_applicability_wastewater_characteristics_iiih(data, group_cols = "site_id")
  expect_equal(result$wastewater_applicable, c(TRUE, FALSE))
})

test_that("recovery system applicability validates capture and operations", {
  data <- tibble(
    site_id = c("WW1", "WW2"),
    gas_capture_installed = c(TRUE, FALSE),
    destruction_technology = c("enclosed flare", "flare"),
    operating_hours_per_week = c(40, 12),
    redundancy_installed = c(TRUE, FALSE),
    utilisation_documented = c(TRUE, FALSE)
  )
  result <- check_applicability_recovery_system_iiih(data, group_cols = "site_id")
  expect_equal(result$recovery_applicable, c(TRUE, FALSE))
})

test_that("monitoring applicability validates sampling and calibrations", {
  data <- tibble(
    site_id = c("WW1", "WW2"),
    flow_measurements_per_week = c(10, 3),
    methane_measurements_per_week = c(5, 1),
    cod_samples_per_month = c(6, 2),
    calibration_events_per_year = c(5, 3)
  )
  result <- check_applicability_monitoring_framework_iiih(data, group_cols = "site_id")
  expect_equal(result$monitoring_applicable, c(TRUE, FALSE))
})
