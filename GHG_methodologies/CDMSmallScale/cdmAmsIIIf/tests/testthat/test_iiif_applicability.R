library(tibble)

test_that("feedstock management applicability checks waste, segregation, and contamination", {
  data <- tibble(
    site_id = c("A", "B"),
    waste_type = c("municipal organics", "mixed waste"),
    source_segregated = c(TRUE, FALSE),
    contamination_fraction = c(0.06, 0.18)
  )
  result <- check_applicability_feedstock_management_iiif(data, group_cols = "site_id")
  expect_true(result$feedstock_applicable[result$site_id == "A"])
  expect_false(result$feedstock_applicable[result$site_id == "B"])
})

test_that("composting practices require compliant aeration and retention", {
  data <- tibble(
    site_id = c("A", "B"),
    aeration_method = c("forced aeration", "static pile"),
    retention_days = c(50, 30),
    leachate_managed = c(TRUE, TRUE),
    curing_phase = c(TRUE, FALSE)
  )
  result <- check_applicability_composting_practices_iiif(data, group_cols = "site_id")
  expect_true(result$composting_practices_applicable[result$site_id == "A"])
  expect_false(result$composting_practices_applicable[result$site_id == "B"])
})

test_that("monitoring framework thresholds are enforced", {
  data <- tibble(
    site_id = c("A", "B"),
    organic_samples_per_year = c(14, 6),
    temperature_checks_per_week = c(4, 2),
    moisture_checks_per_week = c(3, 1)
  )
  result <- check_applicability_monitoring_framework_iiif(data, group_cols = "site_id")
  expect_true(result$monitoring_framework_applicable[result$site_id == "A"])
  expect_false(result$monitoring_framework_applicable[result$site_id == "B"])
})

test_that("simulation provides expected list elements", {
  simulated <- simulate_ams_iiif_dataset(n_sites = 1, n_periods = 2, seed = 123)
  expect_named(simulated, c("baseline", "project", "leakage", "applicability"))
  expect_equal(nrow(simulated$baseline), 2)
})
