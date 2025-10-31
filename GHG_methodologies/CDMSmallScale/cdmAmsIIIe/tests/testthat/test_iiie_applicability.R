library(tibble)

test_that("feedstock applicability considers type and moisture", {
  data <- tibble(
    plant_id = c("A", "B"),
    feedstock_type = c("agricultural residues", "municipal waste"),
    moisture_fraction = c(0.4, 0.7)
  )
  result <- check_applicability_feedstock_characteristics_iiie(data, group_cols = "plant_id")
  expect_true(result$feedstock_applicable[result$plant_id == "A"])
  expect_false(result$feedstock_applicable[result$plant_id == "B"])
})

test_that("biomass control requires both indicators", {
  data <- tibble(
    plant_id = c("A", "B"),
    anaerobic_baseline = c(TRUE, TRUE),
    biomass_control_plan = c(TRUE, FALSE)
  )
  result <- check_applicability_biomass_control_iiie(data, group_cols = "plant_id")
  expect_true(result$biomass_control_applicable[result$plant_id == "A"])
  expect_false(result$biomass_control_applicable[result$plant_id == "B"])
})

test_that("monitoring applicability checks thresholds", {
  data <- tibble(
    plant_id = c("A", "B"),
    energy_measurements_per_month = c(6, 3),
    operating_hours_per_period = c(450, 350),
    feedstock_samples_per_month = c(3, 1)
  )
  result <- check_applicability_monitoring_practices_iiie(data, group_cols = "plant_id")
  expect_true(result$monitoring_practices_applicable[result$plant_id == "A"])
  expect_false(result$monitoring_practices_applicable[result$plant_id == "B"])
})

test_that("simulation provides expected list elements", {
  simulated <- simulate_ams_iiie_dataset(n_plants = 1, n_periods = 2, seed = 123)
  expect_named(simulated, c("baseline", "project", "leakage", "applicability"))
  expect_equal(nrow(simulated$baseline), 2)
})
