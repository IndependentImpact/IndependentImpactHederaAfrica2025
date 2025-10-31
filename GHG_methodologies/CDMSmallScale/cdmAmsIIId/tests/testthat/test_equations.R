test_that("baseline methane emissions aggregate correctly", {
  baseline <- tibble::tibble(
    farm_id = c("A", "A", "B"),
    volatile_solids_kg_per_day = c(40, 42, 38),
    methane_potential_m3_per_kg_vs = c(0.25, 0.25, 0.23),
    baseline_mcf_fraction = c(0.8, 0.75, 0.78),
    days_in_period = c(30, 30, 30)
  )
  result <- calculate_baseline_methane_emissions_iiid(baseline, group_cols = "farm_id")
  factor <- 0.00067 * 28
  expected_a <- sum(baseline$volatile_solids_kg_per_day[1:2] * 0.25 * c(0.8, 0.75) * 30 * factor)
  expected_b <- baseline$volatile_solids_kg_per_day[3] * 0.23 * 0.78 * 30 * factor
  expect_equal(result$baseline_emissions_tco2e[result$farm_id == "A"], expected_a)
  expect_equal(result$baseline_emissions_tco2e[result$farm_id == "B"], expected_b)
})

test_that("project methane emissions account for capture and destruction", {
  project <- tibble::tibble(
    farm_id = c("A", "B"),
    volatile_solids_kg_per_day = c(40, 32),
    methane_potential_m3_per_kg_vs = 0.24,
    project_mcf_fraction = c(0.6, 0.55),
    capture_efficiency_fraction = c(0.9, 0.85),
    destruction_efficiency_fraction = c(0.98, 0.95),
    days_in_period = 30
  )
  result <- calculate_project_methane_emissions_iiid(project, group_cols = "farm_id")
  factor <- 0.00067 * 28
  generated <- project$volatile_solids_kg_per_day * 0.24 * project$project_mcf_fraction * 30 * factor
  destroyed <- generated * project$capture_efficiency_fraction * project$destruction_efficiency_fraction
  expected <- generated - destroyed
  expect_equal(result$project_emissions_tco2e, expected)
})

test_that("recovered methane converts to destroyed emissions", {
  recovery <- tibble::tibble(
    digester_id = c("D1", "D2"),
    methane_recovered_m3 = c(12000, 15000),
    destruction_efficiency_fraction = c(0.99, 0.97)
  )
  result <- calculate_recovered_methane_iiid(recovery, group_cols = "digester_id")
  factor <- 0.00067 * 28
  expected <- recovery$methane_recovered_m3 * factor * recovery$destruction_efficiency_fraction
  expect_equal(result$recovered_methane_tco2e, expected)
})

test_that("emission reductions combine baseline, project, recovery, and leakage", {
  baseline <- tibble::tibble(farm_id = "A", baseline_emissions_tco2e = 100)
  project <- tibble::tibble(farm_id = "A", project_emissions_tco2e = 20)
  recovered <- tibble::tibble(farm_id = "A", recovered_methane_tco2e = 70)
  leakage <- tibble::tibble(farm_id = "A", leakage_emissions_tco2e = 5)
  reductions <- calculate_emission_reductions_iiid(
    baseline,
    project,
    recovered,
    leakage,
    group_cols = "farm_id"
  )
  expect_equal(reductions$emission_reductions_tco2e, 100 - 20 + 70 - 5)
})

test_that("meta workflow matches manual calculation", {
  monitoring <- simulate_ams_iiid_dataset(n_farms = 3, n_periods = 4, seed = 42)

  baseline <- calculate_baseline_methane_emissions_iiid(
    monitoring,
    group_cols = "farm_id"
  )

  project <- calculate_project_methane_emissions_iiid(
    monitoring,
    group_cols = "farm_id"
  )

  recovered <- calculate_recovered_methane_iiid(
    monitoring,
    group_cols = "farm_id"
  )

  leakage <- calculate_leakage_emissions_iiid(
    monitoring,
    group_cols = "farm_id"
  )

  manual <- calculate_emission_reductions_iiid(
    baseline,
    project,
    recovered,
    leakage,
    group_cols = "farm_id"
  )

  meta <- estimate_emission_reductions_ams_iiid(
    monitoring,
    monitoring,
    recovery_data = monitoring,
    leakage_data = monitoring,
    group_cols = "farm_id"
  )

  expect_equal(
    dplyr::arrange(meta, farm_id)$emission_reductions_tco2e,
    dplyr::arrange(manual, farm_id)$emission_reductions_tco2e
  )
})

test_that("monitoring aggregation sums flows and averages coefficients", {
  monitoring <- tibble::tibble(
    farm_id = rep("A", 2),
    monitoring_period = 1:2,
    volatile_solids_kg_per_day = c(30, 32),
    methane_recovered_m3 = c(1000, 1200),
    methane_potential_m3_per_kg_vs = c(0.23, 0.25),
    baseline_mcf_fraction = c(0.8, 0.82),
    project_mcf_fraction = c(0.6, 0.58),
    capture_efficiency_fraction = c(0.9, 0.88),
    destruction_efficiency_fraction = c(0.98, 0.97),
    days_in_period = c(30, 30)
  )
  aggregated <- aggregate_monitoring_periods_iiid(monitoring, group_cols = "farm_id")
  expect_equal(aggregated$volatile_solids_kg_per_day, sum(monitoring$volatile_solids_kg_per_day))
  expect_equal(aggregated$methane_recovered_m3, sum(monitoring$methane_recovered_m3))
  expect_equal(aggregated$baseline_mcf_fraction, mean(monitoring$baseline_mcf_fraction))
  expect_equal(aggregated$capture_efficiency_fraction, mean(monitoring$capture_efficiency_fraction))
})
