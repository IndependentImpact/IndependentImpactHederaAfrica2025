test_that("simulation emits required monitoring columns", {
  data <- simulate_ams_iiia_dataset(n_farms = 2, n_periods = 3, seed = 123)
  expect_equal(nrow(data), 6)
  required <- c(
    "farm_id",
    "monitoring_period",
    "baseline_synthetic_n_kg",
    "baseline_production_ef_tco2_per_kg",
    "baseline_field_ef_tco2_per_kg",
    "project_synthetic_n_kg",
    "project_production_ef_tco2_per_kg",
    "project_field_ef_tco2_per_kg",
    "inoculant_rate_kg_per_ha",
    "legume_area_ha",
    "total_area_ha",
    "inoculant_ef_tco2_per_kg",
    "inoculant_registered",
    "leakage_emissions_tco2e"
  )
  expect_true(all(required %in% names(data)))
  expect_true(all(data$baseline_synthetic_n_kg > data$project_synthetic_n_kg))
  expect_true(all(data$inoculant_registered %in% c(TRUE, FALSE)))
})

test_that("aggregator rolls monitoring data to farms", {
  data <- simulate_ams_iiia_dataset(n_farms = 3, n_periods = 2, seed = 42)
  aggregated <- data |>
    dplyr::mutate(
      baseline_emissions_tco2e = baseline_synthetic_n_kg * (baseline_production_ef_tco2_per_kg + baseline_field_ef_tco2_per_kg),
      project_fertilizer_emissions_tco2e = project_synthetic_n_kg * (project_production_ef_tco2_per_kg + project_field_ef_tco2_per_kg),
      project_inoculant_emissions_tco2e = inoculant_rate_kg_per_ha * legume_area_ha * inoculant_ef_tco2_per_kg
    ) |>
    aggregate_monitoring_periods_iiia(group_cols = "farm_id", monitoring_col = "monitoring_period")

  expect_equal(nrow(aggregated), 3)
  expect_true(all(aggregated$monitoring_periods == 2))
  expect_true(all(aggregated$baseline_emissions_tco2e > aggregated$project_fertilizer_emissions_tco2e))
})

test_that("aggregated data feeds meta workflow", {
  data <- simulate_ams_iiia_dataset(n_farms = 1, n_periods = 2, seed = 77)
  aggregated <- data |>
    dplyr::mutate(
      baseline_emissions_tco2e = baseline_synthetic_n_kg * (baseline_production_ef_tco2_per_kg + baseline_field_ef_tco2_per_kg),
      project_fertilizer_emissions_tco2e = project_synthetic_n_kg * (project_production_ef_tco2_per_kg + project_field_ef_tco2_per_kg),
      project_inoculant_emissions_tco2e = inoculant_rate_kg_per_ha * legume_area_ha * inoculant_ef_tco2_per_kg
    )

  summary <- aggregate_monitoring_periods_iiia(
    aggregated,
    group_cols = "farm_id",
    monitoring_col = "monitoring_period"
  )

  reductions <- estimate_emission_reductions_ams_iiia(
    baseline_data = data,
    project_data = data,
    leakage_data = data,
    group_cols = "farm_id",
    leakage_col = "leakage_emissions_tco2e"
  )

  expect_equal(nrow(summary), 1)
  expect_equal(nrow(reductions), 1)
  expect_true(all(!is.na(reductions$emission_reductions_tco2e)))
})
