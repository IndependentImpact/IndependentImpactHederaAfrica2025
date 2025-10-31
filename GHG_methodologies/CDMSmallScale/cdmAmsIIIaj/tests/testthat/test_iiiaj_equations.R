library(tibble)

test_that("baseline emissions aggregate virgin and disposal components", {
  data <- tibble(
    facility_id = c("F1", "F1", "F2"),
    material_processed_tonnes = c(40, 45, 38),
    virgin_material_ef_tco2_per_tonne = c(1.8, 1.8, 1.6),
    baseline_disposal_ef_tco2_per_tonne = c(0.3, 0.3, 0.28),
    residual_fraction = c(0.05, 0.04, 0.06)
  )
  result <- calculate_baseline_emissions_iiiaj(data, group_cols = "facility_id")
  expected <- c(F1 = 154.14, F2 = 61.4384)
  expect_equal(result$baseline_emissions_tco2e, unname(expected[result$facility_id]))
})

test_that("project emissions combine energy and supplementary inputs", {
  data <- tibble(
    facility_id = c("F1", "F1", "F2"),
    electricity_consumption_mwh = c(12, 13, 11),
    electricity_ef_tco2_per_mwh = c(0.45, 0.46, 0.48),
    thermal_fuel_consumption_gj = c(5, 4, 4.5),
    thermal_fuel_ef_tco2_per_gj = c(0.07, 0.07, 0.068),
    supplementary_material_tonnes = c(0.5, 0.45, 0.4),
    supplementary_material_ef_tco2_per_tonne = c(0.5, 0.45, 0.52)
  )
  result <- calculate_project_emissions_iiiaj(data, group_cols = "facility_id")
  expected <- c(F1 = 12.4625, F2 = 5.794)
  expect_equal(result$project_emissions_tco2e, unname(expected[result$facility_id]))
})

test_that("leakage emissions incorporate transport, residual, and market components", {
  data <- tibble(
    facility_id = c("F1", "F1", "F2"),
    material_collected_tonnes = c(60, 58, 52),
    transport_distance_km = c(18, 20, 16),
    transport_ef_tco2_per_tkm = c(0.00012, 0.00011, 0.0001),
    residual_disposal_tonnes = c(3, 2.8, 2.5),
    residual_disposal_ef_tco2_per_tonne = c(0.32, 0.3, 0.28),
    market_leakage_fraction = c(0.05, 0.02, -0.03),
    market_emission_factor_tco2_per_tonne = c(0.6, 0.55, 0.5)
  )
  result <- calculate_leakage_emissions_iiiaj(data, group_cols = "facility_id")
  expected <- c(F1 = 4.4952, F2 = 0.0032)
  expect_equal(result$leakage_emissions_tco2e, unname(expected[result$facility_id]))
})

test_that("emission reduction helper merges grouped outputs", {
  baseline <- tibble(facility_id = "F1", baseline_emissions_tco2e = 100)
  project <- tibble(facility_id = "F1", project_emissions_tco2e = 40)
  leakage <- tibble(facility_id = "F1", leakage_emissions_tco2e = 5)
  reductions <- calculate_emission_reductions_iiiaj(
    baseline,
    project,
    leakage,
    group_cols = "facility_id"
  )
  expect_equal(reductions$net_emission_reductions_tco2e, 55)
})

test_that("meta workflow returns expected structure", {
  baseline_data <- tibble(
    facility_id = rep(c("F1", "F2"), each = 2),
    period = rep(1:2, times = 2),
    days_in_period = 30,
    material_processed_tonnes = c(40, 42, 38, 36),
    virgin_material_ef_tco2_per_tonne = c(1.8, 1.75, 1.6, 1.55),
    baseline_disposal_ef_tco2_per_tonne = 0.3,
    residual_fraction = c(0.05, 0.05, 0.06, 0.06)
  )
  project_data <- tibble(
    facility_id = rep(c("F1", "F2"), each = 2),
    period = rep(1:2, times = 2),
    days_in_period = 30,
    electricity_consumption_mwh = c(12, 13, 11, 10),
    electricity_ef_tco2_per_mwh = c(0.45, 0.46, 0.48, 0.47),
    thermal_fuel_consumption_gj = c(5, 4.5, 4, 3.8),
    thermal_fuel_ef_tco2_per_gj = c(0.07, 0.07, 0.068, 0.068),
    supplementary_material_tonnes = c(0.5, 0.48, 0.4, 0.38),
    supplementary_material_ef_tco2_per_tonne = c(0.5, 0.48, 0.52, 0.5)
  )
  leakage_data <- tibble(
    facility_id = rep(c("F1", "F2"), each = 2),
    period = rep(1:2, times = 2),
    material_collected_tonnes = c(60, 58, 52, 50),
    transport_distance_km = c(18, 20, 16, 14),
    transport_ef_tco2_per_tkm = c(0.00012, 0.00011, 0.0001, 0.0001),
    residual_disposal_tonnes = c(3, 2.8, 2.5, 2.4),
    residual_disposal_ef_tco2_per_tonne = c(0.32, 0.3, 0.28, 0.28),
    market_leakage_fraction = c(0.05, 0.02, -0.03, -0.02),
    market_emission_factor_tco2_per_tonne = c(0.6, 0.55, 0.5, 0.48)
  )
  reductions <- estimate_emission_reductions_ams_iiiaj(
    baseline_data = baseline_data,
    project_data = project_data,
    leakage_data = leakage_data,
    group_cols = "facility_id",
    baseline_args = list(days_col = "days_in_period"),
    project_args = list(days_col = "days_in_period")
  )
  expect_true(all(c(
    "baseline_emissions_tco2e",
    "project_emissions_tco2e",
    "leakage_emissions_tco2e",
    "net_emission_reductions_tco2e"
  ) %in% colnames(reductions)))
})

test_that("monitoring aggregation sums numeric columns", {
  monitoring <- tibble(
    facility_id = c("F1", "F1", "F2"),
    period = c(1, 2, 1),
    material_processed_tonnes = c(40, 42, 38),
    electricity_consumption_mwh = c(12, 13, 11)
  )
  aggregated <- aggregate_monitoring_periods_iiiaj(monitoring, group_cols = "facility_id")
  expect_equal(
    aggregated$material_processed_tonnes,
    unname(c(F1 = 82, F2 = 38)[aggregated$facility_id])
  )
})
