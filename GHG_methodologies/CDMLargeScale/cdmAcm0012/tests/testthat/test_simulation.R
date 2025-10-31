test_that("simulation generates ACM0012 monitoring records", {
  data <- simulate_acm0012_dataset(3, observations_per_period = 4, seed = 101)
  expect_s3_class(data, "tbl_df")
  expected_cols <- c(
    "period",
    "electricity_export_mwh",
    "baseline_grid_ef_t_per_mwh",
    "thermal_export_gj",
    "baseline_thermal_ef_t_per_gj",
    "flare_gas_displacement_emissions",
    "electricity_import_mwh",
    "project_grid_ef_t_per_mwh",
    "auxiliary_fuel_tj",
    "auxiliary_ef_t_per_tj",
    "methane_leakage_nm3",
    "methane_density_t_per_nm3",
    "gwp_ch4",
    "leakage_energy_mwh",
    "leakage_ef_t_per_mwh",
    "waste_energy_fraction",
    "measurement_uncertainty",
    "baseline_operating_hours",
    "project_operating_hours"
  )
  expect_true(all(expected_cols %in% names(data)))
  expect_equal(nrow(data), 3 * 4)
  expect_true(all(data$electricity_export_mwh >= 0))
})

test_that("aggregated simulation data returns positive reductions", {
  data <- simulate_acm0012_dataset(5, observations_per_period = 6, seed = 2024)
  summary <- aggregate_monitoring_periods(data)
  expect_true(all(summary$baseline_emissions >= summary$project_emissions))
  totals <- estimate_emission_reductions_acm0012(data)
  expect_true(totals$total_emission_reductions >= 0)
  expect_equal(totals$total_baseline_emissions, sum(summary$baseline_emissions))
})
