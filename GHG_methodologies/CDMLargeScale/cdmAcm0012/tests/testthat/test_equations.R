test_that("baseline electricity emissions scale linearly", {
  expect_equal(
    calculate_baseline_electricity_emissions_acm0012(100, 0.8),
    80
  )
})

test_that("baseline thermal emissions scale linearly", {
  expect_equal(
    calculate_baseline_thermal_emissions_acm0012(200, 0.05),
    10
  )
})

test_that("total baseline emissions aggregate components", {
  expect_equal(
    calculate_baseline_emissions_acm0012(80, 10, 5),
    95
  )
})

test_that("project emissions add electricity, auxiliary fuel, and methane", {
  electricity <- calculate_project_electricity_emissions_acm0012(5, 0.7)
  auxiliary <- calculate_auxiliary_fuel_emissions_acm0012(0.1, 50)
  methane <- calculate_methane_leakage_emissions_acm0012(100)
  total <- calculate_project_emissions_acm0012(electricity, auxiliary, methane)

  expect_equal(total, electricity + auxiliary + methane)
})

test_that("leakage emissions follow simple multiplication", {
  expect_equal(
    calculate_leakage_emissions_acm0012(10, 0.2),
    2
  )
})

test_that("emission reductions subtract project and leakage from baseline", {
  expect_equal(
    calculate_emission_reductions_acm0012(100, 20, 5),
    75
  )
})

test_that("aggregate_monitoring_periods summarises waste energy data", {
  data <- tibble::tibble(
    period = rep(c("P1", "P2"), each = 2),
    electricity_export_mwh = c(80, 82, 90, 88),
    baseline_grid_ef_t_per_mwh = 0.8,
    thermal_export_gj = c(120, 125, 130, 128),
    baseline_thermal_ef_t_per_gj = 0.055,
    flare_gas_displacement_emissions = c(1, 1.2, 0.8, 1),
    electricity_import_mwh = c(6, 6.5, 7, 7.2),
    project_grid_ef_t_per_mwh = 0.75,
    auxiliary_fuel_tj = c(0.08, 0.082, 0.085, 0.09),
    auxiliary_ef_t_per_tj = 56,
    methane_leakage_nm3 = c(40, 42, 38, 41),
    methane_density_t_per_nm3 = 0.000716,
    gwp_ch4 = 28,
    leakage_energy_mwh = c(4, 4.2, 4.4, 4.5),
    leakage_ef_t_per_mwh = 0.08
  )

  summary <- aggregate_monitoring_periods(data)
  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 2)
  expect_true(all(summary$baseline_emissions > summary$project_emissions))
  expect_equal(
    summary$emission_reductions,
    summary$baseline_emissions - summary$project_emissions - summary$leakage_emissions
  )
})

test_that("estimate_emission_reductions_acm0012 matches aggregated totals", {
  data <- simulate_acm0012_dataset(3, observations_per_period = 5, seed = 321)
  summary <- aggregate_monitoring_periods(data)
  totals <- estimate_emission_reductions_acm0012(data)

  expect_equal(totals$total_baseline_emissions, sum(summary$baseline_emissions))
  expect_equal(totals$total_emission_reductions, sum(summary$emission_reductions))
})
