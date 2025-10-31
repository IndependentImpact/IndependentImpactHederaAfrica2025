test_that("simulation emits required monitoring columns", {
  data <- simulate_ams_iig_dataset(n_sites = 2, n_periods = 3, seed = 123)
  expect_equal(nrow(data), 6)
  required <- c(
    "site_id",
    "monitoring_label",
    "baseline_biomass_consumption_tonnes",
    "baseline_non_renewable_fraction",
    "baseline_net_calorific_value_mj_per_tonne",
    "baseline_emission_factor_tco2_per_mj",
    "project_biomass_consumption_tonnes",
    "project_non_renewable_fraction",
    "project_net_calorific_value_mj_per_tonne",
    "project_emission_factor_tco2_per_mj",
    "leakage_emissions_tco2e"
  )
  expect_true(all(required %in% names(data)))
  expect_true(all(data$baseline_biomass_consumption_tonnes > 0))
  expect_true(all(data$project_biomass_consumption_tonnes > 0))
  expect_true(all(data$baseline_non_renewable_fraction >= data$project_non_renewable_fraction))
})

test_that("aggregator rolls monitoring data to reporting periods", {
  data <- simulate_ams_iig_dataset(n_sites = 2, n_periods = 2, seed = 42)
  summary <- aggregate_monitoring_periods_iig(data)
  expect_equal(nrow(summary), 4)
  expect_true(all(summary$baseline_biomass_consumption_tonnes > summary$project_biomass_consumption_tonnes))
  expect_true(all(summary$baseline_non_renewable_fraction >= summary$project_non_renewable_fraction))
})

test_that("aggregated data feeds meta workflow", {
  data <- simulate_ams_iig_dataset(n_sites = 1, n_periods = 2, seed = 99)
  summary <- aggregate_monitoring_periods_iig(data, group_cols = "site_id", period_cols = "monitoring_label")
  reductions <- estimate_emission_reductions_ams_iig(
    baseline_data = summary,
    project_data = summary,
    leakage_data = summary,
    group_cols = c("site_id", "monitoring_label"),
    leakage_col = "leakage_emissions_tco2e"
  )
  expect_equal(nrow(reductions), 2)
  expect_true(all(!is.na(reductions$emission_reductions_tco2e)))
})
