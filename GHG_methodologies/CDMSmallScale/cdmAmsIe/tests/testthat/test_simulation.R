test_that("simulation produces expected structure", {
  data <- simulate_ams_ie_dataset(n_users = 2, n_periods = 3, seed = 123)
  expect_equal(nrow(data), 6)
  expect_true(all(c(
    "user_id",
    "year",
    "month",
    "biomass_consumption_tonnes",
    "non_renewable_fraction",
    "net_calorific_value",
    "emission_factor",
    "project_energy_mj",
    "project_emission_factor"
  ) %in% names(data)))
  expect_true(all(data$biomass_consumption_tonnes >= 6))
  expect_true(all(data$non_renewable_fraction >= 0.5 & data$non_renewable_fraction <= 1))
})

test_that("aggregator summarises monitoring periods", {
  data <- simulate_ams_ie_dataset(n_users = 2, n_periods = 2, seed = 42)
  summary <- aggregate_monitoring_periods(data)
  expect_equal(nrow(summary), 4)
  expect_true(all(summary$baseline_emissions_tco2e >= 0))
  expect_true(all(summary$emission_reductions_tco2e <= summary$baseline_emissions_tco2e))
})
