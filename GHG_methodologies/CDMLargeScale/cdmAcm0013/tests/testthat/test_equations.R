test_that("baseline emissions scale with electricity generation", {
  baseline <- calculate_baseline_emissions_acm0013(40000, 0.85)
  expect_equal(baseline, 40000 * 0.85)
  expect_error(calculate_baseline_emissions_acm0013(-1, 0.8))
})

test_that("project emissions incorporate fuel and auxiliary electricity", {
  emissions <- calculate_project_emissions_acm0013(
    fuel_consumed = 1e5,
    fuel_emission_factor_tco2_per_unit = 7.2e-5,
    auxiliary_electricity_mwh = 500,
    auxiliary_emission_factor_tco2_per_mwh = 0.9,
    other_project_emissions_tco2e = 10
  )
  expect_gt(emissions, 0)
  expect_error(calculate_project_emissions_acm0013("100", 0.0001))
  expect_error(calculate_project_emissions_acm0013(100, -0.1))
})

test_that("leakage emissions are additive", {
  leakage <- calculate_leakage_emissions_acm0013(5, 12, 3)
  expect_equal(leakage, 20)
  expect_error(calculate_leakage_emissions_acm0013(-1, 0, 0))
})

test_that("net emission reductions subtract project and leakage", {
  net <- calculate_net_emission_reductions_acm0013(50000, 42000, 1200)
  expect_equal(net, 50000 - 42000 - 1200)
  expect_error(calculate_net_emission_reductions_acm0013("50", 10, 1))
})
