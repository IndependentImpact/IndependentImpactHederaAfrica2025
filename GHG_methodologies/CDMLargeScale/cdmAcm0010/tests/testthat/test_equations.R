test_that("methane volume conversion operates elementwise", {
  expect_equal(convert_methane_volume_to_mass(100), 100 * 0.0007168)
  expect_error(convert_methane_volume_to_mass("100"))
})

test_that("COD removal respects concentration relationships", {
  expect_error(calculate_cod_removed_acm0010(100, 200, 50))
  expect_error(calculate_cod_removed_acm0010(4000, 800, -5))

  result <- calculate_cod_removed_acm0010(4000, 800, 250)
  expect_gt(result, 0)
})

test_that("baseline emissions respond to methane conversion factor", {
  cod_removed <- calculate_cod_removed_acm0010(3500, 700, 200)
  baseline_low <- calculate_baseline_emissions_acm0010(cod_removed, 0.6)
  baseline_high <- calculate_baseline_emissions_acm0010(cod_removed, 0.9)
  expect_lt(baseline_low, baseline_high)
  expect_error(calculate_baseline_emissions_acm0010(cod_removed, 1.2))
})

test_that("project and leakage emissions sum correctly", {
  project <- calculate_project_emissions_acm0010(1200, 0.95)
  leakage <- calculate_leakage_emissions_acm0010(10, 0.8, 2)
  net <- calculate_net_emission_reductions_acm0010(50, project, leakage)

  expect_lt(project, 50)
  expect_equal(net, 50 - project - leakage)
})
