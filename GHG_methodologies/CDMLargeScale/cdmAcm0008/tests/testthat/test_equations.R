test_that("methane volume calculation is multiplicative", {
  expect_equal(calculate_methane_volume_acm0008(500, 720), 360000)
  expect_equal(
    calculate_methane_volume_acm0008(c(450, 520), c(700, 680)),
    c(315000, 353600)
  )
  expect_error(calculate_methane_volume_acm0008(-1, 2))
  expect_error(calculate_methane_volume_acm0008(1, c(1, 2)))
})

test_that("baseline emissions scale with methane fraction", {
  volume <- calculate_methane_volume_acm0008(480, 720)
  baseline <- calculate_baseline_emissions_acm0008(volume, 0.4)
  expect_gt(baseline, 0)
  expect_error(calculate_baseline_emissions_acm0008(-1, 0.4))
  expect_error(calculate_baseline_emissions_acm0008(volume, 1.5))
})

test_that("project emissions respond to oxidation efficiency", {
  volume <- calculate_methane_volume_acm0008(520, 720)
  high <- calculate_project_emissions_acm0008(volume, 0.38, 0.98)
  low <- calculate_project_emissions_acm0008(volume, 0.38, 0.9)
  expect_lt(high, low)
  expect_error(calculate_project_emissions_acm0008(volume, 0.4, 1.2))
})

test_that("leakage and net emissions behave correctly", {
  leakage <- calculate_leakage_emissions_acm0008(c(5, 6), c(0.7, 0.72))
  expect_equal(leakage, c(3.5, 4.32))
  expect_error(calculate_leakage_emissions_acm0008(-1, 0.7))

  net <- calculate_net_emission_reductions_acm0008(100, 10, 4)
  expect_equal(net, 86)
})
