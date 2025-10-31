test_that("baseline emissions multiply electricity and grid factor", {
  electricity <- c(1000, 2000)
  grid_factor <- c(0.8, 0.7)
  result <- calculate_baseline_emissions_acm0018(electricity, grid_factor)
  expect_equal(result, electricity * grid_factor)
})

test_that("project emissions add auxiliary, onsite, and transport contributions", {
  value <- calculate_project_emissions_acm0018(
    auxiliary_fossil_tj = c(0.1, 0.2),
    auxiliary_fossil_ef = 75,
    onsite_generation_mwh = c(10, 0),
    onsite_emission_factor = 0.7,
    biomass_transport_tkm = c(100, 200),
    transport_emission_factor = 0.00012
  )
  expected <- c(0.1 * 75 + 10 * 0.7 + 100 * 0.00012,
                0.2 * 75 + 0 * 0.7 + 200 * 0.00012)
  expect_equal(value, expected)
})

test_that("leakage emissions respect leakage fraction", {
  baseline <- c(100, 200)
  leakage_fraction <- 0.05
  expect_equal(
    calculate_leakage_emissions_acm0018(leakage_fraction, baseline),
    baseline * leakage_fraction
  )
})

test_that("emission reductions subtract project and leakage", {
  baseline <- 1000
  project <- 150
  leakage <- 20
  expect_equal(
    calculate_emission_reductions_acm0018(baseline, project, leakage),
    baseline - project - leakage
  )
})
