test_that("methane volume conversion follows ACM0001 default density", {
  expect_equal(convert_methane_volume_to_mass(1000), 0.716)
})

test_that("baseline, project, leakage, and reductions chain correctly", {
  baseline <- calculate_baseline_emissions_acm0001(6000,
                                                   baseline_capture_efficiency = 0.1,
                                                   oxidation_fraction = 0.05)
  project <- calculate_project_emissions_acm0001(4500, 0.9,
                                                 auxiliary_fuel_tj = 0.02,
                                                 auxiliary_ef_t_per_tj = 74,
                                                 electricity_import_mwh = 5,
                                                 import_ef_t_per_mwh = 0.75)
  leakage <- calculate_leakage_emissions_acm0001(0.05, 4500)
  reductions <- calculate_emission_reductions_acm0001(baseline, project, leakage)

  expect_gt(baseline, project)
  expect_gte(leakage, 0)
  expect_equal(reductions, baseline - project - leakage)
})

test_that("oxidation fraction reduces baseline emissions", {
  no_oxidation <- calculate_baseline_emissions_acm0001(5000, oxidation_fraction = 0)
  with_oxidation <- calculate_baseline_emissions_acm0001(5000, oxidation_fraction = 0.1)

  expect_lt(with_oxidation, no_oxidation)
})

test_that("methane destruction helpers align", {
  destroyed_t <- calculate_methane_destroyed_acm0001(4000, 0.85)
  destroyed_co2e <- calculate_methane_destruction_co2e_acm0001(4000, 0.85)

  expect_gt(destroyed_t, 0)
  expect_equal(destroyed_co2e, destroyed_t * 28)
})

test_that("aggregate_monitoring_periods summarises landfill gas data", {
  data <- tibble::tibble(
    period = rep(c("P1", "P2"), each = 2),
    methane_generation_m3 = c(5000, 5200, 4800, 5100),
    baseline_capture_efficiency = 0.05,
    methane_captured_m3 = c(1200, 1300, 1100, 1250),
    destruction_efficiency = 0.92,
    auxiliary_fuel_tj = c(0.02, 0.021, 0.018, 0.019),
    auxiliary_ef_t_per_tj = 74,
    electricity_import_mwh = c(10, 12, 11, 9),
    import_ef_t_per_mwh = 0.75,
    leakage_fraction = 0.04,
    methane_density_t_per_m3 = 0.000716,
    gwp_ch4 = 28
  )

  summary <- aggregate_monitoring_periods(data)
  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 2)
  expect_true(all(summary$baseline_emissions > summary$project_emissions))
  expect_true(all(summary$emission_reductions > 0))
  expect_true(all(summary$methane_destroyed_t >= 0))
})

test_that("estimate_emission_reductions_acm0001 matches aggregated totals", {
  data <- simulate_acm0001_dataset(4, seed = 123)
  summary <- aggregate_monitoring_periods(data)
  totals <- estimate_emission_reductions_acm0001(data)

  expect_equal(totals$total_baseline_emissions, sum(summary$baseline_emissions))
  expect_equal(totals$total_emission_reductions, sum(summary$emission_reductions))
})
