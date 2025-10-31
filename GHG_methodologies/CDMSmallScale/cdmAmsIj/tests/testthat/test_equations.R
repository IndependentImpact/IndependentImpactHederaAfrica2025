test_that("calculate_useful_thermal_output sums useful heat", {
  data <- tibble::tibble(site_id = c("A", "A", "B"), useful_heat_mwh = c(120, 180, 200))
  result <- calculate_useful_thermal_output(data, group_cols = "site_id")
  expect_equal(nrow(result), 2)
  expect_equal(result$useful_thermal_output_mwh[result$site_id == "A"], 300)
  expect_equal(result$useful_thermal_output_mwh[result$site_id == "B"], 200)
})

test_that("baseline and project emissions combine into reductions", {
  data <- tibble::tibble(site_id = c("A", "B"), useful_heat_mwh = c(250, 180))
  baseline <- calculate_useful_thermal_output(data, group_cols = "site_id")
  be <- calculate_baseline_emissions(baseline, baseline_emission_factor = 0.24)
  auxiliary <- tibble::tibble(site_id = c("A", "B"), auxiliary_energy_mwh = c(30, 40))
  pe <- calculate_project_emissions_auxiliary(auxiliary, emission_factor = 0.18, group_cols = "site_id")
  er <- calculate_emission_reductions(be, pe)

  expect_true(all(er$emission_reductions_tco2e <= be$baseline_emissions_tco2e))
  expect_equal(er$emission_reductions_tco2e, be$baseline_emissions_tco2e - pe$project_emissions_tco2e)
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(
    site_id = c("A", "B"),
    useful_heat_mwh = c(250, 180),
    auxiliary_energy_mwh = c(30, 40)
  )
  manual <- calculate_emission_reductions(
    calculate_baseline_emissions(
      calculate_useful_thermal_output(data, group_cols = "site_id"),
      baseline_emission_factor = 0.22
    ),
    calculate_project_emissions_auxiliary(
      data,
      energy_col = "auxiliary_energy_mwh",
      emission_factor = 0.16,
      group_cols = "site_id"
    )
  )
  wrapper <- estimate_emission_reductions_ams_ij(
    data,
    baseline_emission_factor = 0.22,
    auxiliary_emission_factor = 0.16,
    group_cols = "site_id"
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("monitoring aggregation preserves totals", {
  data <- simulate_ams_ij_dataset(n_sites = 2, n_periods = 4)
  aggregated <- aggregate_monitoring_periods(
    data,
    monitoring_cols = c("year", "month"),
    group_cols = "site_id",
    useful_energy_col = "useful_heat_mwh",
    auxiliary_energy_col = "auxiliary_energy_mwh",
    baseline_factor_col = "baseline_emission_factor",
    auxiliary_factor_col = "auxiliary_emission_factor"
  )

  expect_equal(nrow(aggregated), 8)
  expect_true(all(c(
    "site_id", "year", "month", "useful_thermal_output_mwh",
    "baseline_emissions_tco2e", "project_emissions_tco2e",
    "emission_reductions_tco2e", "baseline_emission_factor",
    "auxiliary_emission_factor"
  ) %in% names(aggregated)))
  expect_true(all(aggregated$baseline_emissions_tco2e >= aggregated$emission_reductions_tco2e))
})
