test_that("baseline non-renewable biomass aggregates correctly", {
  baseline <- tibble::tibble(
    site_id = c("A", "A", "B"),
    baseline_biomass_consumption_tonnes = c(10, 6, 12),
    baseline_non_renewable_fraction = c(0.8, 0.75, 0.9)
  )
  result <- calculate_baseline_non_renewable_biomass_iig(
    baseline,
    group_cols = "site_id"
  )
  expect_equal(result$baseline_non_renewable_biomass_tonnes[result$site_id == "A"], 10 * 0.8 + 6 * 0.75)
  expect_equal(result$baseline_non_renewable_biomass_tonnes[result$site_id == "B"], 12 * 0.9)
})

test_that("thermal energy conversion applies NCV by group", {
  non_renewable <- tibble::tibble(
    site_id = c("A", "B"),
    baseline_non_renewable_biomass_tonnes = c(12, 15)
  )
  baseline_inputs <- tibble::tibble(
    site_id = c("A", "B"),
    baseline_net_calorific_value_mj_per_tonne = c(15, 14)
  )
  energy <- calculate_baseline_thermal_energy_iig(
    non_renewable,
    baseline_data = baseline_inputs,
    group_cols = "site_id"
  )
  expect_equal(energy$baseline_thermal_energy_mj, c(180, 210))
})

test_that("emissions from energy use emission factors", {
  energy <- tibble::tibble(
    site_id = c("A", "B"),
    baseline_thermal_energy_mj = c(180, 210)
  )
  factors <- tibble::tibble(
    site_id = c("A", "B"),
    baseline_emission_factor_tco2_per_mj = c(0.00009, 0.000095)
  )
  emissions <- calculate_emissions_from_energy_iig(
    energy_data = energy,
    factor_data = factors,
    group_cols = "site_id"
  )
  expect_equal(emissions$baseline_emissions_tco2e, c(0.0162, 0.01995))
})

test_that("emission reductions subtract project and leakage", {
  baseline_emissions <- tibble::tibble(
    site_id = c("A", "B"),
    baseline_emissions_tco2e = c(1.2, 0.9)
  )
  project_emissions <- tibble::tibble(
    site_id = c("A", "B"),
    project_emissions_tco2e = c(0.4, 0.3)
  )
  leakage <- tibble::tibble(
    site_id = c("A", "B"),
    leakage_emissions_tco2e = c(0.05, 0.02)
  )
  reductions <- calculate_emission_reductions_iig(
    baseline_emissions,
    project_emissions,
    leakage,
    group_cols = "site_id"
  )
  expect_equal(reductions$emission_reductions_tco2e, c(0.75, 0.58))
})

test_that("meta workflow returns consistent totals", {
  baseline <- tibble::tibble(
    site_id = c("A", "A", "B"),
    baseline_biomass_consumption_tonnes = c(10, 6, 12),
    baseline_non_renewable_fraction = c(0.8, 0.75, 0.9),
    baseline_net_calorific_value_mj_per_tonne = c(15, 15, 14.5),
    baseline_emission_factor_tco2_per_mj = 0.00009
  )
  project <- tibble::tibble(
    site_id = c("A", "A", "B"),
    project_biomass_consumption_tonnes = c(6, 4, 7),
    project_non_renewable_fraction = c(0.4, 0.38, 0.35),
    project_net_calorific_value_mj_per_tonne = c(15.2, 15.2, 14.7),
    project_emission_factor_tco2_per_mj = 0.00009
  )
  leakage <- tibble::tibble(
    site_id = c("A", "B"),
    leakage_emissions_tco2e = c(0.1, 0.05)
  )

  reductions <- estimate_emission_reductions_ams_iig(
    baseline_data = baseline,
    project_data = project,
    leakage_data = leakage,
    group_cols = "site_id"
  )

  baseline_nrb <- calculate_baseline_non_renewable_biomass_iig(baseline, group_cols = "site_id")
  project_nrb <- calculate_project_non_renewable_biomass_iig(project, group_cols = "site_id")
  baseline_energy <- calculate_baseline_thermal_energy_iig(baseline_nrb, baseline_data = baseline, group_cols = "site_id")
  project_energy <- calculate_project_thermal_energy_iig(project_nrb, project_data = project, group_cols = "site_id")
  baseline_emissions <- calculate_emissions_from_energy_iig(
    energy_data = baseline_energy,
    factor_data = baseline,
    group_cols = "site_id"
  )
  project_emissions <- calculate_emissions_from_energy_iig(
    energy_data = project_energy,
    energy_col = "project_thermal_energy_mj",
    factor_data = project,
    emission_factor_col = "project_emission_factor_tco2_per_mj",
    group_cols = "site_id",
    output_col = "project_emissions_tco2e"
  )
  leakage_totals <- calculate_leakage_emissions_iig(leakage, group_cols = "site_id")
  manual <- calculate_emission_reductions_iig(
    baseline_emissions = baseline_emissions,
    project_emissions = project_emissions,
    leakage_emissions = leakage_totals,
    group_cols = "site_id"
  )

  expect_true(all(c(
    "baseline_non_renewable_biomass_tonnes",
    "project_non_renewable_biomass_tonnes",
    "baseline_emissions_tco2e",
    "project_emissions_tco2e",
    "leakage_emissions_tco2e",
    "emission_reductions_tco2e"
  ) %in% names(reductions)))
  expect_equal(nrow(reductions), 2)
  expect_equal(
    dplyr::arrange(reductions, site_id)$emission_reductions_tco2e,
    dplyr::arrange(manual, site_id)$emission_reductions_tco2e
  )
})
