test_that("baseline emissions aggregate with intensity diagnostics", {
  baseline <- tibble::tibble(
    building_id = c("A", "A", "B"),
    baseline_energy_use_mwh = c(400, 360, 540),
    baseline_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.61),
    baseline_service_output_mwh = c(380, 340, 520)
  )

  result <- calculate_baseline_building_emissions_iiq(
    baseline,
    group_cols = "building_id"
  )

  expect_equal(
    result$baseline_emissions_tco2e[result$building_id == "A"],
    400 * 0.62 + 360 * 0.62,
    tolerance = 1e-10
  )
  expect_equal(
    result$baseline_energy_intensity[result$building_id == "A"],
    (400 + 360) / (380 + 340),
    tolerance = 1e-10
  )
  expect_equal(
    result$baseline_emissions_tco2e[result$building_id == "B"],
    540 * 0.61,
    tolerance = 1e-10
  )
})

test_that("project emissions return intensity", {
  project <- tibble::tibble(
    building_id = c("A", "A", "B"),
    project_energy_use_mwh = c(280, 260, 360),
    project_emission_factor_tco2_per_mwh = c(0.58, 0.58, 0.57),
    project_service_output_mwh = c(380, 340, 520)
  )

  result <- calculate_project_building_emissions_iiq(
    project,
    group_cols = "building_id"
  )

  expect_equal(
    result$project_emissions_tco2e[result$building_id == "A"],
    280 * 0.58 + 260 * 0.58,
    tolerance = 1e-10
  )
  expect_equal(
    result$project_energy_intensity[result$building_id == "A"],
    (280 + 260) / (380 + 340),
    tolerance = 1e-10
  )
})

test_that("onsite emissions handle optional input", {
  monitoring <- tibble::tibble(
    building_id = c("A", "B"),
    project_onsite_energy_gj = c(210, 0),
    project_onsite_emission_factor_tco2_per_gj = c(0.05, 0.055)
  )

  result <- calculate_project_onsite_energy_emissions_iiq(monitoring, group_cols = "building_id")
  expect_equal(
    result$project_onsite_emissions_tco2e,
    c(210 * 0.05, 0 * 0.055),
    tolerance = 1e-10
  )

  zeros <- calculate_project_onsite_energy_emissions_iiq(monitoring, onsite_energy_col = NULL)
  expect_equal(zeros$project_onsite_emissions_tco2e, 0)
})

test_that("emission reductions subtract project and leakage", {
  baseline_emissions <- tibble::tibble(
    building_id = c("A", "B"),
    baseline_emissions_tco2e = c(12.1, 9.4)
  )
  project_emissions <- tibble::tibble(
    building_id = c("A", "B"),
    project_emissions_tco2e = c(6.8, 5.2)
  )
  leakage <- tibble::tibble(
    building_id = c("A", "B"),
    leakage_emissions_tco2e = c(0.3, 0.2)
  )

  reductions <- calculate_emission_reductions_iiq(
    baseline_emissions,
    project_emissions,
    leakage,
    group_cols = "building_id"
  )

  expect_equal(reductions$emission_reductions_tco2e, c(12.1 - 6.8 - 0.3, 9.4 - 5.2 - 0.2))
})

test_that("meta workflow matches manual combination", {
  baseline <- tibble::tibble(
    building_id = c("A", "A", "B"),
    baseline_energy_use_mwh = c(400, 360, 540),
    baseline_emission_factor_tco2_per_mwh = c(0.62, 0.62, 0.61),
    baseline_service_output_mwh = c(380, 340, 520)
  )
  project <- tibble::tibble(
    building_id = c("A", "A", "B"),
    project_energy_use_mwh = c(280, 260, 360),
    project_emission_factor_tco2_per_mwh = c(0.58, 0.58, 0.57),
    project_service_output_mwh = c(380, 340, 520),
    project_onsite_energy_gj = c(40, 30, 60),
    project_onsite_emission_factor_tco2_per_gj = c(0.05, 0.05, 0.055)
  )
  leakage <- tibble::tibble(
    building_id = c("A", "B"),
    leakage_emissions_tco2e = c(0.3, 0.2)
  )

  reductions <- estimate_emission_reductions_ams_iiq(
    baseline_data = baseline,
    project_data = project,
    leakage_data = leakage,
    group_cols = "building_id"
  )

  baseline_emissions <- calculate_baseline_building_emissions_iiq(
    baseline,
    group_cols = "building_id"
  )
  project_grid <- calculate_project_building_emissions_iiq(
    project,
    group_cols = "building_id",
    output_col = "project_grid_emissions_tco2e"
  )
  project_onsite <- calculate_project_onsite_energy_emissions_iiq(
    project,
    group_cols = "building_id"
  )
  project_totals <- dplyr::left_join(project_grid, project_onsite, by = "building_id") |>
    dplyr::mutate(project_emissions_tco2e = project_grid_emissions_tco2e + project_onsite_emissions_tco2e)

  manual <- calculate_emission_reductions_iiq(
    baseline_emissions = dplyr::select(baseline_emissions, building_id, baseline_emissions_tco2e),
    project_emissions = dplyr::select(project_totals, building_id, project_emissions_tco2e),
    leakage_emissions = leakage,
    group_cols = "building_id"
  )

  expect_equal(
    dplyr::arrange(reductions, building_id)$emission_reductions_tco2e,
    dplyr::arrange(manual, building_id)$emission_reductions_tco2e
  )
})
