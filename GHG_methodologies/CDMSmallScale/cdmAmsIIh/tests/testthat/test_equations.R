test_that("baseline emissions aggregate with specific energy diagnostics", {
  baseline <- tibble::tibble(
    facility = c("A", "A", "B"),
    baseline_fuel_use_gj = c(400, 320, 540),
    baseline_emission_factor_tco2_per_gj = c(0.07, 0.071, 0.072),
    baseline_useful_output_gj = c(360, 300, 480)
  )

  result <- calculate_baseline_decentralized_emissions_iih(
    baseline,
    group_cols = "facility"
  )

  expect_equal(
    result$baseline_emissions_tco2e[result$facility == "A"],
    400 * 0.07 + 320 * 0.071,
    tolerance = 1e-10
  )
  expect_equal(
    result$baseline_specific_energy_gj_per_gj[result$facility == "A"],
    (400 + 320) / (360 + 300),
    tolerance = 1e-10
  )
  expect_equal(
    result$baseline_emissions_tco2e[result$facility == "B"],
    540 * 0.072,
    tolerance = 1e-10
  )
})

test_that("project central emissions return specific energy", {
  project <- tibble::tibble(
    facility = c("A", "A", "B"),
    project_fuel_use_gj = c(280, 250, 360),
    project_emission_factor_tco2_per_gj = c(0.068, 0.068, 0.069),
    project_useful_output_gj = c(270, 240, 350)
  )

  result <- calculate_project_central_emissions_iih(
    project,
    group_cols = "facility"
  )

  expect_equal(
    result$project_central_emissions_tco2e[result$facility == "A"],
    280 * 0.068 + 250 * 0.068,
    tolerance = 1e-10
  )
  expect_equal(
    result$project_specific_energy_gj_per_gj[result$facility == "A"],
    (280 + 250) / (270 + 240),
    tolerance = 1e-10
  )
})

test_that("auxiliary electricity handles optional input", {
  monitoring <- tibble::tibble(
    facility = c("A", "B"),
    project_auxiliary_electricity_mwh = c(210, 160),
    project_electricity_emission_factor_tco2_per_mwh = c(0.6, 0.55)
  )

  result <- calculate_project_auxiliary_electricity_iih(monitoring, group_cols = "facility")
  expect_equal(result$project_auxiliary_emissions_tco2e, c(210 * 0.6, 160 * 0.55))

  zeros <- calculate_project_auxiliary_electricity_iih(monitoring, electricity_consumption_col = NULL)
  expect_equal(zeros$project_auxiliary_emissions_tco2e, 0)
})

test_that("emission reductions subtract project and leakage", {
  baseline_emissions <- tibble::tibble(
    facility = c("A", "B"),
    baseline_emissions_tco2e = c(12.1, 9.4)
  )
  project_emissions <- tibble::tibble(
    facility = c("A", "B"),
    project_emissions_tco2e = c(6.8, 5.2)
  )
  leakage <- tibble::tibble(
    facility = c("A", "B"),
    leakage_emissions_tco2e = c(0.3, 0.2)
  )

  reductions <- calculate_emission_reductions_iih(
    baseline_emissions,
    project_emissions,
    leakage,
    group_cols = "facility"
  )

  expect_equal(reductions$emission_reductions_tco2e, c(12.1 - 6.8 - 0.3, 9.4 - 5.2 - 0.2))
})

test_that("meta workflow matches manual combination", {
  baseline <- tibble::tibble(
    facility = c("A", "A", "B"),
    baseline_fuel_use_gj = c(400, 320, 540),
    baseline_emission_factor_tco2_per_gj = c(0.07, 0.071, 0.072),
    baseline_useful_output_gj = c(360, 300, 480)
  )
  project <- tibble::tibble(
    facility = c("A", "A", "B"),
    project_fuel_use_gj = c(280, 250, 360),
    project_emission_factor_tco2_per_gj = c(0.068, 0.068, 0.069),
    project_useful_output_gj = c(270, 240, 350),
    project_auxiliary_electricity_mwh = c(210, 205, 160),
    project_electricity_emission_factor_tco2_per_mwh = c(0.6, 0.6, 0.55)
  )
  leakage <- tibble::tibble(
    facility = c("A", "B"),
    leakage_emissions_tco2e = c(0.3, 0.2)
  )

  reductions <- estimate_emission_reductions_ams_iih(
    baseline_data = baseline,
    project_data = project,
    leakage_data = leakage,
    group_cols = "facility"
  )

  baseline_emissions <- calculate_baseline_decentralized_emissions_iih(
    baseline,
    group_cols = "facility"
  )
  project_central <- calculate_project_central_emissions_iih(
    project,
    group_cols = "facility"
  )
  project_aux <- calculate_project_auxiliary_electricity_iih(
    project,
    group_cols = "facility"
  )
  project_totals <- dplyr::left_join(project_central, project_aux, by = "facility") |>
    dplyr::mutate(project_emissions_tco2e = project_central_emissions_tco2e + project_auxiliary_emissions_tco2e)

  manual <- calculate_emission_reductions_iih(
    baseline_emissions = dplyr::select(baseline_emissions, facility, baseline_emissions_tco2e),
    project_emissions = dplyr::select(project_totals, facility, project_emissions_tco2e),
    leakage_emissions = leakage,
    group_cols = "facility"
  )

  expect_equal(
    dplyr::arrange(reductions, facility)$emission_reductions_tco2e,
    dplyr::arrange(manual, facility)$emission_reductions_tco2e
  )
})
