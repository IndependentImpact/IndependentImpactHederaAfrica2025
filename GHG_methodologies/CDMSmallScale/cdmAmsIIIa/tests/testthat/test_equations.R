test_that("baseline fertilizer emissions aggregate by group", {
  baseline <- tibble::tibble(
    farm_id = c("A", "A", "B"),
    synthetic_n_applied_kg = c(120, 80, 90),
    production_ef_tco2_per_kg = c(0.004, 0.004, 0.004),
    field_ef_tco2_per_kg = c(0.01, 0.01, 0.011)
  )
  totals <- calculate_baseline_fertilizer_emissions_iiia(
    baseline,
    group_cols = "farm_id"
  )
  expect_equal(
    totals$baseline_emissions_tco2e[totals$farm_id == "A"],
    sum((baseline$synthetic_n_applied_kg[1:2]) * 0.014)
  )
  expect_equal(
    totals$baseline_emissions_tco2e[totals$farm_id == "B"],
    baseline$synthetic_n_applied_kg[3] * (0.004 + 0.011)
  )
})

test_that("project fertilizer helper mirrors baseline calculation", {
  project <- tibble::tibble(
    farm_id = c("A", "B"),
    synthetic_n_applied_kg = c(20, 10),
    production_ef_tco2_per_kg = 0.004,
    field_ef_tco2_per_kg = 0.01
  )
  result <- calculate_project_residual_fertilizer_emissions_iiia(project, group_cols = "farm_id")
  expect_equal(result$project_fertilizer_emissions_tco2e, project$synthetic_n_applied_kg * 0.014)
})

test_that("inoculant emissions scale with rate and area", {
  project <- tibble::tibble(
    farm_id = c("A", "B"),
    inoculant_rate_kg_per_ha = c(0.5, 0.4),
    legume_area_ha = c(30, 25),
    inoculant_ef_tco2_per_kg = 0.002
  )
  inoculant <- calculate_project_inoculant_emissions_iiia(project, group_cols = "farm_id")
  expect_equal(
    inoculant$project_inoculant_emissions_tco2e,
    c(0.5 * 30, 0.4 * 25) * 0.002
  )
})

test_that("emission reductions subtract project and leakage", {
  baseline <- tibble::tibble(
    farm_id = c("A", "B"),
    baseline_emissions_tco2e = c(5, 4)
  )
  project <- tibble::tibble(
    farm_id = c("A", "B"),
    project_fertilizer_emissions_tco2e = c(0.5, 0.4),
    project_inoculant_emissions_tco2e = c(0.3, 0.2)
  )
  leakage <- tibble::tibble(
    farm_id = c("A", "B"),
    leakage_emissions_tco2e = c(0.1, 0.05)
  )
  reductions <- calculate_emission_reductions_iiia(
    baseline,
    project,
    leakage,
    group_cols = "farm_id"
  )
  expect_equal(
    reductions$emission_reductions_tco2e,
    baseline$baseline_emissions_tco2e - rowSums(project[,-1]) - leakage$leakage_emissions_tco2e
  )
})

test_that("meta workflow matches manual combination", {
  monitoring <- simulate_ams_iiia_dataset(n_farms = 3, n_periods = 2, seed = 123)

  baseline <- calculate_baseline_fertilizer_emissions_iiia(
    monitoring,
    fertilizer_use_col = "baseline_synthetic_n_kg",
    production_emission_factor_col = "baseline_production_ef_tco2_per_kg",
    field_emission_factor_col = "baseline_field_ef_tco2_per_kg",
    group_cols = "farm_id"
  )

  project_fert <- calculate_project_residual_fertilizer_emissions_iiia(
    monitoring,
    fertilizer_use_col = "project_synthetic_n_kg",
    production_emission_factor_col = "project_production_ef_tco2_per_kg",
    field_emission_factor_col = "project_field_ef_tco2_per_kg",
    group_cols = "farm_id"
  )

  project_inoc <- calculate_project_inoculant_emissions_iiia(
    monitoring,
    inoculant_rate_col = "inoculant_rate_kg_per_ha",
    area_planted_col = "legume_area_ha",
    inoculant_emission_factor_col = "inoculant_ef_tco2_per_kg",
    group_cols = "farm_id"
  )

  leakage <- calculate_leakage_emissions_iiia(
    monitoring,
    group_cols = "farm_id",
    leakage_col = "leakage_emissions_tco2e"
  )

  manual <- calculate_emission_reductions_iiia(
    baseline,
    project_fert |>
      dplyr::left_join(project_inoc, by = "farm_id"),
    leakage,
    group_cols = "farm_id"
  )

  meta <- estimate_emission_reductions_ams_iiia(
    monitoring,
    monitoring,
    leakage_data = monitoring,
    group_cols = "farm_id",
    leakage_col = "leakage_emissions_tco2e"
  )

  expect_equal(
    dplyr::arrange(meta, farm_id)$emission_reductions_tco2e,
    dplyr::arrange(manual, farm_id)$emission_reductions_tco2e
  )
})
