library(tibble)

test_that("baseline emissions aggregate COD-derived methane", {
  data <- tibble(
    site_id = c("WW1", "WW1", "WW2"),
    influent_flow_m3_per_day = c(5000, 5200, 3100),
    cod_mg_l = c(3000, 3200, 2500),
    biodegradable_fraction = c(0.8, 0.8, 0.7),
    baseline_capture_efficiency_fraction = c(0.05, 0.05, 0.1),
    baseline_oxidation_fraction = c(0.05, 0.05, 0.04),
    days_in_period = 30
  )
  result <- calculate_baseline_methane_emissions_iiih(
    data,
    group_cols = "site_id",
    days_col = "days_in_period"
  )

  expected <- data |>
    dplyr::mutate(
      cod_load = influent_flow_m3_per_day * cod_mg_l / 1000 * biodegradable_fraction * days_in_period,
      generated = cod_load * 0.35,
      captured = generated * baseline_capture_efficiency_fraction,
      oxidised = (generated - captured) * baseline_oxidation_fraction,
      emitted = pmax(generated - captured - oxidised, 0),
      component = emitted * 0.00067 * 28
    ) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(baseline_emissions_tco2e = sum(component), .groups = "drop")

  expect_equal(result$baseline_emissions_tco2e, expected$baseline_emissions_tco2e)
})

test_that("project emissions capture residual methane and energy inputs", {
  data <- tibble(
    site_id = c("WW1", "WW1", "WW2"),
    influent_flow_m3_per_day = c(5000, 5200, 3100),
    cod_mg_l = c(3000, 3200, 2500),
    biodegradable_fraction = c(0.8, 0.8, 0.7),
    project_capture_efficiency_fraction = c(0.7, 0.72, 0.65),
    destruction_efficiency_fraction = c(0.98, 0.975, 0.97),
    project_oxidation_fraction = c(0.08, 0.06, 0.07),
    fugitive_leakage_fraction = c(0.01, 0.015, 0.012),
    days_in_period = 30,
    electricity_consumption_mwh = c(28, 32, 18),
    electricity_ef_tco2_per_mwh = c(0.48, 0.5, 0.46),
    thermal_energy_consumption_gj = c(10, 12, 6),
    thermal_energy_ef_tco2_per_gj = c(0.06, 0.058, 0.055)
  )
  result <- calculate_project_emissions_iiih(
    data,
    group_cols = "site_id",
    days_col = "days_in_period"
  )

  expected <- data |>
    dplyr::mutate(
      cod_load = influent_flow_m3_per_day * cod_mg_l / 1000 * biodegradable_fraction * days_in_period,
      generated = cod_load * 0.35,
      captured = generated * project_capture_efficiency_fraction,
      destroyed = captured * destruction_efficiency_fraction,
      oxidised = (generated - captured) * project_oxidation_fraction,
      fugitive = captured * fugitive_leakage_fraction,
      residual = pmax(generated - destroyed - oxidised + fugitive, 0),
      methane_component = residual * 0.00067 * 28,
      electricity_component = electricity_consumption_mwh * electricity_ef_tco2_per_mwh,
      thermal_component = thermal_energy_consumption_gj * thermal_energy_ef_tco2_per_gj,
      component = methane_component + electricity_component + thermal_component
    ) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(project_emissions_tco2e = sum(component), .groups = "drop")

  expect_equal(result$project_emissions_tco2e, expected$project_emissions_tco2e)
})

test_that("leakage emissions aggregate transport, treatment, and displacement", {
  data <- tibble(
    site_id = c("WW1", "WW1", "WW2"),
    sludge_tonnes = c(420, 380, 300),
    transport_distance_km = c(20, 24, 35),
    transport_ef_tco2_per_tkm = c(0.0001, 0.00012, 0.00011),
    sludge_treatment_ef_tco2_per_tonne = c(0.3, 0.32, 0.28),
    chemical_usage_tonnes = c(8, 6, 5),
    chemical_ef_tco2_per_tonne = c(0.7, 0.65, 0.6),
    displaced_fossil_fuel_gj = c(120, 90, 70),
    displaced_fossil_fuel_ef_tco2_per_gj = c(0.06, 0.055, 0.05)
  )
  result <- calculate_leakage_emissions_iiih(data, group_cols = "site_id")

  expected <- data |>
    dplyr::mutate(
      transport = sludge_tonnes * transport_distance_km * transport_ef_tco2_per_tkm,
      treatment = sludge_tonnes * sludge_treatment_ef_tco2_per_tonne,
      chemical = chemical_usage_tonnes * chemical_ef_tco2_per_tonne,
      displacement = displaced_fossil_fuel_gj * displaced_fossil_fuel_ef_tco2_per_gj,
      component = transport + treatment + chemical - displacement
    ) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(leakage_emissions_tco2e = sum(component), .groups = "drop")

  expect_equal(result$leakage_emissions_tco2e, expected$leakage_emissions_tco2e)
})

test_that("emission reduction helper merges grouped outputs", {
  baseline <- tibble(site_id = "WW1", baseline_emissions_tco2e = 100)
  project <- tibble(site_id = "WW1", project_emissions_tco2e = 40)
  leakage <- tibble(site_id = "WW1", leakage_emissions_tco2e = 5)
  reductions <- calculate_emission_reductions_iiih(
    baseline,
    project,
    leakage,
    group_cols = "site_id"
  )
  expect_equal(reductions$net_emission_reductions_tco2e, 55)
})

test_that("meta workflow returns expected structure", {
  monitoring <- simulate_ams_iiih_dataset(n_sites = 2, n_periods = 2, seed = 123)
  reductions <- estimate_emission_reductions_ams_iiih(
    baseline_data = monitoring$baseline,
    project_data = monitoring$project,
    leakage_data = monitoring$leakage,
    group_cols = "site_id",
    baseline_args = list(days_col = "days_in_period"),
    project_args = list(days_col = "days_in_period")
  )
  expect_true(all(c(
    "baseline_emissions_tco2e",
    "project_emissions_tco2e",
    "leakage_emissions_tco2e",
    "net_emission_reductions_tco2e"
  ) %in% colnames(reductions)))
})
