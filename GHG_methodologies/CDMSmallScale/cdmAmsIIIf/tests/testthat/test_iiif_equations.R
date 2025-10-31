library(tibble)

compute_baseline_expected <- function(waste, doc, docf, mcf, oxidation = 0, methane_fraction = 0.5, gwp = 28) {
  sum(waste * doc * docf * mcf * methane_fraction * (16 / 12) * (1 - oxidation) * gwp)
}

test_that("baseline emissions aggregate correctly", {
  data <- tibble(
    site_id = c("A", "A"),
    waste_tonnes = c(1000, 800),
    doc_fraction = c(0.15, 0.16),
    docf_fraction = c(0.5, 0.48),
    baseline_mcf_fraction = 0.7,
    baseline_oxidation_fraction = c(0.05, 0.08)
  )
  result <- calculate_baseline_methane_emissions_iiif(
    data,
    oxidation_factor_col = "baseline_oxidation_fraction",
    group_cols = "site_id"
  )
  expected <- compute_baseline_expected(
    waste = data$waste_tonnes,
    doc = data$doc_fraction,
    docf = data$docf_fraction,
    mcf = data$baseline_mcf_fraction,
    oxidation = data$baseline_oxidation_fraction
  )
  expect_equal(result$baseline_emissions_tco2e, expected)
})

test_that("project emissions include residual methane and energy use", {
  data <- tibble(
    site_id = "A",
    composted_waste_tonnes = 900,
    doc_fraction = 0.15,
    docf_fraction = 0.5,
    compost_mcf_fraction = 0.12,
    compost_oxidation_fraction = 0.1,
    electricity_mwh = 40,
    grid_ef_tco2_per_mwh = 0.6,
    diesel_litres = 2500,
    diesel_ef_tco2_per_litre = 0.0026
  )
  result <- calculate_project_emissions_iiif(
    data,
    compost_oxidation_col = "compost_oxidation_fraction",
    fossil_fuel_consumption_col = "diesel_litres",
    fossil_fuel_emission_factor_col = "diesel_ef_tco2_per_litre"
  )
  methane_component <- compute_baseline_expected(
    waste = data$composted_waste_tonnes,
    doc = data$doc_fraction,
    docf = data$docf_fraction,
    mcf = data$compost_mcf_fraction,
    oxidation = data$compost_oxidation_fraction
  )
  energy_component <- data$electricity_mwh * data$grid_ef_tco2_per_mwh +
    data$diesel_litres * data$diesel_ef_tco2_per_litre
  expect_equal(result$project_emissions_tco2e, methane_component + energy_component)
})

test_that("leakage emissions sum transport, residual, and displacement", {
  data <- tibble(
    compost_transported_tonnes = 500,
    transport_distance_km = 30,
    transport_ef_tco2_per_tkm = 0.0001,
    residual_waste_tonnes = 40,
    residual_waste_ef_tco2_per_tonne = 0.4,
    displaced_fertiliser_tonnes = 20,
    fertiliser_ef_tco2_per_tonne = 0.45
  )
  result <- calculate_leakage_emissions_iiif(data)
  expected <- 500 * 30 * 0.0001 + 40 * 0.4 - 20 * 0.45
  expect_equal(result$leakage_emissions_tco2e, expected)
})

test_that("emission reduction helper merges datasets", {
  baseline <- tibble(site_id = "A", baseline_emissions_tco2e = 25)
  project <- tibble(site_id = "A", project_emissions_tco2e = 6)
  leakage <- tibble(site_id = "A", leakage_emissions_tco2e = 2)
  reductions <- calculate_emission_reductions_iiif(
    baseline,
    project,
    leakage,
    group_cols = "site_id"
  )
  expect_equal(reductions$net_emission_reductions_tco2e, 17)
})

test_that("meta workflow produces expected columns", {
  monitoring <- simulate_ams_iiif_dataset(n_sites = 2, n_periods = 2, seed = 99)
  reductions <- estimate_emission_reductions_ams_iiif(
    baseline_data = monitoring$baseline,
    project_data = monitoring$project,
    leakage_data = monitoring$leakage,
    group_cols = "site_id",
    baseline_args = list(oxidation_factor_col = "baseline_oxidation_fraction"),
    project_args = list(compost_oxidation_col = "compost_oxidation_fraction")
  )
  expect_true(all(c(
    "baseline_emissions_tco2e",
    "project_emissions_tco2e",
    "leakage_emissions_tco2e",
    "net_emission_reductions_tco2e"
  ) %in% colnames(reductions)))
})
