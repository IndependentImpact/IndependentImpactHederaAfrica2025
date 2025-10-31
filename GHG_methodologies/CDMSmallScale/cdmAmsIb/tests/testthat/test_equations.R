
test_that("calculate_baseline_energy_content aggregates energy", {
  data <- tibble::tibble(
    machine_id = c("A", "A", "B"),
    fuel_consumption = c(100, 150, 200),
    net_calorific_value = c(43, 43, 42)
  )
  result <- calculate_baseline_energy_content(data, group_cols = "machine_id")
  expect_equal(nrow(result), 2)
  expect_equal(result$baseline_energy_mj[result$machine_id == "A"], (100 + 150) * 43)
  expect_equal(result$baseline_energy_mj[result$machine_id == "B"], 200 * 42)
})

test_that("baseline and project emissions produce expected reductions", {
  baseline_energy <- tibble::tibble(machine_id = c("A", "B"), baseline_energy_mj = c(4300, 5200))
  baseline_emissions <- calculate_baseline_emissions(baseline_energy, emission_factor = 0.00007)
  project_energy <- tibble::tibble(machine_id = c("A", "B"), project_energy_mj = c(100, 120))
  project_emissions <- calculate_project_emissions(project_energy, project_emission_factor = 0.00007)
  reductions <- calculate_emission_reductions(baseline_emissions, project_emissions)

  expect_equal(reductions$emission_reductions_tco2e,
               baseline_emissions$baseline_emissions_tco2e - project_emissions$project_emissions_tco2e)
})

test_that("meta-function matches manual workflow", {
  data <- tibble::tibble(
    machine_id = c("A", "A", "B"),
    fuel_consumption = c(100, 120, 150),
    net_calorific_value = c(43, 43, 42),
    backup_energy_mj = c(5, 5, 10)
  )

  manual <- calculate_emission_reductions(
    calculate_baseline_emissions(
      calculate_baseline_energy_content(data, group_cols = "machine_id"),
      emission_factor = 0.00007
    ),
    calculate_project_emissions(
      tibble::tibble(
        machine_id = c("A", "B"),
        project_energy_mj = c(sum(data$backup_energy_mj[data$machine_id == "A"]),
                               sum(data$backup_energy_mj[data$machine_id == "B"]))
      ),
      project_emission_factor = 0.00007
    )
  )

  wrapper <- estimate_emission_reductions_ams_ib(
    fuel_data = data,
    emission_factor = 0.00007,
    project_emission_factor = 0.00007,
    project_energy_col = "backup_energy_mj",
    group_cols = "machine_id"
  )

  expect_equal(
    sort(names(wrapper)),
    sort(c(
      "machine_id",
      "baseline_energy_mj",
      "baseline_emissions_tco2e",
      "project_energy_mj",
      "project_emissions_tco2e",
      "emission_reductions_tco2e"
    ))
  )
  expect_equal(wrapper$emission_reductions_tco2e, manual$emission_reductions_tco2e)
})

test_that("meta-function handles ungrouped inputs without duplicate columns", {
  data <- tibble::tibble(
    fuel_consumption = c(100, 120, 150),
    net_calorific_value = c(43, 43, 42)
  )

  wrapper <- estimate_emission_reductions_ams_ib(
    fuel_data = data,
    emission_factor = 0.00007
  )

  expect_false(any(grepl("\\.x$|\\.y$", names(wrapper))))
  expect_setequal(
    names(wrapper),
    c(
      "baseline_energy_mj",
      "baseline_emissions_tco2e",
      "project_energy_mj",
      "project_emissions_tco2e",
      "emission_reductions_tco2e"
    )
  )
})
