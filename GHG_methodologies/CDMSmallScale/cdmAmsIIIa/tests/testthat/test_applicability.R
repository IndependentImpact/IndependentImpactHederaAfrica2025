test_that("legume share applicability enforces threshold", {
  farms <- tibble::tibble(
    farm_id = c("A", "B"),
    legume_area_ha = c(24, 6),
    total_area_ha = c(30, 40)
  )
  result <- check_applicability_legume_share_iiia(
    farms,
    group_cols = "farm_id"
  )
  expect_equal(result$legume_share_applicable, c(TRUE, FALSE))
})

test_that("inoculant registration requires TRUE for all periods", {
  records <- tibble::tibble(
    farm_id = c("A", "A", "B"),
    monitoring_period = c(1, 2, 1),
    inoculant_registered = c(TRUE, TRUE, FALSE)
  )
  result <- check_applicability_inoculant_registration_iiia(
    records,
    registration_col = "inoculant_registered",
    group_cols = "farm_id"
  )
  expect_equal(result$inoculant_registration_applicable, c(TRUE, FALSE))
})

test_that("monitoring practices require minimum periods", {
  monitoring <- tibble::tibble(
    farm_id = c("A", "A", "A", "B"),
    season = c(1, 2, 3, 1)
  )
  result <- check_applicability_monitoring_practices_iiia(
    monitoring,
    monitoring_unit_cols = "farm_id",
    min_periods = 3
  )
  expect_equal(result$monitoring_practices_applicable, c(TRUE, FALSE))
  expect_error(check_applicability_monitoring_practices_iiia(monitoring, character()),
               "must identify at least one")
})
