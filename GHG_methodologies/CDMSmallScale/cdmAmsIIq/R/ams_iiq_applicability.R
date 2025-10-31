#' Check commercial building service coverage (Applicability Condition 1)
#'
#' AMS-II.Q requires that the project serves the same commercial building loads
#' as the baseline scenario. This helper checks that each service represented in
#' the baseline summary (e.g. chilled water, space conditioning, lighting) also
#' appears in the project summary and that the project does not increase the
#' number of service units beyond the baseline.
#'
#' @param baseline_summary Tibble summarising baseline services with a service
#'   identifier and unit count.
#' @param project_summary Tibble summarising project services with the same
#'   identifiers and unit counts.
#' @param service_col Column name storing the service identifier.
#' @param baseline_units_col Column storing the baseline unit counts.
#' @param project_units_col Column storing the project unit counts.
#' @return `TRUE` when all baseline services are covered and the project does not
#'   increase unit counts; otherwise `FALSE`.
#' @examples
#' baseline <- tibble::tibble(service = c("cooling", "lighting"), baseline_units = c(4, 200))
#' project <- tibble::tibble(service = c("cooling", "lighting"), project_units = c(2, 200))
#' check_applicability_service_scope_iiq(baseline, project)
#' @export
check_applicability_service_scope_iiq <- function(baseline_summary,
                                                  project_summary,
                                                  service_col = "service",
                                                  baseline_units_col = "baseline_units",
                                                  project_units_col = "project_units") {
  baseline_tbl <- tibble::as_tibble(baseline_summary)
  project_tbl <- tibble::as_tibble(project_summary)

  service_sym <- rlang::ensym(service_col)
  baseline_units_sym <- rlang::ensym(baseline_units_col)
  project_units_sym <- rlang::ensym(project_units_col)

  required_baseline <- c(rlang::as_string(service_sym), rlang::as_string(baseline_units_sym))
  required_project <- c(rlang::as_string(service_sym), rlang::as_string(project_units_sym))
  if (!all(required_baseline %in% names(baseline_tbl))) {
    stop("Baseline summary must contain service and baseline unit columns", call. = FALSE)
  }
  if (!all(required_project %in% names(project_tbl))) {
    stop("Project summary must contain service and project unit columns", call. = FALSE)
  }

  join <- baseline_tbl |>
    dplyr::select(!!service_sym, !!baseline_units_sym) |>
    dplyr::left_join(
      project_tbl |>
        dplyr::select(!!service_sym, !!project_units_sym),
      by = rlang::as_string(service_sym)
    )

  all(!is.na(join[[rlang::as_string(project_units_sym)]]) &
    join[[rlang::as_string(project_units_sym)]] <= join[[rlang::as_string(baseline_units_sym)]])
}

#' Check monitoring completeness (Applicability Condition 2)
#'
#' AMS-II.Q mandates that baseline and project monitoring cover the key energy
#' and emission parameters used in the equations. This helper validates that the
#' monitoring dataset contains all required columns and that none of them include
#' missing values.
#'
#' @param monitoring_data Tibble containing the combined monitoring dataset.
#' @param required_cols Character vector listing required column names.
#' @return `TRUE` when all required columns exist and contain no `NA` values;
#'   otherwise `FALSE`.
#' @examples
#' monitoring <- tibble::tibble(
#'   baseline_energy_use_mwh = c(400, 380),
#'   baseline_emission_factor_tco2_per_mwh = c(0.62, 0.62),
#'   project_energy_use_mwh = c(300, 280),
#'   project_emission_factor_tco2_per_mwh = c(0.58, 0.58)
#' )
#' check_applicability_monitoring_iiq(monitoring)
#' @export
check_applicability_monitoring_iiq <- function(monitoring_data,
                                               required_cols = c(
                                                 "baseline_energy_use_mwh",
                                                 "baseline_emission_factor_tco2_per_mwh",
                                                 "project_energy_use_mwh",
                                                 "project_emission_factor_tco2_per_mwh"
                                               )) {
  monitoring_tbl <- tibble::as_tibble(monitoring_data)
  if (!all(required_cols %in% names(monitoring_tbl))) {
    stop("Monitoring dataset is missing required columns", call. = FALSE)
  }

  all(vapply(required_cols, function(col) !any(is.na(monitoring_tbl[[col]])), logical(1)))
}

#' Check energy intensity improvement (Applicability Condition 3)
#'
#' AMS-II.Q projects must demonstrate an efficiency gain relative to the
#' baseline. The helper compares baseline and project energy intensities and
#' confirms that the relative improvement meets the methodology's minimum
#' threshold.
#'
#' @param baseline_data Tibble containing baseline monitoring observations.
#' @param project_data Tibble containing project monitoring observations.
#' @param group_cols Optional character vector of grouping columns shared across
#'   both datasets.
#' @param minimum_improvement Minimum fractional improvement (e.g. `0.05` for a
#'   5% efficiency gain).
#' @param baseline_energy_col Column storing baseline energy use.
#' @param baseline_service_col Column storing baseline service output.
#' @param project_energy_col Column storing project energy use.
#' @param project_service_col Column storing project service output.
#' @return `TRUE` when every group achieves at least the minimum improvement; the
#'   function returns `FALSE` when any group falls short.
#' @examples
#' baseline <- tibble::tibble(
#'   building_id = c("A", "B"),
#'   baseline_energy_use_mwh = c(800, 600),
#'   baseline_service_output_mwh = c(720, 540)
#' )
#' project <- tibble::tibble(
#'   building_id = c("A", "B"),
#'   project_energy_use_mwh = c(520, 420),
#'   project_service_output_mwh = c(720, 540)
#' )
#' check_applicability_efficiency_gain_iiq(
#'   baseline,
#'   project,
#'   group_cols = "building_id",
#'   minimum_improvement = 0.05
#' )
#' @export
check_applicability_efficiency_gain_iiq <- function(baseline_data,
                                                    project_data,
                                                    group_cols = NULL,
                                                    minimum_improvement = 0.05,
                                                    baseline_energy_col = "baseline_energy_use_mwh",
                                                    baseline_service_col = "baseline_service_output_mwh",
                                                    project_energy_col = "project_energy_use_mwh",
                                                    project_service_col = "project_service_output_mwh") {
  if (minimum_improvement < 0 || minimum_improvement > 1) {
    stop("minimum_improvement must be between 0 and 1", call. = FALSE)
  }

  baseline_tbl <- tibble::as_tibble(baseline_data)
  project_tbl <- tibble::as_tibble(project_data)

  energy_baseline_sym <- rlang::ensym(baseline_energy_col)
  service_baseline_sym <- rlang::ensym(baseline_service_col)
  energy_project_sym <- rlang::ensym(project_energy_col)
  service_project_sym <- rlang::ensym(project_service_col)

  join_cols <- if (is.null(group_cols) || length(group_cols) == 0) character() else group_cols

  compute_intensity <- function(tbl, energy_sym, service_sym) {
    tbl |>
      dplyr::summarise(
        energy_total = sum(dplyr::coalesce(!!energy_sym, 0), na.rm = TRUE),
        service_total = sum(dplyr::coalesce(!!service_sym, NA_real_), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        energy_intensity = energy_total / service_total,
        energy_intensity = ifelse(is.finite(energy_intensity), energy_intensity, NA_real_)
      ) |>
      dplyr::select(-energy_total, -service_total)
  }

  if (length(join_cols) == 0) {
    baseline_intensity <- compute_intensity(baseline_tbl, energy_baseline_sym, service_baseline_sym)
    project_intensity <- compute_intensity(project_tbl, energy_project_sym, service_project_sym)
    improvement <- (baseline_intensity$energy_intensity - project_intensity$energy_intensity) /
      baseline_intensity$energy_intensity
    return(all(is.finite(improvement) & improvement >= minimum_improvement))
  }

  baseline_grouped <- baseline_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) |>
    compute_intensity(energy_baseline_sym, service_baseline_sym) |>
    dplyr::rename(baseline_intensity = energy_intensity)

  project_grouped <- project_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) |>
    compute_intensity(energy_project_sym, service_project_sym) |>
    dplyr::rename(project_intensity = energy_intensity)

  comparisons <- baseline_grouped |>
    dplyr::left_join(project_grouped, by = join_cols) |>
    dplyr::mutate(
      improvement = (baseline_intensity - project_intensity) / baseline_intensity
    )

  all(is.finite(comparisons$improvement) & comparisons$improvement >= minimum_improvement)
}
