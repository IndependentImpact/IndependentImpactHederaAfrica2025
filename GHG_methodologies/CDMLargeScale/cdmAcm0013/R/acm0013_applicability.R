#' Check ACM0013 applicability conditions
#'
#' Evaluates whether a monitoring dataset satisfies the core ACM0013
#' applicability conditions: the plant must be a new build, connected to the
#' grid, fuelled by a less carbon-intensive technology than the baseline, and the
#' project must demonstrate that it displaces higher-emitting generation on the
#' grid.
#'
#' @param data A tibble or data frame containing at least the columns
#'   `is_new_plant`, `grid_connected`, `technology_emission_factor_tco2_per_mwh`,
#'   and `baseline_emission_factor_tco2_per_mwh`.
#' @param efficiency_improvement_threshold Minimum percentage improvement in the
#'   emission factor (baseline minus project) required for the methodology to
#'   apply. Defaults to `0.1` (i.e. 10%).
#' @return Logical scalar indicating whether the dataset meets the applicability
#'   criteria.
#' @examples
#' dataset <- simulate_acm0013_dataset(periods = 1, seed = 123)
#' check_applicability_acm0013(dataset)
#' @export
check_applicability_acm0013 <- function(data, efficiency_improvement_threshold = 0.1) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }
  required_cols <- c(
    "is_new_plant", "grid_connected", "technology_emission_factor_tco2_per_mwh",
    "baseline_emission_factor_tco2_per_mwh"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(sprintf(
      "Missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  if (!is.numeric(efficiency_improvement_threshold) || length(efficiency_improvement_threshold) != 1) {
    rlang::abort("`efficiency_improvement_threshold` must be a single numeric value.")
  }
  if (efficiency_improvement_threshold < 0 || efficiency_improvement_threshold >= 1) {
    rlang::abort("`efficiency_improvement_threshold` must be between 0 and 1.")
  }

  if (!is.logical(data$is_new_plant) || !is.logical(data$grid_connected)) {
    rlang::abort("`is_new_plant` and `grid_connected` must be logical vectors.")
  }

  required_flags <- all(data$is_new_plant, na.rm = TRUE) && all(data$grid_connected, na.rm = TRUE)

  emission_factor_check <- mean(data$technology_emission_factor_tco2_per_mwh <
                                  data$baseline_emission_factor_tco2_per_mwh,
                                na.rm = TRUE)

  if (is.nan(emission_factor_check)) {
    emission_factor_check <- 0
  }

  average_improvement <- mean(
    (data$baseline_emission_factor_tco2_per_mwh - data$technology_emission_factor_tco2_per_mwh) /
      data$baseline_emission_factor_tco2_per_mwh,
    na.rm = TRUE
  )

  required_flags && emission_factor_check == 1 && average_improvement >= efficiency_improvement_threshold
}
