#' Simulate monitoring data for ACM0019 nitric acid projects
#'
#' Generates tidy monitoring datasets consistent with ACM0019 inputs. The
#' simulation produces production, emission factors, energy use, and auxiliary
#' monitoring information for a configurable number of periods.
#'
#' @param periods Integer number of monitoring periods to simulate.
#' @param observations_per_period Integer number of observations per period.
#' @param seed Optional seed for reproducibility.
#'
#' @return Tibble with period-level observations including nitric acid
#'   production, emission factors, auxiliary energy use, and monitoring metadata.
#' @examples
#' simulate_acm0019_dataset(periods = 3, observations_per_period = 5, seed = 42)
#' @export
simulate_acm0019_dataset <- function(periods = 6,
                                     observations_per_period = 30,
                                     seed = NULL) {
  if (!is.numeric(periods) || length(periods) != 1 || periods <= 0) {
    rlang::abort("`periods` must be a positive numeric scalar.")
  }
  if (!is.numeric(observations_per_period) || length(observations_per_period) != 1 ||
      observations_per_period <= 0) {
    rlang::abort("`observations_per_period` must be a positive numeric scalar.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  total_rows <- periods * observations_per_period
  period_labels <- paste0("P", seq_len(periods))

  tibble::tibble(
    period = factor(rep(period_labels, each = observations_per_period), levels = period_labels),
    production_tonnes = stats::rnorm(total_rows, mean = 1500, sd = 120),
    baseline_ef_kg_per_tonne = stats::rnorm(total_rows, mean = 9.5, sd = 0.4),
    project_ef_kg_per_tonne = stats::rnorm(total_rows, mean = 1.2, sd = 0.2),
    electricity_mwh = stats::rgamma(total_rows, shape = 10, rate = 0.08),
    grid_ef_t_per_mwh = stats::rnorm(total_rows, mean = 0.65, sd = 0.05),
    steam_tonnes = stats::rgamma(total_rows, shape = 8, rate = 0.1),
    steam_ef_t_per_tonne = stats::rnorm(total_rows, mean = 0.08, sd = 0.01),
    data_capture_rate = stats::runif(total_rows, min = 0.88, max = 0.99),
    catalyst_configuration = sample(c("Secondary", "Tertiary"), total_rows, replace = TRUE)
  ) |>
    dplyr::mutate(
      production_tonnes = pmax(production_tonnes, 0),
      baseline_ef_kg_per_tonne = pmax(baseline_ef_kg_per_tonne, 0),
      project_ef_kg_per_tonne = pmax(project_ef_kg_per_tonne, 0),
      electricity_mwh = pmax(electricity_mwh, 0),
      grid_ef_t_per_mwh = pmax(grid_ef_t_per_mwh, 0),
      steam_tonnes = pmax(steam_tonnes, 0),
      steam_ef_t_per_tonne = pmax(steam_ef_t_per_tonne, 0)
    )
}
