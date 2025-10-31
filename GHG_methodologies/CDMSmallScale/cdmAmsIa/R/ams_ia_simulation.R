
#' Simulate a dataset compliant with AMS-I.A
#'
#' Generates a tidy dataset representing user-level electricity generation. The
#' simulation reflects renewable generation that displaces fossil baseline
#' electricity use and produces monitoring period metadata suitable for
#' downstream aggregation helpers.
#'
#' @param n_users Number of end users to simulate.
#' @param n_periods Number of monitoring periods (default monthly observations across a year).
#' @param start_year Calendar year assigned to the first monitoring period.
#' @param start_month Calendar month (1-12) assigned to the first monitoring period.
#' @param mean_generation_kwh Mean annual electricity generation per user in kWh.
#' @param sd_generation_kwh Standard deviation of annual electricity generation.
#' @param grid_emission_factor Grid emission factor in tCO2e/kWh used for simulated emissions.
#' @return A tibble containing simulated user identifiers, monitoring period metadata, generation, and emissions.
#' @examples
#' simulate_ams_ia_dataset(n_users = 5)
#' @importFrom stats rnorm
#' @importFrom tibble as_tibble
#' @export

simulate_ams_ia_dataset <- function(n_users = 20,
                                    n_periods = 12,
                                    start_year = 2023,
                                    start_month = 1,
                                    mean_generation_kwh = 15000,
                                    sd_generation_kwh = 2000,
                                    grid_emission_factor = 0.75) {
  if (!is.numeric(n_users) || n_users <= 0) {
    stop("`n_users` must be a positive numeric value.", call. = FALSE)
  }

  if (!is.numeric(n_periods) || n_periods <= 0) {
    stop("`n_periods` must be a positive numeric value.", call. = FALSE)
  }

  if (!start_month %in% 1:12) {
    stop("`start_month` must be between 1 and 12.", call. = FALSE)
  }

  user_ids <- paste0("user_", seq_len(n_users))
  period_index <- seq_len(n_periods)

  grid <- expand.grid(
    user_id = user_ids,
    monitoring_period = period_index,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  grid <- grid[order(grid$user_id, grid$monitoring_period), , drop = FALSE]

  month_offset <- grid$monitoring_period - 1L
  grid$year <- start_year + ((start_month - 1L + month_offset) %/% 12L)
  grid$month <- ((start_month - 1L + month_offset) %% 12L) + 1L
  grid$day <- sample.int(28L, size = nrow(grid), replace = TRUE)
  grid$monitoring_date <- as.Date(sprintf("%04d-%02d-%02d", grid$year, grid$month, grid$day))
  grid$monitoring_label <- sprintf("%04d-%02d", grid$year, grid$month)

  generation_draws <- stats::rnorm(
    n = nrow(grid),
    mean = mean_generation_kwh / n_periods,
    sd = sd_generation_kwh / sqrt(n_periods)
  )

  grid$generation_kwh <- pmax(generation_draws, 0)
  grid$grid_emission_factor <- grid_emission_factor
  grid$baseline_generation_kwh <- grid$generation_kwh
  grid$baseline_emissions_tco2e <- grid$baseline_generation_kwh * grid$grid_emission_factor
  grid$project_emissions_tco2e <- 0
  grid$emission_reductions_tco2e <- grid$baseline_emissions_tco2e - grid$project_emissions_tco2e

  tibble::as_tibble(grid)
}
