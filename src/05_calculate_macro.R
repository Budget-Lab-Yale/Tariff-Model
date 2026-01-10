# =============================================================================
# 05_calculate_macro.R - Calculate macroeconomic effects
# =============================================================================
#
# This module calculates macro effects from MAUS quarterly data.
#
# Key calculations:
#   GDP Q4-Q4: 100 * ((tariff_gdp_q4 / tariff_gdp_q4_prior) /
#                     (baseline_gdp_q4 / baseline_gdp_q4_prior) - 1)
#   U rate change: maus_urate - baseline_urate (at Q4)
#   Payroll change: 1000 * (maus_employment - baseline_employment) (at Q4, in thousands)
#
# Outputs:
#   - GDP growth differential (2025, 2026)
#   - Unemployment rate change (2025 Q4, 2026 Q4)
#   - Payroll change in thousands (2025 Q4, 2026 Q4)
#
# =============================================================================

library(tidyverse)

#' Calculate macro effects from MAUS data
#'
#' @param inputs List containing:
#'   - maus: Quarterly data with baseline and tariff scenario values
#'
#' @return List containing macro effect estimates
calculate_macro <- function(inputs) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  maus <- inputs$maus

  if (is.null(maus)) {
    stop('MAUS quarterly data not found in inputs$maus')
  }

  # Validate required columns
  assert_has_columns(maus, c('year', 'quarter', 'gdp_baseline', 'gdp_tariff',
                             'employment_baseline', 'employment_tariff',
                             'urate_baseline', 'urate_tariff'), 'MAUS data')

  message(sprintf('  Processing %d quarters of MAUS data', nrow(maus)))

  # -------------------------------------------------------------------------
  # Extract Q4 data for key years and compute all metrics
  # -------------------------------------------------------------------------

  q4_data <- maus %>%
    filter(quarter == 4, year %in% c(2024, 2025, 2026)) %>%
    arrange(year)

  # GDP Q4-Q4 growth differential:
  # Formula: 100 * ((tariff_q4/tariff_q4_prior) / (base_q4/base_q4_prior) - 1)
  calc_gdp_q4q4 <- function(yr) {
    current <- q4_data %>% filter(year == yr)
    prior <- q4_data %>% filter(year == yr - 1)
    if (nrow(current) == 0 || nrow(prior) == 0) return(NA_real_)
    tariff_growth <- current$gdp_tariff / prior$gdp_tariff
    base_growth <- current$gdp_baseline / prior$gdp_baseline
    100 * (tariff_growth / base_growth - 1)
  }

  gdp_2025 <- calc_gdp_q4q4(2025)
  gdp_2026 <- calc_gdp_q4q4(2026)

  # Unemployment and payroll changes (simple differences at Q4)
  q4_2025 <- q4_data %>% filter(year == 2025)
  q4_2026 <- q4_data %>% filter(year == 2026)

  urate_2025 <- if (nrow(q4_2025) > 0) q4_2025$urate_tariff - q4_2025$urate_baseline else NA_real_
  urate_2026 <- if (nrow(q4_2026) > 0) q4_2026$urate_tariff - q4_2026$urate_baseline else NA_real_

  # Employment in millions -> thousands
  payroll_2025 <- if (nrow(q4_2025) > 0) 1000 * (q4_2025$employment_tariff - q4_2025$employment_baseline) else NA_real_
  payroll_2026 <- if (nrow(q4_2026) > 0) 1000 * (q4_2026$employment_tariff - q4_2026$employment_baseline) else NA_real_

  # -------------------------------------------------------------------------
  # Build results
  # -------------------------------------------------------------------------

  results <- list(
    # GDP effects (percentage points)
    gdp_2025 = gdp_2025,
    gdp_2026 = gdp_2026,

    # Unemployment rate changes (percentage points)
    urate_2025 = urate_2025,
    urate_2026 = urate_2026,

    # Payroll changes (thousands of jobs)
    payroll_2025 = payroll_2025,
    payroll_2026 = payroll_2026,

    # Raw quarterly data for reference
    quarterly = maus
  )

  # Log results
  message(sprintf('  GDP 2025 Q4-Q4: %.2f%%', gdp_2025))
  message(sprintf('  GDP 2026 Q4-Q4: %.2f%%', gdp_2026))
  message(sprintf('  U-rate 2025 Q4: %+.2f pp', urate_2025))
  message(sprintf('  U-rate 2026 Q4: %+.2f pp', urate_2026))
  message(sprintf('  Payroll 2025 Q4: %.0f thousand', payroll_2025))
  message(sprintf('  Payroll 2026 Q4: %.0f thousand', payroll_2026))

  return(results)
}


#' Print macro summary table
#'
#' @param macro_results Results from calculate_macro()
print_macro_summary <- function(macro_results) {

  cat('\n----------------------------------------------------------\n')
  cat('MACROECONOMIC EFFECTS\n')
  cat('----------------------------------------------------------\n')

  cat(sprintf('%-25s %15s %15s\n', 'Metric', '2025', '2026'))
  cat('----------------------------------------------------------\n')

  cat(sprintf('%-25s %14.2f%% %14.2f%%\n', 'GDP (Q4-Q4)',
              macro_results$gdp_2025, macro_results$gdp_2026))
  cat(sprintf('%-25s %+14.2f pp %+13.2f pp\n', 'Unemployment Rate (Q4)',
              macro_results$urate_2025, macro_results$urate_2026))
  cat(sprintf('%-25s %14.0f %15.0f\n', 'Payroll (thousands)',
              macro_results$payroll_2025, macro_results$payroll_2026))

  cat('----------------------------------------------------------\n')
}
