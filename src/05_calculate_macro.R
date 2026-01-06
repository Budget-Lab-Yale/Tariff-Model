# =============================================================================
# 05_calculate_macro.R - Calculate macroeconomic effects
# =============================================================================
#
# This module calculates macro effects from MAUS quarterly data.
#
# Key calculations:
#   GDP Q4-Q4: 100 * ((maus_gdp_q4 / maus_gdp_q4_prior) - (base_gdp_q4 / base_gdp_q4_prior))
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
  required_cols <- c('year', 'quarter', 'gdp_baseline', 'gdp_tariff',
                     'employment_baseline', 'employment_tariff',
                     'urate_baseline', 'urate_tariff')
  missing_cols <- setdiff(required_cols, names(maus))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in MAUS data: ', paste(missing_cols, collapse = ', '))
  }

  message(sprintf('  Processing %d quarters of MAUS data', nrow(maus)))

  # -------------------------------------------------------------------------
  # Helper function to get Q4 data
  # -------------------------------------------------------------------------

  get_q4 <- function(data, yr) {
    data %>%
      filter(year == yr, quarter == 4) %>%
      slice(1)
  }

  # -------------------------------------------------------------------------
  # Calculate GDP Q4-Q4 growth differential
  # -------------------------------------------------------------------------
  #
  # Formula: 100 * ((tariff_q4/tariff_q4_prior) - (base_q4/base_q4_prior))
  # This measures the difference in Q4-to-Q4 GDP growth rates

  calc_gdp_q4q4 <- function(yr) {
    q4_current <- get_q4(maus, yr)
    q4_prior <- get_q4(maus, yr - 1)

    if (nrow(q4_current) == 0 || nrow(q4_prior) == 0) {
      return(NA_real_)
    }

    tariff_growth <- q4_current$gdp_tariff / q4_prior$gdp_tariff
    base_growth <- q4_current$gdp_baseline / q4_prior$gdp_baseline

    100 * (tariff_growth - base_growth)
  }

  gdp_2025 <- calc_gdp_q4q4(2025)
  gdp_2026 <- calc_gdp_q4q4(2026)

  # -------------------------------------------------------------------------
  # Calculate unemployment rate changes
  # -------------------------------------------------------------------------
  #
  # Simple difference: tariff_urate - baseline_urate at Q4

  calc_urate_change <- function(yr) {
    q4 <- get_q4(maus, yr)
    if (nrow(q4) == 0) {
      return(NA_real_)
    }
    q4$urate_tariff - q4$urate_baseline
  }

  urate_2025 <- calc_urate_change(2025)
  urate_2026 <- calc_urate_change(2026)

  # -------------------------------------------------------------------------
  # Calculate payroll changes (in thousands)
  # -------------------------------------------------------------------------
  #
  # Employment is in millions, multiply by 1000 to get thousands
  # Negative means job losses

  calc_payroll_change <- function(yr) {
    q4 <- get_q4(maus, yr)
    if (nrow(q4) == 0) {
      return(NA_real_)
    }
    1000 * (q4$employment_tariff - q4$employment_baseline)
  }

  payroll_2025 <- calc_payroll_change(2025)
  payroll_2026 <- calc_payroll_change(2026)

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
