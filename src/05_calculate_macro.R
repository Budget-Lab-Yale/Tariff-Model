# =============================================================================
# 05_calculate_macro.R - Calculate macroeconomic effects
# =============================================================================
#
# This module calculates macro effects from USMM quarterly data.
#
# Key calculations:
#   GDP Q4-Q4: 100 * ((tariff_gdp_q4 / tariff_gdp_q4_prior) /
#                     (baseline_gdp_q4 / baseline_gdp_q4_prior) - 1)
#   U rate change: urate_tariff - urate_baseline (at Q4)
#   Nonfarm payroll change: 1000 * (employment_tariff - employment_baseline) (at Q4, in thousands)
#
# Outputs:
#   - GDP growth differential (2025, 2026)
#   - Unemployment rate change (2025 Q4, 2026 Q4)
#   - Payroll change in thousands (2025 Q4, 2026 Q4)
#   - PCE price level effect
#   - Fed funds rate effect
#
# =============================================================================

library(tidyverse)

#' Calculate macro effects from USMM data
#'
#' @param inputs List containing:
#'   - macro_quarterly: Quarterly data with baseline and tariff scenario values
#'
#' @return List containing macro effect estimates
calculate_macro <- function(inputs) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  macro <- inputs$macro_quarterly

  if (is.null(macro)) {
    stop('USMM quarterly data not found in inputs$macro_quarterly')
  }

  # Validate required columns
  assert_has_columns(macro, c('year', 'quarter', 'gdp_baseline', 'gdp_tariff',
                              'employment_baseline', 'employment_tariff',
                              'urate_baseline', 'urate_tariff'), 'USMM data')

  message(sprintf('  Processing %d quarters of USMM data', nrow(macro)))

  # -------------------------------------------------------------------------
  # Extract Q4 data for key years and compute all metrics
  # -------------------------------------------------------------------------

  q4_data <- macro %>%
    filter(quarter == 4, year %in% c(2024, 2025, 2026)) %>%
    arrange(year)

  # -------------------------------------------------------------------------
  # GDP Q4-Q4 growth differential using blended USMM→GTAP path
  # -------------------------------------------------------------------------
  # The blended path linearly transitions from USMM (weight=1 at 2025Q1)
  # to GTAP long-run (weight=0 at 2029Q1) over 16 quarters.
  # blended_gdp_tariff = gdp_baseline * (1 + blended_deviation / 100)
  # where blended_deviation = w * usmm_dev + (1-w) * gtap_lr_gdp

  gtap_lr_gdp <- if (!is.null(inputs$qgdp) && 'usa' %in% names(inputs$qgdp)) {
    inputs$qgdp['usa']
  } else {
    NULL
  }

  # Compute blended tariff GDP for all quarters
  blended_macro <- macro %>%
    filter(year >= 2024) %>%
    mutate(
      raw_deviation = (gdp_tariff - gdp_baseline) / gdp_baseline * 100,
      q_index = (year - 2025) * 4 + (quarter - 1),
      blend_weight = pmax(0, 1 - q_index / 16),
      blended_deviation = if (!is.null(gtap_lr_gdp)) {
        case_when(
          q_index < 0 ~ raw_deviation,  # Before 2025Q1: no blend
          TRUE ~ blend_weight * raw_deviation + (1 - blend_weight) * gtap_lr_gdp
        )
      } else {
        raw_deviation
      },
      gdp_blended = gdp_baseline * (1 + blended_deviation / 100)
    )

  if (!is.null(gtap_lr_gdp)) {
    message(sprintf('  Blending USMM→GTAP for GDP Q4-Q4 (GTAP LR: %.2f%%)', gtap_lr_gdp))
  }

  q4_blended <- blended_macro %>%
    filter(quarter == 4, year %in% c(2024, 2025, 2026)) %>%
    arrange(year)

  calc_gdp_q4q4 <- function(yr) {
    current <- q4_blended %>% filter(year == yr)
    prior <- q4_blended %>% filter(year == yr - 1)
    if (nrow(current) == 0 || nrow(prior) == 0) {
      return(NA_real_)
    }
    tariff_growth <- current$gdp_blended / prior$gdp_blended
    base_growth <- current$gdp_baseline / prior$gdp_baseline
    return(100 * (tariff_growth / base_growth - 1))
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

  # PCE price level effect (% change at Q4 2025)
  pce_2025 <- if (nrow(q4_2025) > 0 && 'pce_tariff' %in% names(q4_2025)) {
    100 * (q4_2025$pce_tariff / q4_2025$pce_baseline - 1)
  } else {
    NA_real_
  }
  pce_2026 <- if (nrow(q4_2026) > 0 && 'pce_tariff' %in% names(q4_2026)) {
    100 * (q4_2026$pce_tariff / q4_2026$pce_baseline - 1)
  } else {
    NA_real_
  }

  # Fed funds rate effect (pp change at Q4)
  fed_funds_2025 <- if (nrow(q4_2025) > 0 && 'fed_funds_tariff' %in% names(q4_2025)) {
    q4_2025$fed_funds_tariff - q4_2025$fed_funds_baseline
  } else {
    NA_real_
  }
  fed_funds_2026 <- if (nrow(q4_2026) > 0 && 'fed_funds_tariff' %in% names(q4_2026)) {
    q4_2026$fed_funds_tariff - q4_2026$fed_funds_baseline
  } else {
    NA_real_
  }

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

    # PCE price level effect (percentage)
    pce_2025 = pce_2025,
    pce_2026 = pce_2026,

    # Fed funds rate effect (percentage points)
    fed_funds_2025 = fed_funds_2025,
    fed_funds_2026 = fed_funds_2026,

    # Raw quarterly data for reference
    quarterly = macro
  )

  # Log results
  message(sprintf('  GDP 2025 Q4-Q4: %.2f%%', gdp_2025))
  message(sprintf('  GDP 2026 Q4-Q4: %.2f%%', gdp_2026))
  message(sprintf('  U-rate 2025 Q4: %+.2f pp', urate_2025))
  message(sprintf('  U-rate 2026 Q4: %+.2f pp', urate_2026))
  message(sprintf('  Payroll 2025 Q4: %.0f thousand', payroll_2025))
  message(sprintf('  Payroll 2026 Q4: %.0f thousand', payroll_2026))
  message(sprintf('  PCE 2025 Q4: %+.2f%%', pce_2025))
  message(sprintf('  PCE 2026 Q4: %+.2f%%', pce_2026))
  message(sprintf('  Fed funds 2025 Q4: %+.2f pp', fed_funds_2025))
  message(sprintf('  Fed funds 2026 Q4: %+.2f pp', fed_funds_2026))

  return(results)
}
