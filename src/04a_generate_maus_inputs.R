# =============================================================================
# 04a_generate_maus_inputs.R - Generate MAUS shock inputs
# =============================================================================
#
# This module generates the VIX and UTFIBC shock series that feed into MAUS.
# The model pauses after generating these so the user can run MAUS externally.
#
# Key calculations:
#   VIX: Quarterly average of daily VIX values
#   UTFIBC: Baseline + ETR-based shock (interpolates from pre-sub to post-sub)
#
# Outputs:
#   - Quarterly shock series for MAUS input
#   - Written to output/{scenario}/maus_inputs/shocks.csv
#
# =============================================================================

library(tidyverse)

#' Load daily VIX data and compute quarterly averages
#'
#' @param vix_file Path to daily VIX CSV file
#' @param start_date Start date for shock period
#' @param n_quarters Number of quarters to generate
#'
#' @return Tibble with quarter dates and VIX values
compute_quarterly_vix <- function(vix_file, start_date, n_quarters = 44) {

  if (!file.exists(vix_file)) {
    stop('VIX data file not found: ', vix_file)
  }

  daily_vix <- read_csv(vix_file, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))

  # Generate quarterly date sequence
  quarters <- tibble(
    quarter_start = seq(as.Date(start_date), by = 'quarter', length.out = n_quarters)
  ) %>%
    mutate(
      quarter_end = lead(quarter_start, default = max(quarter_start) + 90) - 1,
      year = year(quarter_start),
      quarter = quarter(quarter_start)
    )

  # Compute quarterly averages
  quarterly_vix <- quarters %>%
    rowwise() %>%
    mutate(
      vix = {
        period_data <- daily_vix %>%
          filter(date >= quarter_start, date <= quarter_end)
        if (nrow(period_data) > 0) {
          mean(period_data$vix)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()

  # Fill forward any missing VIX values (for quarters without data)
  quarterly_vix <- quarterly_vix %>%
    fill(vix, .direction = 'down') %>%
    # If still NA at start, fill backward
    fill(vix, .direction = 'up')

  return(quarterly_vix)
}


#' Compute UTFIBC shock series from ETR
#'
#' @param baseline_utfibc Baseline UTFIBC rate (typically ~3%)
#' @param pre_sub_etr Pre-substitution ETR increase (as decimal, e.g., 0.1439)
#' @param post_sub_etr Post-substitution ETR increase (as decimal, e.g., 0.1194)
#' @param shock_start_quarter Which quarter (1-indexed) the shock begins
#' @param initial_shock Initial shock value for first shock quarter (from VIX spike)
#' @param interpolation_quarters Number of quarters to interpolate from pre to post
#' @param n_quarters Total number of quarters to generate
#'
#' @return Vector of UTFIBC values (baseline + shock)
compute_utfibc_shocks <- function(baseline_utfibc = 3.0,
                                   pre_sub_etr,
                                   post_sub_etr,
                                   shock_start_quarter = 4,
                                   initial_shock = NULL,
                                   interpolation_quarters = 12,
                                   n_quarters = 44) {

  # Convert ETR to percentage points (as used in MAUS)
  pre_sub_shock <- pre_sub_etr * 100
  post_sub_shock <- post_sub_etr * 100

  # If no initial shock specified, use a VIX-derived estimate
  # (roughly 38% of pre-sub shock based on Excel pattern)
  if (is.null(initial_shock)) {
    initial_shock <- pre_sub_shock * 0.38
  }

  # Build the shock series
  utfibc <- rep(baseline_utfibc, n_quarters)

  for (q in seq_len(n_quarters)) {
    if (q < shock_start_quarter) {
      # Before shock: baseline only
      utfibc[q] <- baseline_utfibc
    } else if (q == shock_start_quarter) {
      # First shock quarter: baseline + initial shock
      utfibc[q] <- baseline_utfibc + initial_shock
    } else {
      # Subsequent quarters: interpolate from pre-sub to post-sub
      quarters_after_start <- q - shock_start_quarter

      if (quarters_after_start <= interpolation_quarters) {
        # Linear interpolation
        shock <- pre_sub_shock + (post_sub_shock - pre_sub_shock) *
          (quarters_after_start - 1) / interpolation_quarters
      } else {
        # After interpolation period: stay at post-sub level
        shock <- post_sub_shock
      }

      utfibc[q] <- baseline_utfibc + shock
    }
  }

  return(utfibc)
}


#' Generate complete MAUS input shock series
#'
#' @param etr_results Results from calculate_etr() containing pre/post sub ETRs
#' @param inputs Model inputs (for parameters)
#' @param scenario Scenario name
#'
#' @return List containing shock data and file path
generate_maus_inputs <- function(etr_results, inputs, scenario) {

  message('  Generating MAUS input shocks...')

  # -------------------------------------------------------------------------
  # Parameters
  # -------------------------------------------------------------------------

  # Get model parameters (with defaults)
  maus_params <- inputs$model_params$maus %||% list()

  start_date <- maus_params$start_date %||% '2024-07-01'
  n_quarters <- maus_params$n_quarters %||% 44
  baseline_utfibc <- maus_params$baseline_utfibc %||% 3.0
  shock_start_quarter <- maus_params$shock_start_quarter %||% 4
  interpolation_quarters <- maus_params$interpolation_quarters %||% 12

  # ETR values from calculation results
  pre_sub_etr <- etr_results$pre_sub_increase / 100
  post_sub_etr <- etr_results$post_sub_increase / 100

  message(sprintf('    Pre-sub ETR: %.2f%%', pre_sub_etr * 100))
  message(sprintf('    Post-sub ETR: %.2f%%', post_sub_etr * 100))

  # -------------------------------------------------------------------------
  # Compute VIX quarterly series
  # -------------------------------------------------------------------------

  vix_file <- 'resources/vix/daily_vix.csv'
  quarterly_vix <- compute_quarterly_vix(vix_file, start_date, n_quarters)

  message(sprintf('    Computed VIX for %d quarters', nrow(quarterly_vix)))

  # -------------------------------------------------------------------------
  # Compute UTFIBC shock series
  # -------------------------------------------------------------------------

  utfibc <- compute_utfibc_shocks(
    baseline_utfibc = baseline_utfibc,
    pre_sub_etr = pre_sub_etr,
    post_sub_etr = post_sub_etr,
    shock_start_quarter = shock_start_quarter,
    interpolation_quarters = interpolation_quarters,
    n_quarters = n_quarters
  )

  message(sprintf('    Computed UTFIBC shocks (starts Q%d)', shock_start_quarter))

  # -------------------------------------------------------------------------
  # Apply CY2026 refund adjustment (spread evenly across Q1-Q4)
  # -------------------------------------------------------------------------

  refund_2026 <- inputs$model_params$refund_2026 %||% 0
  if (!is.numeric(refund_2026) || length(refund_2026) != 1) {
    stop('refund_2026 must be a single numeric value (billions)')
  }
  if (refund_2026 > 0) {
    cbo <- inputs$baselines$cbo
    fy_2026 <- cbo %>% filter(fiscal_year == 2026) %>% pull(imports_bn)
    fy_2027 <- cbo %>% filter(fiscal_year == 2027) %>% pull(imports_bn)
    if (length(fy_2026) != 1 || length(fy_2027) != 1) {
      stop('CBO baseline imports not found for FY2026/FY2027')
    }
    cy_2026_imports <- 0.75 * fy_2026 + 0.25 * fy_2027
    refund_pp <- refund_2026 / cy_2026_imports * 100
    message(sprintf('    Applying CY2026 refund to UTFIBC: -%.2f pp', refund_pp))
    quarterly_vix <- quarterly_vix %>%
      mutate(refund_pp = if_else(year == 2026, refund_pp, 0))
    utfibc <- utfibc - quarterly_vix$refund_pp
  }

  # -------------------------------------------------------------------------
  # Build output dataframe
  # -------------------------------------------------------------------------

  shocks <- quarterly_vix %>%
    select(year, quarter, quarter_start, vix) %>%
    mutate(utfibc = utfibc) %>%
    # Round for cleaner output
    mutate(
      vix = round(vix, 2),
      utfibc = round(utfibc, 2)
    )

  # -------------------------------------------------------------------------
  # Write output file
  # -------------------------------------------------------------------------

  output_dir <- file.path('output', scenario, 'maus_inputs')
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  output_file <- file.path(output_dir, 'shocks.csv')
  write_csv(shocks, output_file)

  message(sprintf('    Wrote shocks to: %s', output_file))

  # -------------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------------

  results <- list(
    shocks = shocks,
    output_file = output_file,
    pre_sub_etr = pre_sub_etr,
    post_sub_etr = post_sub_etr,
    n_quarters = n_quarters
  )

  return(results)
}


#' Print shock summary for user
#'
#' @param maus_inputs Results from generate_maus_inputs()
print_shock_summary <- function(maus_inputs) {

  shocks <- maus_inputs$shocks

  cat('\n')
  cat('===========================================================\n')
  cat('MAUS INPUT SHOCKS GENERATED\n')
  cat('===========================================================\n')
  cat('\n')
  cat('Shock file: ', maus_inputs$output_file, '\n')
  cat('\n')
  cat('First 8 quarters:\n')
  cat('-----------------------------------------------------------\n')
  cat(sprintf('%-6s %-4s %10s %10s\n', 'Year', 'Qtr', 'VIX', 'UTFIBC'))
  cat('-----------------------------------------------------------\n')

  for (i in 1:min(8, nrow(shocks))) {
    cat(sprintf('%-6d %-4d %10.2f %10.2f\n',
                shocks$year[i], shocks$quarter[i],
                shocks$vix[i], shocks$utfibc[i]))
  }

  cat('-----------------------------------------------------------\n')
  cat('\n')
}
