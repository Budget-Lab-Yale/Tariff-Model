# =============================================================================
# 04_calculate_revenue.R - Calculate tariff revenue estimates
# =============================================================================
#
# This module calculates conventional tariff revenue based on:
# - CBO baseline imports and duties (from resources/baselines/)
# - GTAP qmwreg (percent change in imports from tariffs)
# - Phase-in schedule (from global assumptions)
# - ETR increase (calculated from GTAP postsim vs baseline)
# - Behavioral adjustments (compliance effect, income and payroll tax offset)
#
# Key formula:
#   New Imports = Baseline × (1 - phase_in) + phase_in × Baseline × (1 + qmwreg/100)
#   New Duties = New Imports × Total ETR
#   Gross Revenue = New Duties - Baseline Duties
#   Net Revenue = Gross Revenue × (1 - compliance_effect - income_effect)
#
# =============================================================================

library(tidyverse)

#' Compute FY-weighted average etr_increase from time-varying date schedule
#'
#' For each fiscal year (Oct 1 – Sep 30), computes a day-weighted average
#' etr_increase using step-function logic:
#' - Before the first date: etr_increase = 0
#' - Each date's etr_increase applies from that date until the next date
#' - After the last date: last etr_increase continues
#'
#' @param etr_increase_by_date Tibble with date and etr_increase columns
#' @param fiscal_years Integer vector of fiscal years to compute
#'
#' @return Named numeric vector keyed by fiscal year (as character)
compute_fy_weighted_etr_increase <- function(etr_increase_by_date, fiscal_years) {

  # Sort by date
  schedule <- etr_increase_by_date %>%
    arrange(date)

  dates <- schedule$date
  increases <- schedule$etr_increase

  result <- numeric(length(fiscal_years))
  names(result) <- as.character(fiscal_years)

  for (fy in fiscal_years) {
    # FY runs from Oct 1 of prior CY to Sep 30 of FY year
    fy_start <- as.Date(paste0(fy - 1, '-10-01'))
    fy_end <- as.Date(paste0(fy, '-09-30'))
    fy_days <- as.integer(fy_end - fy_start) + 1L

    # Build intervals within this FY
    # Breakpoints are: fy_start, any schedule dates within [fy_start, fy_end], fy_end+1
    breakpoints <- c(fy_start, dates[dates > fy_start & dates <= fy_end], fy_end + 1L)
    breakpoints <- sort(unique(breakpoints))

    weighted_sum <- 0

    for (i in seq_len(length(breakpoints) - 1)) {
      interval_start <- breakpoints[i]
      interval_end <- breakpoints[i + 1] - 1L
      n_days <- as.integer(interval_end - interval_start) + 1L

      # Find active etr_increase: latest schedule date <= interval_start
      # If before first date, use first regime (tariffs were already active)
      active_idx <- which(dates <= interval_start)
      if (length(active_idx) == 0) {
        active_etr <- increases[1]
      } else {
        active_etr <- increases[max(active_idx)]
      }

      weighted_sum <- weighted_sum + active_etr * n_days
    }

    result[as.character(fy)] <- weighted_sum / fy_days
  }

  return(result)
}


#' Calculate tariff revenue estimates
#'
#' @param inputs List containing:
#'   - baselines$cbo: CBO baseline imports, duties, and baseline_etr
#'   - qmwreg: GTAP import change percentage
#'   - etr_increase: GTAP-derived ETR increase from new tariff policy
#'   - assumptions: Global assumptions (phase_in, compliance_effect, etc.)
#' @param etr_results Results from calculate_etr() (optional, contains etr_increase_by_date for time-varying)
#'
#' @return List containing revenue estimates by year and 10-year totals
calculate_revenue <- function(inputs, etr_results = NULL) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  cbo <- inputs$baselines$cbo
  assumptions <- inputs$assumptions
  qmwreg <- inputs$qmwreg
  etr_increase <- inputs$etr_increase
  refund_2026 <- inputs$model_params$refund_2026 %||% 0

  # Validate required inputs
  if (is.null(cbo)) {
    stop('CBO baselines not found in inputs$baselines$cbo')
  }
  if (is.null(qmwreg)) {
    stop('GTAP qmwreg not found in inputs$qmwreg')
  }
  if (is.null(etr_increase)) {
    stop('GTAP etr_increase not found in inputs$etr_increase')
  }

  # -------------------------------------------------------------------------
  # Get parameters from global assumptions
  # -------------------------------------------------------------------------

  compliance_effect <- assumptions$compliance_effect
  income_effect <- assumptions$income_effect
  phase_in_schedule <- assumptions$phase_in

  message(sprintf('  Using compliance effect: %.1f%%', compliance_effect * 100))
  message(sprintf('  Using income and payroll tax offset: %.1f%%', income_effect * 100))
  message(sprintf('  Using qmwreg: %.2f%%', qmwreg))
  message(sprintf('  Using etr_increase: %.4f (%.2f%%)', etr_increase, etr_increase * 100))
  if (!is.numeric(refund_2026) || length(refund_2026) != 1) {
    stop('refund_2026 must be a single numeric value (billions)')
  }
  if (refund_2026 > 0) {
    message(sprintf('  Applying CY2026 refund: $%.1fB', refund_2026))
  }

  # -------------------------------------------------------------------------
  # Time-varying: compute per-FY etr_increase if available
  # -------------------------------------------------------------------------

  etr_increase_by_date <- if (!is.null(etr_results)) etr_results$etr_increase_by_date else NULL
  use_fy_etr <- !is.null(etr_increase_by_date)

  if (use_fy_etr) {
    fiscal_years <- cbo$fiscal_year
    fy_etr_vec <- compute_fy_weighted_etr_increase(etr_increase_by_date, fiscal_years)
    message('  Time-varying: using per-FY day-weighted etr_increase')
    for (fy in fiscal_years) {
      val <- fy_etr_vec[as.character(fy)]
      message(sprintf('    FY%d: etr_increase=%.4f (%.2f%%)', fy, val, val * 100))
    }
  }

  # -------------------------------------------------------------------------
  # Build revenue calculation table
  # -------------------------------------------------------------------------

  # Create phase-in lookup (all years must be specified)
  get_phase_in <- function(fy) {
    # Handle both integer and character keys in the YAML
    key <- as.character(fy)
    if (!key %in% names(phase_in_schedule)) {
      stop('Missing phase_in value for fiscal year: ', fy)
    }
    return(phase_in_schedule[[key]])
  }

  # Calculate revenue for each fiscal year
  # Note: baseline_etr comes from CBO data (duties/imports) and varies by year
  refund_fy26 <- refund_2026 * 0.75
  refund_fy27 <- refund_2026 * 0.25

  revenue_data <- cbo %>%
    mutate(
      # Get phase-in for each year
      phase_in = sapply(fiscal_year, get_phase_in),

      # Step 1: Calculate new imports with GTAP import response
      new_imports = imports_bn * (1 - phase_in) +
                    phase_in * imports_bn * (1 + qmwreg / 100),

      # Step 2: Calculate total ETR (CBO baseline + policy increase)
      # baseline_etr already exists in CBO data (duties/imports)
      # Time-varying: use per-FY etr_increase; static: scalar etr_increase
      total_etr = baseline_etr + if (use_fy_etr) {
        fy_etr_vec[as.character(fiscal_year)]
      } else {
        etr_increase
      },

      # Step 3: Calculate new duties
      new_duties = new_imports * total_etr,

      # Step 4: Gross revenue = New duties - Baseline duties
      gross_revenue = new_duties - duties_bn,

      # Step 5: Behavioral adjustments
      compliance_adj = gross_revenue * -compliance_effect,
      income_adj = gross_revenue * -income_effect,

      # Step 6: Net revenue
      refund_adj = case_when(
        fiscal_year == 2026 ~ -refund_fy26,
        fiscal_year == 2027 ~ -refund_fy27,
        TRUE ~ 0
      ),
      net_revenue = gross_revenue + compliance_adj + income_adj + refund_adj
    )

  # -------------------------------------------------------------------------
  # Calculate 10-year totals (FY2026-FY2035)
  # -------------------------------------------------------------------------

  ten_year_data <- revenue_data %>%
    filter(fiscal_year >= 2026, fiscal_year <= 2035)

  conventional_10yr <- sum(ten_year_data$net_revenue)
  gross_10yr <- sum(ten_year_data$gross_revenue)

  message(sprintf('  10-year gross revenue (FY26-35): $%.1fB', gross_10yr))
  message(sprintf('  10-year conventional revenue (FY26-35): $%.1fB', conventional_10yr))

  # -------------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------------

  results <- list(
    # Annual data
    by_year = revenue_data,

    # 10-year totals
    conventional_10yr = conventional_10yr,
    gross_10yr = gross_10yr,

    # Parameters used
    params_used = list(
      compliance_effect = compliance_effect,
      income_effect = income_effect,
      qmwreg = qmwreg,
      etr_increase = etr_increase
    )
  )

  return(results)
}


#' Print revenue summary table
#'
#' @param revenue_results Results from calculate_revenue()
print_revenue_summary <- function(revenue_results) {

  cat('\n----------------------------------------------------------\n')
  cat('REVENUE ESTIMATES BY FISCAL YEAR\n')
  cat('----------------------------------------------------------\n')
  cat(sprintf('%-6s %10s %10s %10s %10s %10s\n',
              'FY', 'Imports', 'New Imp', 'Duties', 'Gross', 'Net'))
  cat(sprintf('%-6s %10s %10s %10s %10s %10s\n',
              '', '(base)', '', '(new)', 'Rev', 'Rev'))
  cat('----------------------------------------------------------\n')

  revenue_results$by_year %>%
    filter(fiscal_year >= 2026) %>%
    select(fiscal_year, imports_bn, new_imports, new_duties, gross_revenue, net_revenue) %>%
    mutate(across(where(is.numeric), ~round(., 1))) %>%
    pwalk(function(fiscal_year, imports_bn, new_imports, new_duties, gross_revenue, net_revenue) {
      cat(sprintf('%-6d %10.0f %10.0f %10.0f %10.0f %10.0f\n',
                  fiscal_year, imports_bn, new_imports, new_duties, gross_revenue, net_revenue))
    })

  cat('----------------------------------------------------------\n')
  cat(sprintf('10-year conventional revenue: $%.0fB\n', revenue_results$conventional_10yr))
  cat('----------------------------------------------------------\n')
}
