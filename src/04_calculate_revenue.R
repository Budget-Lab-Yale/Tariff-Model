# =============================================================================
# 04_calculate_revenue.R - Calculate tariff revenue estimates
# =============================================================================
#
# This module calculates conventional tariff revenue based on:
# - CBO baseline imports and duties (from resources/baselines/)
# - GTAP qmwreg (percent change in imports from tariffs)
# - long_run_transition (from global assumptions): GTAP weight w by quarter
# - Short-run rate (each date's rates on the historical import mix) and
#   long-run rate (GTAP postsim vs baseline), blended day by day
# - Behavioral adjustments (compliance effect, income and payroll tax offset)
#
# Key formula:
#   New Imports (per day) = Baseline × (1 + w × qmwreg/100)
#   Policy rate (per day) = (1 - w) × short-run rate + w × long-run rate
#   New Duties = sum over the fiscal year's days of New Imports × Total ETR
#   Gross Revenue = New Duties - Baseline Duties
#   Net Revenue = Gross Revenue × (1 - compliance_effect - income_effect)
#
# =============================================================================

library(tidyverse)

# A dated policy starts on its first date. Undated rates describe a static regime.
revenue_rate_on <- function(dates, schedule, static_rate) {
  if (is.null(schedule)) {
    if (length(static_rate) != 1L || !is.finite(static_rate)) stop('Missing revenue rate')
    return(rep(static_rate, length(dates)))
  }
  schedule <- arrange(schedule, date)
  if (!nrow(schedule) || anyNA(schedule$date) || anyDuplicated(schedule$date) ||
      any(!is.finite(schedule$etr_increase))) stop('Invalid dated revenue rates')
  c(0, schedule$etr_increase)[1L + findInterval(dates, schedule$date)]
}

# Average collections over the days in each fiscal year. Applying the rates
# before averaging gives an April tariff only its actual days in FY 2027.
fiscal_year_revenue_rates <- function(years, etr_results, long_rate, qmwreg, timing) {
  dates <- seq(as.Date(sprintf('%d-10-01', min(years) - 1L)),
               as.Date(sprintf('%d-09-30', max(years))), by = 'day')
  short <- revenue_rate_on(dates, etr_results$short_run_etr_by_date,
                           etr_results$short_run_etr_increase)
  long <- revenue_rate_on(dates, etr_results$etr_increase_by_date, long_rate)
  weight <- long_run_weight(year(dates), quarter(dates), timing)
  tibble(fiscal_year = year(dates) + (month(dates) >= 10),
         weight = weight, imports_ratio = 1 + weight * qmwreg / 100,
         rate = (1 - weight) * short + weight * long) %>%
    group_by(fiscal_year) %>%
    summarise(phase_in = mean(weight), policy_rate = weighted.mean(rate, imports_ratio),
              imports_ratio = mean(imports_ratio), .groups = 'drop')
}


#' Calculate tariff revenue estimates
#'
#' @param inputs List containing:
#'   - baselines$cbo: CBO baseline imports, duties, and baseline_etr
#'   - qmwreg: GTAP import change percentage
#'   - etr_increase: GTAP-derived ETR increase from new tariff policy
#'   - assumptions: Global assumptions (long_run_transition, compliance_effect, etc.)
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

  # When noncompliance is active, the GTAP shock — and thus the mtax-derived
  # etr_increase that drives gross_revenue — already reflects rate (b). Applying
  # the revenue-side compliance haircut again would double-count, so zero it out.
  # Legacy (inactive) scenarios keep the historical haircut. Bridge scenarios can
  # opt back into the historical haircut while using a neutral eta file so rates
  # remain statutory (b == a) but revenue follows the old flat-compliance method.
  noncompliance_active <- isTRUE(inputs$noncompliance_active)
  legacy_revenue_haircut <- isTRUE(inputs$model_params$noncompliance$legacy_revenue_haircut)
  compliance_effect_applied <- if (noncompliance_active && !legacy_revenue_haircut) 0 else compliance_effect

  if (noncompliance_active && legacy_revenue_haircut) {
    message(sprintf('  Noncompliance active with neutral/bridge override: applying legacy revenue haircut %.1f%%',
                    compliance_effect * 100))
  } else if (noncompliance_active) {
    message('  Noncompliance active: haircut applied upstream via eta\' (revenue compliance_effect = 0)')
  } else {
    message(sprintf('  Using compliance effect: %.1f%%', compliance_effect * 100))
  }
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
  # Both import volume and the collected rate follow the quarterly GDP clock.
  # For a static call without ETR detail the supplied rate is the only rate.
  if (is.null(etr_results)) etr_results <- list(short_run_etr_increase = etr_increase)
  timing <- assumptions$long_run_transition
  fy_rates <- fiscal_year_revenue_rates(cbo$fiscal_year, etr_results, etr_increase,
                                        qmwreg, timing)

  refund_fy26 <- refund_2026 * 0.75
  refund_fy27 <- refund_2026 * 0.25

  revenue_data <- cbo %>%
    left_join(fy_rates, by = 'fiscal_year') %>%
    mutate(
      new_imports = imports_bn * imports_ratio,
      total_etr = baseline_etr + policy_rate,

      # Step 3: Calculate new duties
      new_duties = new_imports * total_etr,

      # Step 4: Gross revenue = New duties - Baseline duties
      gross_revenue = new_duties - duties_bn,

      # Step 5: Behavioral adjustments
      compliance_adj = gross_revenue * -compliance_effect_applied,
      income_adj = gross_revenue * -income_effect,

      # Step 6: Net revenue
      refund_adj = case_when(
        fiscal_year == 2026 ~ -refund_fy26,
        fiscal_year == 2027 ~ -refund_fy27,
        TRUE ~ 0
      ),
      net_revenue = gross_revenue + compliance_adj + income_adj + refund_adj
    ) %>%
    select(-imports_ratio, -policy_rate)

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
      compliance_effect = compliance_effect_applied,
      income_effect = income_effect,
      qmwreg = qmwreg,
      etr_increase = etr_increase,
      long_run_transition = timing
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
