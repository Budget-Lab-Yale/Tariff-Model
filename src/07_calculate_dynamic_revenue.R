# =============================================================================
# 07_calculate_dynamic_revenue.R - Calculate dynamic revenue effects
# =============================================================================
#
# This module calculates dynamic revenue by applying CBO's budgetary rules
# to GDP impacts from MAUS.
#
# Key formula:
#   dynamic_effect = sensitivity × (nominal_gdp_scenario - nominal_gdp_baseline)
#
# Where:
#   - sensitivity comes from CBO's rules of thumb (revenue per $1B nominal GDP)
#   - GDP must be converted from real (MAUS) to nominal (CBO basis)
#   - Fiscal year GDP = 0.75 × current CY + 0.25 × prior CY
#
# =============================================================================

library(tidyverse)

#' Calculate dynamic revenue effects
#'
#' Applies CBO dynamic scoring rules to GDP impacts from MAUS. The dynamic
#' effect is calculated as: sensitivity × (nominal_gdp_scenario - nominal_gdp_baseline)
#'
#' Note: Requires MAUS data for each year. Years without MAUS data will have
#' dynamic_effect = 0. For complete 10-year estimates (FY2026-2035), MAUS
#' projections should extend through 2035.
#'
#' @param inputs List containing:
#'   - maus: Quarterly GDP data with baseline and tariff scenarios
#'   - cbo_sensitivity: CBO sensitivity parameters by fiscal year
#' @param revenue_results Results from calculate_revenue() with conventional revenue
#'
#' @return List containing dynamic revenue estimates
calculate_dynamic_revenue <- function(inputs, revenue_results) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  maus <- inputs$maus
  cbo_sens <- inputs$cbo_sensitivity

  if (is.null(maus)) {
    stop('MAUS quarterly data not found in inputs$maus')
  }

  if (is.null(cbo_sens)) {
    # Try to load from default location
    cbo_file <- 'resources/cbo_rules/dynamic_scoring.csv'
    if (file.exists(cbo_file)) {
      cbo_sens <- read_csv(cbo_file, show_col_types = FALSE)
      message('  Loaded CBO sensitivity parameters')
    } else {
      stop('CBO sensitivity data not found')
    }
  }

  message(sprintf('  Processing %d quarters of MAUS data', nrow(maus)))

  # Check MAUS coverage
  max_maus_year <- max(maus$year)
  if (max_maus_year < 2035) {
    message(sprintf('  Note: MAUS data ends in %d; dynamic effects for %d-2035 will be 0',
                    max_maus_year, max_maus_year + 1))
  }

  # -------------------------------------------------------------------------
  # Convert quarterly real GDP to annual nominal GDP
  # -------------------------------------------------------------------------

  # Calculate annual (calendar year) averages
  annual_gdp <- maus %>%
    group_by(year) %>%
    summarise(
      real_gdp_baseline = mean(gdp_baseline),
      real_gdp_tariff = mean(gdp_tariff),
      .groups = 'drop'
    )

  # Convert to fiscal year (FY = 0.75 * CY + 0.25 * prior CY)
  # FY2026 = 0.75 * CY2026 + 0.25 * CY2025
  fy_gdp <- annual_gdp %>%
    arrange(year) %>%
    mutate(
      prior_real_baseline = lag(real_gdp_baseline),
      prior_real_tariff = lag(real_gdp_tariff),
      fiscal_year = year,
      fy_real_baseline = 0.75 * real_gdp_baseline + 0.25 * coalesce(prior_real_baseline, real_gdp_baseline),
      fy_real_tariff = 0.75 * real_gdp_tariff + 0.25 * coalesce(prior_real_tariff, real_gdp_tariff)
    ) %>%
    select(fiscal_year, fy_real_baseline, fy_real_tariff)

  # Join with CBO sensitivity data to get nominal GDP baseline and convert
  # CBO sensitivity formula: revenue_change = revenue_per_gdp × gdp_change
  # Derived from CBO rules of thumb workbook: sensitivity = cbo_revenue_change / cbo_gdp_change
  fy_gdp <- fy_gdp %>%
    left_join(cbo_sens, by = 'fiscal_year') %>%
    filter(!is.na(revenue_per_gdp)) %>%
    mutate(
      # Calculate deflator (nominal/real ratio from CBO baseline)
      # Use CBO nominal baseline and our real baseline
      deflator = cbo_gdp_base / fy_real_baseline,

      # Convert real GDP to nominal
      fy_nominal_baseline = fy_real_baseline * deflator,
      fy_nominal_tariff = fy_real_tariff * deflator,

      # Calculate GDP change
      nominal_gdp_change = fy_nominal_tariff - fy_nominal_baseline,

      # Apply CBO sensitivity to get dynamic effect
      # revenue_per_gdp = how much revenue changes per $1B GDP change
      dynamic_effect = revenue_per_gdp * nominal_gdp_change
    )

  # -------------------------------------------------------------------------
  # Calculate dynamic revenue by year
  # -------------------------------------------------------------------------

  # Get conventional revenue by year
  conv_by_year <- revenue_results$by_year %>%
    select(fiscal_year, net_revenue)

  # Join and calculate dynamic revenue
  dynamic_by_year <- conv_by_year %>%
    left_join(
      fy_gdp %>% select(fiscal_year, nominal_gdp_change, dynamic_effect),
      by = 'fiscal_year'
    ) %>%
    mutate(
      # Fill NA dynamic effects with 0 (years without MAUS data)
      dynamic_effect = coalesce(dynamic_effect, 0),
      dynamic_revenue = net_revenue + dynamic_effect
    )

  # -------------------------------------------------------------------------
  # Calculate 10-year totals
  # -------------------------------------------------------------------------

  ten_year <- dynamic_by_year %>%
    filter(fiscal_year >= 2026, fiscal_year <= 2035)

  conventional_10yr <- sum(ten_year$net_revenue)
  dynamic_effect_10yr <- sum(ten_year$dynamic_effect)
  dynamic_10yr <- sum(ten_year$dynamic_revenue)

  message(sprintf('  10-year conventional revenue: $%.1fB', conventional_10yr))
  message(sprintf('  10-year dynamic effect: $%.1fB', dynamic_effect_10yr))
  message(sprintf('  10-year dynamic revenue: $%.1fB', dynamic_10yr))

  # -------------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------------

  # Count years with actual dynamic data
  years_with_data <- sum(ten_year$dynamic_effect != 0)
  if (years_with_data < 10) {
    message(sprintf('  Warning: Only %d of 10 years have MAUS data; totals may underestimate dynamic effects',
                    years_with_data))
  }

  results <- list(
    by_year = dynamic_by_year,
    conventional_10yr = conventional_10yr,
    dynamic_effect_10yr = dynamic_effect_10yr,
    dynamic_10yr = dynamic_10yr,
    gdp_details = fy_gdp,
    years_with_data = years_with_data
  )

  return(results)
}


#' Print dynamic revenue summary
#'
#' @param dynamic_results Results from calculate_dynamic_revenue()
print_dynamic_revenue_summary <- function(dynamic_results) {

  cat('\n----------------------------------------------------------\n')
  cat('DYNAMIC REVENUE ESTIMATES\n')
  cat('----------------------------------------------------------\n')
  cat(sprintf('%-6s %12s %12s %12s\n', 'FY', 'Conventional', 'Dyn Effect', 'Dynamic'))
  cat('----------------------------------------------------------\n')

  dynamic_results$by_year %>%
    filter(fiscal_year >= 2026) %>%
    pwalk(function(fiscal_year, net_revenue, dynamic_effect, dynamic_revenue, ...) {
      cat(sprintf('%-6d %12.1f %12.1f %12.1f\n',
                  fiscal_year, net_revenue, dynamic_effect, dynamic_revenue))
    })

  cat('----------------------------------------------------------\n')
  cat(sprintf('10-yr  %12.1f %12.1f %12.1f\n',
              dynamic_results$conventional_10yr,
              dynamic_results$dynamic_effect_10yr,
              dynamic_results$dynamic_10yr))
  cat('----------------------------------------------------------\n')
}
