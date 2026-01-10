# =============================================================================
# 07_calculate_dynamic_revenue.R - Calculate dynamic revenue effects
# =============================================================================
#
# This module calculates dynamic revenue by applying CBO's Rules of Thumb
# methodology, including impulse response convolution.
#
# The CBO model works as follows (from 61183-Rules-of-Thumb-2025):
#   1. User inputs FY growth rate deviations (percentage points)
#   2. Apply to CBO baseline productivity growth to get scenario growth
#   3. Cumulate growth rates to get level indices
#   4. Compute level deviation = scenario_index / baseline_index - 1
#   5. Apply impulse response convolution to real GDP
#   6. Scale to nominal GDP
#   7. Apply FY weighting (Q2-Q4 at 0.75 + Q1 at 0.25)
#   8. Compute dynamic effect = CBO_sensitivity × GDP_change
#
# CBO sensitivity formula:
#   sensitivity = CBO_revenue_change / (CBO_GDP_scenario - CBO_GDP_baseline)
#
# =============================================================================

library(tidyverse)

#' Calculate FY growth deviations from MAUS quarterly data
#'
#' Maps calendar quarters to fiscal years and computes the incremental
#' growth deviation for each fiscal year.
#'
#' Implements Excel blending logic: starting from 2026 Q1, level deviations
#' are floored at the GTAP long-run GDP effect (can't recover beyond long-run).
#' Formula: MIN(maus_level_dev, gtap_long_run_gdp) for 2026 Q1 onwards.
#'
#' @param maus Quarterly GDP data with baseline and tariff scenarios
#' @param gtap_long_run_gdp GTAP long-run US GDP percent change (e.g., -0.31)
#' @return Data frame with fiscal_year and growth_deviation
calculate_fy_growth_deviations <- function(maus, gtap_long_run_gdp = NULL) {

  # Map calendar quarters to fiscal years
  # FY runs Oct-Sep: Q4 → next FY Q1, Q1-Q3 → same FY
  maus_fy <- maus %>%
    mutate(
      fiscal_year = if_else(quarter == 4, year + 1, year),
      # Raw level deviation (%) for each quarter
      raw_level_deviation = 100 * (gdp_tariff / gdp_baseline - 1)
    )

  # Apply GTAP long-run GDP floor starting from 2026 Q1
  # Excel logic: MIN(maus_deviation, gtap_long_run) for 2026 Q1 onwards
  # Since both are negative, MIN selects the more negative (worse) value,
  # preventing recovery beyond the GTAP long-run equilibrium
  if (!is.null(gtap_long_run_gdp)) {
    maus_fy <- maus_fy %>%
      mutate(
        level_deviation = case_when(
          # Through 2025 Q4: use raw MAUS deviation
          year < 2026 ~ raw_level_deviation,
          # 2026 Q1 onwards: apply MIN (floor at GTAP long-run)
          TRUE ~ pmin(raw_level_deviation, gtap_long_run_gdp)
        )
      )
    message(sprintf('  Applying GTAP long-run GDP floor: %.2f%% (from 2026 Q1)', gtap_long_run_gdp))
  } else {
    maus_fy <- maus_fy %>%
      mutate(level_deviation = raw_level_deviation)
  }

  # Average level deviation by fiscal year
  fy_level_dev <- maus_fy %>%
    group_by(fiscal_year) %>%
    summarise(
      avg_level_deviation = mean(level_deviation),
      n_quarters = n(),
      .groups = 'drop'
    ) %>%
    filter(n_quarters == 4)  # Only complete fiscal years

  # Compute incremental growth deviation (year-over-year change)
  fy_growth_dev <- fy_level_dev %>%
    arrange(fiscal_year) %>%
    mutate(
      prior_level_deviation = lag(avg_level_deviation, default = 0),
      growth_deviation = avg_level_deviation - prior_level_deviation
    )

  return(fy_growth_dev)
}


#' Apply CBO impulse response convolution
#'
#' Implements the CBO Rules of Thumb convolution formula:
#'   scenario_gdp[t] = baseline_gdp[t] * (1 + sum(impulse[s] * level_dev[t-s+1]))
#'
#' @param growth_deviations Data frame with fiscal_year and growth_deviation
#' @param cbo_params CBO convolution parameters
#' @return Data frame with convolved GDP values
apply_cbo_convolution <- function(growth_deviations, cbo_params) {

  # Filter CBO params to fiscal years with available growth deviations
  available_years <- growth_deviations$fiscal_year
  df <- cbo_params %>%
    filter(fiscal_year %in% available_years) %>%
    left_join(
      growth_deviations %>% select(fiscal_year, growth_deviation),
      by = 'fiscal_year'
    )

  if (nrow(df) == 0) {
    stop('No matching fiscal years between growth_deviations and cbo_params')
  }

  n <- nrow(df)

  # Step 1: Compute scenario productivity growth
  # Row 17 = Row 16 + Row 13
  df <- df %>%
    mutate(scenario_productivity_growth = baseline_productivity_growth + growth_deviation)

  # Step 2: Cumulate into level indices (rows 113-114)
  # Starting from base = 100
  df <- df %>%
    mutate(
      # Cumulative baseline index
      baseline_index = cumprod(1 + baseline_productivity_growth / 100) * 100,
      # Cumulative scenario index
      scenario_index = cumprod(1 + scenario_productivity_growth / 100) * 100,
      # Level deviation for convolution
      level_dev = scenario_index / baseline_index - 1
    )

  # Step 3: Apply impulse response convolution (row 64)
  # GDP_scenario[t] = GDP_baseline[t] * (1 + sum(impulse[s] * level_dev[t-s+1]))
  impulse <- df$impulse_response
  level_dev <- df$level_dev
  baseline_real <- df$baseline_real_gdp

  scenario_real <- numeric(n)
  for (t in 1:n) {
    conv_sum <- 0
    for (s in 1:t) {
      # impulse[s] is the s-lag coefficient
      # level_dev[t-s+1] is the level deviation from year t-s+1
      conv_sum <- conv_sum + impulse[s] * level_dev[t - s + 1]
    }
    scenario_real[t] <- baseline_real[t] * (1 + conv_sum)
  }

  df$scenario_real_gdp <- scenario_real

  # Step 4: Scale to nominal GDP (row 67)
  # Ratio = scenario_real / baseline_real
  # Scenario_nominal = baseline_nominal * ratio
  df <- df %>%
    mutate(
      real_ratio = scenario_real_gdp / baseline_real_gdp,
      scenario_nominal_gdp = baseline_nominal_gdp * real_ratio
    )

  # Step 5: Apply FY weighting (row 70)
  # FY_gdp = baseline + (scenario - baseline)_current * 0.75 + (scenario - baseline)_prior * 0.25
  df <- df %>%
    mutate(
      nominal_diff = scenario_nominal_gdp - baseline_nominal_gdp,
      prior_nominal_diff = lag(nominal_diff, default = 0),
      fy_scenario_gdp = baseline_nominal_gdp + nominal_diff * 0.75 + prior_nominal_diff * 0.25,
      fy_gdp_change = fy_scenario_gdp - baseline_nominal_gdp
    )

  # Step 6: Compute CBO sensitivity and dynamic effect (row 24)
  # sensitivity = CBO_revenue_change / (CBO_scenario - CBO_baseline)
  # dynamic_effect = sensitivity * fy_gdp_change
  df <- df %>%
    mutate(
      cbo_gdp_change = cbo_gdp_scenario - cbo_gdp_baseline,
      cbo_sensitivity = cbo_revenue_change / cbo_gdp_change,
      dynamic_effect = round(cbo_sensitivity * fy_gdp_change, 3)
    )

  return(df)
}


#' Calculate dynamic revenue effects
#'
#' Applies CBO dynamic scoring rules to GDP impacts from MAUS, using the
#' full CBO Rules of Thumb methodology with impulse response convolution.
#'
#' @param inputs List containing:
#'   - maus: Quarterly GDP data with baseline and tariff scenarios
#'   - cbo_convolution_params: CBO parameters (or loads from default)
#' @param revenue_results Results from calculate_revenue() with conventional revenue
#'
#' @return List containing dynamic revenue estimates
calculate_dynamic_revenue <- function(inputs, revenue_results) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  maus <- inputs$maus

  if (is.null(maus)) {
    stop('MAUS quarterly data not found in inputs$maus')
  }

  # Load CBO convolution parameters
  cbo_conv_file <- 'resources/cbo_rules/cbo_convolution_params.csv'
  if (!file.exists(cbo_conv_file)) {
    stop('CBO convolution parameters not found: ', cbo_conv_file)
  }
  cbo_params <- read_csv(cbo_conv_file, show_col_types = FALSE)
  assert_has_columns(cbo_params,
                     c('fiscal_year', 'baseline_productivity_growth', 'impulse_response',
                       'baseline_real_gdp', 'baseline_nominal_gdp',
                       'cbo_gdp_baseline', 'cbo_gdp_scenario', 'cbo_revenue_change'),
                     'CBO convolution parameters')
  message('  Loaded CBO convolution parameters')

  message(sprintf('  Processing %d quarters of MAUS data', nrow(maus)))

  # Get GTAP long-run US GDP effect for blending
  gtap_long_run_gdp <- NULL
  if (!is.null(inputs$qgdp) && 'usa' %in% names(inputs$qgdp)) {
    gtap_long_run_gdp <- inputs$qgdp['usa']
    message(sprintf('  GTAP long-run US GDP: %.2f%%', gtap_long_run_gdp))
  }

  # -------------------------------------------------------------------------
  # Step 1: Calculate FY growth deviations from MAUS
  # -------------------------------------------------------------------------

  fy_growth_dev <- calculate_fy_growth_deviations(maus, gtap_long_run_gdp)

  message('  FY growth deviations (matching Excel AI column):')
  fy_growth_dev %>%
    filter(fiscal_year >= 2025, fiscal_year <= 2029) %>%
    pwalk(function(fiscal_year, growth_deviation, ...) {
      message(sprintf('    FY%d: %.3f%%', fiscal_year, growth_deviation))
    })

  # -------------------------------------------------------------------------
  # Step 2: Apply CBO convolution to get dynamic effects
  # -------------------------------------------------------------------------

  convolution_results <- apply_cbo_convolution(fy_growth_dev, cbo_params)

  message('  CBO convolution results (matching Excel row 24):')
  convolution_results %>%
    filter(fiscal_year >= 2025, fiscal_year <= 2029) %>%
    pwalk(function(fiscal_year, fy_gdp_change, dynamic_effect, ...) {
      message(sprintf('    FY%d: GDP change $%.1fB, Dynamic effect $%.1fB',
                      fiscal_year, fy_gdp_change, dynamic_effect))
    })

  # -------------------------------------------------------------------------
  # Calculate dynamic revenue by year
  # -------------------------------------------------------------------------

  # Get conventional revenue by year
  conv_by_year <- revenue_results$by_year %>%
    select(fiscal_year, net_revenue)

  # Join and calculate dynamic revenue
  dynamic_by_year <- conv_by_year %>%
    left_join(
      convolution_results %>% select(fiscal_year, fy_gdp_change, dynamic_effect),
      by = 'fiscal_year'
    )

  # Handle years without dynamic effects (incomplete MAUS data)
  # Use 0 for missing years (dynamic revenue = conventional revenue)
  if (any(is.na(dynamic_by_year$dynamic_effect))) {
    missing_years <- dynamic_by_year$fiscal_year[is.na(dynamic_by_year$dynamic_effect)]
    message(sprintf('  Note: Using 0 dynamic effect for FY %s (incomplete MAUS data)',
                    paste(missing_years, collapse = ', ')))
    dynamic_by_year <- dynamic_by_year %>%
      mutate(
        fy_gdp_change = coalesce(fy_gdp_change, 0),
        dynamic_effect = coalesce(dynamic_effect, 0)
      )
  }

  dynamic_by_year <- dynamic_by_year %>%
    mutate(dynamic_revenue = net_revenue + dynamic_effect)

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

  results <- list(
    by_year = dynamic_by_year,
    conventional_10yr = conventional_10yr,
    dynamic_effect_10yr = dynamic_effect_10yr,
    dynamic_10yr = dynamic_10yr,
    convolution_details = convolution_results,
    growth_deviations = fy_growth_dev
  )

  return(results)
}


#' Print dynamic revenue summary
#'
#' @param dynamic_results Results from calculate_dynamic_revenue()
print_dynamic_revenue_summary <- function(dynamic_results) {

  # Print growth deviations if available
  if (!is.null(dynamic_results$growth_deviations)) {
    cat('\n----------------------------------------------------------\n')
    cat('FY GROWTH DEVIATIONS (matching Excel AI column)\n')
    cat('----------------------------------------------------------\n')
    cat(sprintf('%-6s %12s %12s\n', 'FY', 'Level Dev', 'Growth Dev'))
    cat('----------------------------------------------------------\n')

    dynamic_results$growth_deviations %>%
      filter(fiscal_year >= 2025, fiscal_year <= 2031) %>%
      pwalk(function(fiscal_year, avg_level_deviation, growth_deviation, ...) {
        cat(sprintf('%-6d %11.3f%% %11.3f%%\n',
                    fiscal_year, avg_level_deviation, growth_deviation))
      })
  }

  # Print convolution details if available
  if (!is.null(dynamic_results$convolution_details)) {
    cat('\n----------------------------------------------------------\n')
    cat('CBO CONVOLUTION RESULTS (matching Excel row 24)\n')
    cat('----------------------------------------------------------\n')
    cat(sprintf('%-6s %12s %12s\n', 'FY', 'GDP Change', 'Dyn Effect'))
    cat('----------------------------------------------------------\n')

    dynamic_results$convolution_details %>%
      filter(fiscal_year >= 2025, fiscal_year <= 2031) %>%
      pwalk(function(fiscal_year, fy_gdp_change, dynamic_effect, ...) {
        cat(sprintf('%-6d %12.1f %12.1f\n',
                    fiscal_year, fy_gdp_change, dynamic_effect))
      })
  }

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
