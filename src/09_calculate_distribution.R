# =============================================================================
# 09_calculate_distribution.R - Calculate distributional impacts by income decile
# =============================================================================
# Calculates per-household tariff costs by income decile, showing the regressive
# nature of tariff impacts (lower-income households pay a higher share of income).
#
# Source: F6 Distribution (C) sheet in Excel model
#
# Key insight: Lower-income households spend a higher proportion of their income
# on tariffed goods (food, apparel, basic electronics), so they bear a
# disproportionate burden as a percentage of income.
#
# Formula chain:
#   1. Per-decile PCE effect = base_pce × pce_variation
#   2. Scaled effect (% of ATI) = pce_effect × scaling_factor × 100
#   3. Dollar cost = scaled_effect × income / 100
#
# The pce_variation factors capture consumption basket differences:
# - Derived from Consumer Expenditure Survey data
# - Values range from 0.94 to 1.04 (±5% variation from average)
# - Higher values mean that decile consumes more tariffed goods
#
# The scaling_factor captures income-relative consumption:
# - Decile 1 (poorest): 1.96 (spends 96% more than proportional share)
# - Decile 10 (richest): 0.65 (spends 35% less than proportional share)

library(tidyverse)

# =============================================================================
# Main calculation function
# =============================================================================

#' Calculate distributional impacts of tariffs by income decile
#'
#' @param price_results Results from calculate_prices()
#' @param inputs List containing decile_parameters
#'
#' @return List with distribution results
calculate_distribution <- function(price_results, inputs) {

  # Load decile parameters
  decile_params <- inputs$decile_parameters

  if (is.null(decile_params)) {
    stop('decile_parameters not found in inputs. Load from resources/distribution/decile_parameters.csv')
  }

  required_cols <- c('decile', 'income', 'scaling_factor', 'pce_variation')
  missing_cols <- setdiff(required_cols, names(decile_params))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in decile_parameters: ', paste(missing_cols, collapse = ', '))
  }

  # -------------------------------------------------------------------------
  # Get base price effect from actual tariff calculation
  # -------------------------------------------------------------------------

  base_pce <- price_results$pre_sub_price_increase / 100  # Convert to decimal

  message(sprintf('  Base PCE effect from tariff calc: %.4f%%', base_pce * 100))

  # -------------------------------------------------------------------------
  # Calculate per-decile impacts using dynamic formula
  # -------------------------------------------------------------------------
  # Formula: PCE_d = base_pce × pce_variation_d
  #          Cost_d = PCE_d × scaling_factor_d × income_d

  # Dynamic calculation using variation factors
  distribution <- decile_params %>%
    mutate(
      # Per-decile PCE effect (varies by consumption basket)
      pce_effect = base_pce * pce_variation,

      # Scaled effect as percentage of after-tax income
      pct_of_income = pce_effect * scaling_factor * 100,

      # Dollar cost per household (negative = cost to household)
      cost_per_hh = -1 * (pct_of_income / 100) * income
    )
  message('  Using dynamic calculation: base_pce × pce_variation × scaling_factor')

  # -------------------------------------------------------------------------
  # Calculate summary statistics
  # -------------------------------------------------------------------------

  # Simple average across deciles (matches Excel AVERAGE(B23:K23))
  avg_cost <- mean(distribution$cost_per_hh)

  # Median cost (decile 5-6 midpoint approximation)
  median_cost <- (distribution$cost_per_hh[5] + distribution$cost_per_hh[6]) / 2

  message(sprintf('  Average per-HH cost: $%.0f', avg_cost))
  message(sprintf('  Median per-HH cost: $%.0f', median_cost))

  # -------------------------------------------------------------------------
  # Calculate post-substitution distribution
  # -------------------------------------------------------------------------

  post_sub_pce <- price_results$post_sub_price_increase / 100

  distribution_post_sub <- decile_params %>%
    mutate(
      pce_effect = post_sub_pce * pce_variation,
      pct_of_income = pce_effect * scaling_factor * 100,
      cost_per_hh = -1 * (pct_of_income / 100) * income
    )

  avg_cost_post_sub <- mean(distribution_post_sub$cost_per_hh)

  message(sprintf('  Post-sub average per-HH cost: $%.0f', avg_cost_post_sub))

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  results <- list(
    # Pre-substitution distribution
    pre_sub = list(
      by_decile = distribution %>%
        select(decile, income, pct_of_income, cost_per_hh),
      avg_per_hh_cost = avg_cost,
      median_per_hh_cost = median_cost
    ),

    # Post-substitution distribution
    post_sub = list(
      by_decile = distribution_post_sub %>%
        select(decile, income, pct_of_income, cost_per_hh),
      avg_per_hh_cost = avg_cost_post_sub
    ),

    # Regressivity measures
    regressivity = list(
      # Ratio of decile 1 burden to decile 10 burden (as % of income)
      burden_ratio = distribution$pct_of_income[1] / distribution$pct_of_income[10],
      # Difference in percentage points
      burden_diff_pp = distribution$pct_of_income[1] - distribution$pct_of_income[10]
    )
  )

  # Print distribution summary
  message('\n  Distribution by decile (pre-substitution):')
  message('  Decile | Income     | % of Income | Cost/HH')
  message('  -------|------------|-------------|--------')
  for (i in 1:10) {
    message(sprintf('  %6d | $%9s | %6.2f%%     | $%s',
                    distribution$decile[i],
                    format(distribution$income[i], big.mark = ',', scientific = FALSE),
                    distribution$pct_of_income[i],
                    format(round(distribution$cost_per_hh[i]), big.mark = ',', scientific = FALSE)))
  }

  message(sprintf('\n  Regressivity: Decile 1 pays %.1fx the rate of Decile 10',
                  results$regressivity$burden_ratio))

  return(results)
}


# =============================================================================
# Validation function
# =============================================================================

#' Validate distribution results against Excel model
#'
#' @param results Distribution results from calculate_distribution()
#' @param expected_avg Expected average per-HH cost (default: $1,671)
#'
#' @return TRUE if validation passes, FALSE otherwise
validate_distribution <- function(results, expected_avg = 1671) {
  actual_avg <- abs(results$pre_sub$avg_per_hh_cost)
  pct_diff <- abs(actual_avg - expected_avg) / expected_avg * 100

  if (pct_diff > 5) {
    warning(sprintf('Distribution average ($%.0f) differs from expected ($%.0f) by %.1f%%',
                    actual_avg, expected_avg, pct_diff))
    return(FALSE)
  }

  message(sprintf('  Validation: Average cost $%.0f matches expected $%.0f (%.1f%% diff)',
                  actual_avg, expected_avg, pct_diff))
  return(TRUE)
}
