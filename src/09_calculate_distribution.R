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
  # Get base price effects from actual tariff calculation (pre and post-sub)
  # -------------------------------------------------------------------------

  pre_sub_pce <- price_results$pre_sub_price_increase / 100  # Convert to decimal
  post_sub_pce <- price_results$post_sub_price_increase / 100  # Convert to decimal

  message(sprintf('  Pre-sub PCE effect: %.4f%%', pre_sub_pce * 100))
  message(sprintf('  Post-sub PCE effect: %.4f%%', post_sub_pce * 100))

  # -------------------------------------------------------------------------
  # Calculate per-decile impacts using dynamic formula
  # -------------------------------------------------------------------------
  # Formula: PCE_d = base_pce × pce_variation_d
  #          Cost_d = PCE_d × scaling_factor_d × income_d

  # Dynamic calculation using variation factors (pre-substitution)
  distribution <- decile_params %>%
    mutate(
      # Per-decile PCE effect (varies by consumption basket)
      pce_effect = pre_sub_pce * pce_variation,

      # Scaled effect as percentage of after-tax income
      pct_of_income = pce_effect * scaling_factor * 100,

      # Dollar cost per household (negative = cost to household)
      cost_per_hh = -1 * (pct_of_income / 100) * income
    )

  # Post-substitution distribution (same formula, different base PCE)
  distribution_post <- decile_params %>%
    mutate(
      pce_effect = post_sub_pce * pce_variation,
      pct_of_income = pce_effect * scaling_factor * 100,
      cost_per_hh = -1 * (pct_of_income / 100) * income
    )

  message('  Using dynamic calculation: base_pce × pce_variation × scaling_factor')

  # -------------------------------------------------------------------------
  # Calculate summary statistics
  # -------------------------------------------------------------------------

  # Simple average across deciles (matches Excel AVERAGE(B23:K23) in F6 Distribution)
  # This is what Key Results per-HH cost should use (via ricco_price_effects_and_etr!I24)
  pre_sub_avg_cost <- mean(distribution$cost_per_hh)
  post_sub_avg_cost <- mean(distribution_post$cost_per_hh)

  # Median cost (decile 5-6 midpoint approximation)
  pre_sub_median_cost <- (distribution$cost_per_hh[5] + distribution$cost_per_hh[6]) / 2
  post_sub_median_cost <- (distribution_post$cost_per_hh[5] + distribution_post$cost_per_hh[6]) / 2

  message(sprintf('  Pre-sub average per-HH cost: $%.0f', pre_sub_avg_cost))
  message(sprintf('  Post-sub average per-HH cost: $%.0f', post_sub_avg_cost))

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  results <- list(
    # Distribution by decile (pre-substitution, for detailed output)
    by_decile = distribution %>%
      select(decile, income, pct_of_income, cost_per_hh),

    # Pre-substitution per-HH costs (matches Excel Key Results B13)
    pre_sub_per_hh_cost = pre_sub_avg_cost,
    pre_sub_median_cost = pre_sub_median_cost,

    # Post-substitution per-HH costs (matches Excel Key Results B15)
    post_sub_per_hh_cost = post_sub_avg_cost,
    post_sub_median_cost = post_sub_median_cost,

    # Legacy field for backwards compatibility
    avg_per_hh_cost = pre_sub_avg_cost,
    median_per_hh_cost = pre_sub_median_cost
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
  actual_avg <- abs(results$avg_per_hh_cost)
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
