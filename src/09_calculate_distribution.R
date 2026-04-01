# =============================================================================
# 09_calculate_distribution.R - Calculate distributional impacts by income decile
# =============================================================================
# Calculates per-household tariff costs by income decile, showing the regressive
# nature of tariff impacts (lower-income households pay a higher share of income).
#
# Source: F6 Distribution (C) sheet in Excel model
#
# Formula chain:
#   1. Per-decile PCE effect = aggregate_price * pce_variation
#   2. Scaled effect (% of ATI) = pce_effect * scaling_factor * 100
#   3. Dollar cost = scaled_effect * income / 100
#
# The pce_variation scalars capture per-decile price exposure (+/-5% variation
# from aggregate, derived from CBO income-decile consumption patterns).
#
# The scaling_factor captures income-relative consumption:
# - Decile 1 (poorest): 1.96 (spends 96% more than proportional share)
# - Decile 10 (richest): 0.65 (spends 35% less than proportional share)

library(tidyverse)

# =============================================================================
# Helper functions
# =============================================================================

#' Calculate distribution for given per-decile PCE effects
#'
#' @param decile_params Data frame with decile, income, scaling_factor
#' @param decile_pce Numeric vector of length 10: per-decile PCE price effect
#'   as decimal (e.g., 0.0124 for 1.24%)
#' @param inflation_factor Factor to convert 2024 dollars to 2025 dollars
#'
#' @return Data frame with pce_effect, pct_of_income, cost_per_hh columns
calc_decile_distribution <- function(decile_params, decile_pce, inflation_factor) {
  decile_params %>%
    mutate(
      income_2025 = income * inflation_factor,

      # Per-decile PCE effect
      pce_effect = decile_pce,

      # Scaled effect as percentage of after-tax income
      pct_of_income = pce_effect * scaling_factor * 100,

      # Dollar cost per household in 2025 dollars.
      cost_per_hh = -1 * (pct_of_income / 100) * income_2025,

      # Display income in 2025 dollars to match the burden outputs.
      income = income_2025
    ) %>%
    select(-income_2025)
}


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

  assert_has_columns(decile_params, c('decile', 'income', 'scaling_factor', 'pce_variation'),
                     'decile_parameters')

  # -------------------------------------------------------------------------
  # Get inflation adjustment factor (convert 2024 dollars to 2025 dollars)
  # -------------------------------------------------------------------------

  inflation_factor <- 1 + (inputs$assumptions$inflation_2024_to_2025 %||% 0)
  message(sprintf('  Inflation adjustment: %.1f%% (2024 -> 2025 dollars)',
                  (inflation_factor - 1) * 100))

  # -------------------------------------------------------------------------
  # Compute per-decile price effects
  # -------------------------------------------------------------------------

  if (is.null(price_results$pre_sub_price_increase) ||
      is.null(price_results$pe_postsub_price_increase)) {
    stop('Both pre-sub and PE post-sub price results are required for distribution calculations')
  }

  pre_sub_pce <- price_results$pre_sub_price_increase / 100
  pe_postsub_pce <- price_results$pe_postsub_price_increase / 100

  message(sprintf('  Pre-sub PCE effect: %.4f%%', pre_sub_pce * 100))
  message(sprintf('  PE post-sub PCE effect: %.4f%%', pe_postsub_pce * 100))

  pre_decile_pce <- pre_sub_pce * decile_params$pce_variation
  post_decile_pce <- pe_postsub_pce * decile_params$pce_variation

  # -------------------------------------------------------------------------
  # Calculate per-decile impacts
  # -------------------------------------------------------------------------

  pre_distribution <- calc_decile_distribution(
    decile_params,
    pre_decile_pce,
    inflation_factor
  )
  post_distribution <- calc_decile_distribution(
    decile_params,
    post_decile_pce,
    inflation_factor
  )

  # -------------------------------------------------------------------------
  # Calculate summary statistics
  # -------------------------------------------------------------------------

  # Simple average across deciles (matches Excel AVERAGE(B23:K23) in F6 Distribution)
  pre_sub_avg_cost <- mean(pre_distribution$cost_per_hh)
  post_sub_avg_cost <- mean(post_distribution$cost_per_hh)

  # Median cost (decile 5-6 midpoint approximation)
  pre_sub_median_cost <- (pre_distribution$cost_per_hh[5] + pre_distribution$cost_per_hh[6]) / 2
  post_sub_median_cost <- (post_distribution$cost_per_hh[5] + post_distribution$cost_per_hh[6]) / 2

  message(sprintf('  Pre-sub average per-HH cost: $%.0f (2025 dollars)', abs(pre_sub_avg_cost)))
  message(sprintf('  PE post-sub average per-HH cost: $%.0f (2025 dollars)', abs(post_sub_avg_cost)))

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  results <- list(
    # Distribution by decile
    by_decile = pre_distribution %>%
      select(decile, income, pct_of_income, cost_per_hh),
    by_decile_post_sub = post_distribution %>%
      select(decile, income, pct_of_income, cost_per_hh),

    # Per-HH costs (2025 dollars)
    pre_sub_per_hh_cost = pre_sub_avg_cost,
    pre_sub_median_cost = pre_sub_median_cost,
    post_sub_per_hh_cost = post_sub_avg_cost,
    post_sub_median_cost = post_sub_median_cost
  )

  # Print distribution summary
  message('\n  Distribution by decile (pre-substitution, 2025 dollars):')
  message('  Decile | Income     | % of Income | Cost/HH')
  message('  -------|------------|-------------|--------')
  for (i in 1:10) {
    message(sprintf('  %6d | $%9s | %6.2f%%     | $%s',
                    pre_distribution$decile[i],
                    format(pre_distribution$income[i], big.mark = ',', scientific = FALSE),
                    pre_distribution$pct_of_income[i],
                    format(round(pre_distribution$cost_per_hh[i]), big.mark = ',', scientific = FALSE)))
  }

  message('\n  Distribution by decile (PE post-substitution, 2025 dollars):')
  message('  Decile | Income     | % of Income | Cost/HH')
  message('  -------|------------|-------------|--------')
  for (i in 1:10) {
    message(sprintf('  %6d | $%9s | %6.2f%%     | $%s',
                    post_distribution$decile[i],
                    format(post_distribution$income[i], big.mark = ',', scientific = FALSE),
                    post_distribution$pct_of_income[i],
                    format(round(post_distribution$cost_per_hh[i]), big.mark = ',', scientific = FALSE)))
  }

  return(results)
}
