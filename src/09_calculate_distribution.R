# =============================================================================
# 09_calculate_distribution.R - Calculate distributional impacts by income decile
# =============================================================================
# Calculates per-household tariff costs by income decile, showing the regressive
# nature of tariff impacts (lower-income households pay a higher share of income).
#
# Source: F6 Distribution (C) sheet in Excel model
#
# Two modes:
#   1. Category-level (preferred): Uses 76 NIPA PCE category prices from the
#      Boston Fed I-O model, aggregated to 16 BLS distributional PCE buckets,
#      then weighted by actual decile spending shares from BLS data.
#   2. Aggregate fallback: Applies a single PCE effect with pce_variation
#      scalars (±5% variation). Used when BLS distributional data is missing.
#
# Formula chain (category-level):
#   1. Aggregate 76 NIPA categories -> 16 distributional buckets (PCE-weighted)
#   2. Per-decile price = sum(share_d_b * spend_b * price_b) / sum(share_d_b * spend_b)
#   3. Scaled effect (% of ATI) = price_d × scaling_factor × 100
#   4. Dollar cost = scaled_effect × income / 100
#
# The scaling_factor captures income-relative consumption:
# - Decile 1 (poorest): 1.96 (spends 96% more than proportional share)
# - Decile 10 (richest): 0.65 (spends 35% less than proportional share)

library(tidyverse)

# =============================================================================
# Helper functions
# =============================================================================

#' Compute per-decile price effects from category-level prices
#'
#' Aggregates 76 NIPA PCE categories into 16 BLS distributional buckets, then
#' computes a consumption-weighted price effect for each income decile using
#' BLS distributional PCE shares.
#'
#' @param pce_category_prices Tibble from Boston Fed model with nipa_line,
#'   sr_price_effect (pp), purchasers_value
#' @param nipa_to_bucket Tibble mapping nipa_line -> pce_mp (bucket code)
#' @param distributional_pce Tibble with pce_mp, total_billions, d1..d10
#'   (decile shares as percentages, e.g. 6.96 = 6.96%)
#'
#' @return Named numeric vector of length 10 (per-decile price effects as
#'   decimals, e.g. 0.0124 = 1.24%)
calc_decile_prices <- function(pce_category_prices, nipa_to_bucket,
                               distributional_pce) {

  # ---- Step 1: Aggregate 76 NIPA categories -> 16 distributional buckets ----
  bucket_prices <- pce_category_prices %>%
    inner_join(nipa_to_bucket, by = 'nipa_line') %>%
    group_by(pce_mp, bucket_name) %>%
    summarise(
      bucket_price_pp = sum(sr_price_effect * purchasers_value) / sum(purchasers_value),
      bucket_pce = sum(purchasers_value),
      n_categories = n(),
      .groups = 'drop'
    )

  # Verify all categories matched
  n_matched <- sum(pce_category_prices$nipa_line %in% nipa_to_bucket$nipa_line)
  n_total <- nrow(pce_category_prices)
  if (n_matched < n_total) {
    warning(sprintf('  %d of %d NIPA categories not in bucket mapping',
                    n_total - n_matched, n_total))
  }

  # ---- Step 2: Join with distributional shares ----
  combined <- distributional_pce %>%
    inner_join(bucket_prices, by = 'pce_mp')

  n_buckets_matched <- nrow(combined)
  n_buckets_total <- nrow(distributional_pce)
  if (n_buckets_matched < n_buckets_total) {
    message(sprintf('  Note: %d of %d distributional buckets have price data',
                    n_buckets_matched, n_buckets_total))
  }

  # ---- Step 3: Compute per-decile weighted price effects ----
  decile_prices <- numeric(10)
  names(decile_prices) <- paste0('d', 1:10)

  for (d in 1:10) {
    col <- paste0('d', d)
    shares <- combined[[col]] / 100  # Convert pct to fraction
    spending <- shares * combined$total_billions
    decile_prices[d] <- sum(spending * combined$bucket_price_pp) / sum(spending)
  }
  # decile_prices is in pp — convert to decimal
  decile_prices <- decile_prices / 100

  # ---- Diagnostics ----
  message('  Per-bucket price effects (pp):')
  for (i in 1:nrow(bucket_prices)) {
    message(sprintf('    %-50s %6.3f%%  ($%sB PCE)',
                    substr(bucket_prices$bucket_name[i], 1, 50),
                    bucket_prices$bucket_price_pp[i],
                    format(round(bucket_prices$bucket_pce[i] / 1000), big.mark = ',')))
  }
  message('  Per-decile weighted price effects:')
  for (d in 1:10) {
    message(sprintf('    D%d: %.4f%%', d, decile_prices[d] * 100))
  }

  return(decile_prices)
}


#' Calculate distribution for given per-decile PCE effects
#'
#' @param decile_params Data frame with decile, income, scaling_factor
#' @param decile_pce Numeric vector of length 10: per-decile PCE price effect
#'   as decimal (e.g., 0.0124 for 1.24%)
#' @param inflation_factor Factor to inflate income for display (e.g., 1.025)
#'
#' @return Data frame with pce_effect, pct_of_income, cost_per_hh columns
calc_decile_distribution <- function(decile_params, decile_pce, inflation_factor) {
  decile_params %>%
    mutate(
      # Per-decile PCE effect (already varies by consumption basket)
      pce_effect = decile_pce,

      # Scaled effect as percentage of after-tax income
      pct_of_income = pce_effect * scaling_factor * 100,

      # Dollar cost per household (calculated on original income, before inflation)
      cost_per_hh = -1 * (pct_of_income / 100) * income,

      # Inflate income from 2024 to 2025 dollars for display only
      income = income * inflation_factor
    )
}

# =============================================================================
# Main calculation function
# =============================================================================

#' Calculate distributional impacts of tariffs by income decile
#'
#' @param price_results Results from calculate_prices()
#' @param inputs List containing decile_parameters, and optionally
#'   distributional_pce and nipa_to_bucket for category-level weighting
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
  # Get base price effects from actual tariff calculation (pre and post-sub)
  # -------------------------------------------------------------------------

  pre_sub_pce <- price_results$pre_sub_price_increase / 100  # Convert to decimal
  post_sub_pce <- price_results$post_sub_price_increase / 100  # Convert to decimal

  message(sprintf('  Pre-sub PCE effect: %.4f%%', pre_sub_pce * 100))
  message(sprintf('  Post-sub PCE effect: %.4f%%', post_sub_pce * 100))

  # -------------------------------------------------------------------------
  # Compute per-decile price effects
  # -------------------------------------------------------------------------
  # Category-level: use 76 NIPA prices -> 16 BLS buckets -> decile weighting
  # Fallback: aggregate × pce_variation (original approach)

  use_category_level <- !is.null(inputs$distributional_pce) &&
                        !is.null(inputs$nipa_to_bucket) &&
                        !is.null(price_results$presub$pce_category_prices)

  if (use_category_level) {
    message('  Computing category-level distributional effects (16 BLS buckets):')

    message('  Pre-substitution:')
    pre_decile_pce <- calc_decile_prices(
      price_results$presub$pce_category_prices,
      inputs$nipa_to_bucket,
      inputs$distributional_pce
    )

    message('  Post-substitution:')
    post_decile_pce <- calc_decile_prices(
      price_results$postsub$pce_category_prices,
      inputs$nipa_to_bucket,
      inputs$distributional_pce
    )

    message('  Using category-level distributional PCE (BLS 2023)')
  } else {
    pre_decile_pce <- pre_sub_pce * decile_params$pce_variation
    post_decile_pce <- post_sub_pce * decile_params$pce_variation
    message('  Using aggregate pce_variation fallback (no BLS distributional data)')
  }

  # -------------------------------------------------------------------------
  # Calculate per-decile impacts
  # -------------------------------------------------------------------------

  distribution <- calc_decile_distribution(decile_params, pre_decile_pce, inflation_factor)
  distribution_post <- calc_decile_distribution(decile_params, post_decile_pce, inflation_factor)

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

  message(sprintf('  Pre-sub average per-HH cost: $%.0f (2025 dollars)', pre_sub_avg_cost))
  message(sprintf('  Post-sub average per-HH cost: $%.0f (2025 dollars)', post_sub_avg_cost))

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  results <- list(
    # Distribution by decile (pre-substitution, for detailed output)
    by_decile = distribution %>%
      select(decile, income, pct_of_income, cost_per_hh),

    # Post-substitution by decile
    by_decile_post = distribution_post %>%
      select(decile, income, pct_of_income, cost_per_hh),

    # Pre-substitution per-HH costs (matches Excel Key Results B13)
    pre_sub_per_hh_cost = pre_sub_avg_cost,
    pre_sub_median_cost = pre_sub_median_cost,

    # Post-substitution per-HH costs (matches Excel Key Results B15)
    post_sub_per_hh_cost = post_sub_avg_cost,
    post_sub_median_cost = post_sub_median_cost,

    # Legacy field for backwards compatibility
    avg_per_hh_cost = pre_sub_avg_cost,
    median_per_hh_cost = pre_sub_median_cost,

    # Method used
    method = if (use_category_level) 'category_level' else 'aggregate_fallback'
  )

  # Print distribution summary
  message('\n  Distribution by decile (pre-substitution, 2025 dollars):')
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
