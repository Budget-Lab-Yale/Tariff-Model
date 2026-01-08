# =============================================================================
# 03_calculate_prices.R - Calculate consumer price effects
# =============================================================================
# Calculates the impact of tariffs on consumer prices.
#
# Source: ricco_price_effects_and_etr sheet in Excel model (cells E24-E25)
#
# Key formula (from Excel E24):
#   price_effect = (etr * -usd_offset * goods_share * import_share) +
#                  (etr * 100) * import_share * goods_share * (1 + passthrough) / 100
#
# For post-substitution, goods_share and import_share are adjusted using
# GTAP trade flow data.
#
# Note: Per-household costs are calculated separately in 09_calculate_distribution.R
# using the Excel methodology (average of per-decile costs from F6 Distribution).

library(tidyverse)

# =============================================================================
# Main calculation function
# =============================================================================

#' Calculate consumer price effects from tariff changes
#'
#' @param etr_results Results from calculate_etr()
#' @param inputs List containing assumptions and gtap_substitution data
#'
#' @return List with price effect results
calculate_prices <- function(etr_results, inputs) {

  assumptions <- inputs$assumptions

  # Extract parameters from global assumptions
  usd_offset <- assumptions$usd_offset
  passthrough <- assumptions$price_passthrough
  goods_share <- assumptions$goods_share_pce
  import_share <- assumptions$import_share

  # -------------------------------------------------------------------------
  # Calculate price effects
  # -------------------------------------------------------------------------

  # Pre-substitution price effect (uses baseline goods_share and import_share)
  pre_sub_etr <- etr_results$pre_sub_increase / 100  # Convert from percentage

  pre_sub_price <- calculate_price_effect(
    etr = pre_sub_etr,
    usd_offset = usd_offset,
    passthrough = passthrough,
    goods_share = goods_share,
    import_share = import_share
  )

  # -------------------------------------------------------------------------
  # Calculate post-substitution adjustments from VIWS data
  # -------------------------------------------------------------------------
  # Derived directly from GTAP solution files (viws) vs baseline (viws_baseline)
  # Import adjustment = total_postsim / total_baseline (overall import level change)

  viws <- inputs$viws
  viws_baseline <- inputs$baselines$viws_baseline

  if (is.null(viws) || is.null(viws_baseline)) {
    stop('viws and viws_baseline required for substitution adjustment')
  }

  # Sum across all sectors for total imports
  sum_baseline_total <- sum(viws_baseline)
  sum_postsim_total <- sum(viws)

  if (sum_baseline_total <= 0 || sum_postsim_total <= 0) {
    stop('Invalid viws totals for substitution adjustment')
  }

  # Adjustment factors from NVPP (National Value of Production and Purchases) data
  # Calculated in read_gtap.R from GTAP solution files:
  #   goods_adj = (goods_share_postsim) / (goods_share_baseline)
  #   import_adj = (import_share_postsim) / (import_share_baseline)
  # Where goods sectors are first 45 commodities (tradeable goods)

  nvpp_adjustment <- inputs$nvpp_adjustment
  if (is.null(nvpp_adjustment)) {
    stop('nvpp_adjustment required for substitution adjustment - ensure GTAP data is loaded')
  }

  goods_adjustment <- nvpp_adjustment$goods_adjustment
  import_adjustment <- nvpp_adjustment$import_adjustment

  adjusted_goods_share <- goods_share * goods_adjustment
  adjusted_import_share <- import_share * import_adjustment

  message(sprintf('  Import share adjustment: %.4f (imports at %.1f%% of baseline)',
                  import_adjustment, import_adjustment * 100))

  # Post-substitution price effect
  post_sub_etr <- etr_results$post_sub_increase / 100  # Convert from percentage

  post_sub_price <- calculate_price_effect(
    etr = post_sub_etr,
    usd_offset = usd_offset,
    passthrough = passthrough,
    goods_share = adjusted_goods_share,
    import_share = adjusted_import_share
  )

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------
  # Note: Per-household costs are calculated in 09_calculate_distribution.R
  # using the Excel methodology (average of per-decile costs from F6 Distribution).
  # This matches how Excel Key Results B13/B15 pull from F6 Distribution via
  # ricco_price_effects_and_etr!I24/I25.

  results <- list(
    # Price increases (as percentage points)
    pre_sub_price_increase = pre_sub_price * 100,
    post_sub_price_increase = post_sub_price * 100,

    # Parameters used
    params_used = list(
      usd_offset = usd_offset,
      passthrough = passthrough,
      goods_share = goods_share,
      import_share = import_share,
      adjusted_goods_share = adjusted_goods_share,
      adjusted_import_share = adjusted_import_share
    )
  )

  message(sprintf('  Pre-sub price increase: %.3f%%', pre_sub_price * 100))
  message(sprintf('  Post-sub price increase: %.3f%%', post_sub_price * 100))

  return(results)
}


# =============================================================================
# Helper functions
# =============================================================================

#' Calculate price effect using Excel formula
#'
#' Implements the formula from Excel E24:
#'   (etr * -usd_offset * goods_share * import_share) +
#'   (etr * 100) * import_share * goods_share * (1 + passthrough) / 100
#'
#' @param etr Effective tariff rate (as decimal, e.g., 0.14 for 14%)
#' @param usd_offset USD offset parameter (from B13)
#' @param passthrough Price passthrough parameter (from B14)
#' @param goods_share Goods share of PCE (from B15)
#' @param import_share Import share (from B16)
#'
#' @return Price effect (as decimal)
calculate_price_effect <- function(etr, usd_offset, passthrough, goods_share, import_share) {

  # Term 1: USD offset effect (negative - strengthening dollar reduces prices)
  term1 <- etr * (-usd_offset) * goods_share * import_share

  # Term 2: Direct tariff passthrough effect
  term2 <- (etr * 100) * import_share * goods_share * (1 + passthrough) / 100

  price_effect <- term1 + term2

  return(price_effect)
}
