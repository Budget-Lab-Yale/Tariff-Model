# =============================================================================
# 03_calculate_prices.R - Calculate consumer price effects
# =============================================================================
# Calculates the impact of tariffs on consumer prices and per-household costs.
#
# Source: ricco_price_effects_and_etr sheet in Excel model (cells E24-E25)
#
# Key formula (from Excel E24):
#   price_effect = (etr * -usd_offset * goods_share * import_share) +
#                  (etr * 100) * import_share * goods_share * (1 + passthrough) / 100
#
# For post-substitution, goods_share and import_share are adjusted using
# GTAP trade flow data.

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
  # Calculate post-substitution adjustments
  # -------------------------------------------------------------------------
  # From Excel:
  #   F25 = F24 * (SUM(AG45:AG92)/AG110) / (SUM(AA45:AA92)/AA110)
  #   H25 = H24 * (SUM(AF45:AF92)/SUM(AG45:AG92)) / (SUM(Z45:Z92)/SUM(AA45:AA92))

  # Use substitution adjustment data from inputs if available
  if (!is.null(inputs$gtap_substitution)) {
    sub_data <- inputs$gtap_substitution

    required_cols <- c('baseline_total', 'postsim_total', 'baseline_row', 'postsim_row')
    missing_cols <- setdiff(required_cols, names(sub_data))
    if (length(missing_cols) > 0) {
      stop('Missing required columns in gtap_substitution: ', paste(missing_cols, collapse = ', '))
    }

    # Sum across sectors (rows 45-92 in Excel = all rows in our CSV)
    sum_baseline_total <- sum(sub_data$baseline_total)
    sum_postsim_total <- sum(sub_data$postsim_total)
    sum_baseline_row <- sum(sub_data$baseline_row)
    sum_postsim_row <- sum(sub_data$postsim_row)

    if (sum_baseline_total <= 0 || sum_postsim_total <= 0) {
      stop('Invalid gtap_substitution totals (baseline_total/postsim_total must be > 0)')
    }

    # Adjustment factors
    # Excel uses economy-wide grand totals (row 110) for goods_share adjustment.
    # Our extracted `gtap_substitution.csv` does not include those grand totals,
    # so we leave goods_share unchanged.
    goods_adjustment <- 1

    import_adjustment <- (sum_postsim_row / sum_postsim_total) /
                         (sum_baseline_row / sum_baseline_total)

    adjusted_goods_share <- goods_share * goods_adjustment
    adjusted_import_share <- import_share * import_adjustment

    message(sprintf('  Import share adjustment: %.4f', import_adjustment))

  } else {
    # No adjustment data available - use baseline values
    message('  No substitution adjustment data found, using baseline shares')
    adjusted_goods_share <- goods_share
    adjusted_import_share <- import_share
  }

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
  # Calculate per-household costs
  # -------------------------------------------------------------------------
  # From F6 Distribution sheet - uses a scaling factor based on average
  # household income/expenditure

  # Approximate scaling factor from Excel (I24 value)
  # This should be calibrated to match the distribution calculations
  HH_SCALING_FACTOR <- -135000  # Approximate $/percentage point

  pre_sub_per_hh <- pre_sub_price * HH_SCALING_FACTOR
  post_sub_per_hh <- post_sub_price * HH_SCALING_FACTOR

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  results <- list(
    # Price increases (as percentage points)
    pre_sub_price_increase = pre_sub_price * 100,
    post_sub_price_increase = post_sub_price * 100,

    # Per-household costs (negative = cost to households)
    pre_sub_per_hh_cost = pre_sub_per_hh,
    post_sub_per_hh_cost = post_sub_per_hh,

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
  message(sprintf('  Pre-sub per-HH cost: $%.0f', pre_sub_per_hh))
  message(sprintf('  Post-sub per-HH cost: $%.0f', post_sub_per_hh))

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
