# =============================================================================
# 02_calculate_etr.R - Calculate weighted effective tariff rates
# =============================================================================
# Calculates pre-substitution and post-substitution weighted ETRs based on
# country-level tariff rates and import weights from GTAP.
#
# Source: ricco_price_effects_and_etr sheet in Excel model
# Key formulas:
#   BW55 = SUMPRODUCT(BO55:BV55, BE55:BL55) / SUM(BE55:BL55)  [post-sim]
#   BW125 = SUMPRODUCT(BO125:BV125, BE125:BL125) / SUM(BE125:BL125)  [baseline]
#
# Country columns: China, Canada, Mexico, UK, Japan, EU, ROW, FTA

library(tidyverse)

# =============================================================================
# Main calculation function
# =============================================================================

#' Calculate weighted effective tariff rates
#'
#' @param inputs List containing:
#'   - gtap_postsim: Post-simulation imports and ETRs by country
#'   - baselines$gtap: Baseline imports and ETRs by country
#'   - assumptions: Global assumptions including baseline_etr
#'
#' @return List with pre/post substitution ETR results
calculate_etr <- function(inputs) {

  # -------------------------------------------------------------------------
  # Calculate weighted ETRs using aggregate country data
  # -------------------------------------------------------------------------
  # This matches the Excel BW55/BW125 SUMPRODUCT formula directly

  post_sub_etr <- calculate_weighted_etr(inputs$gtap_postsim)
  pre_sub_etr <- calculate_weighted_etr(inputs$baselines$gtap)

  # -------------------------------------------------------------------------
  # Calculate all-in ETRs (baseline + increase)
  # -------------------------------------------------------------------------

  baseline_etr <- inputs$assumptions$baseline_etr

  pre_sub_all_in <- baseline_etr + (pre_sub_etr / 100)
  post_sub_all_in <- baseline_etr + (post_sub_etr / 100)

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  results <- list(
    # Main ETR results (as percentages)
    pre_sub_increase = pre_sub_etr,
    pre_sub_all_in = pre_sub_all_in * 100,
    post_sub_increase = post_sub_etr,
    post_sub_all_in = post_sub_all_in * 100
  )

  message(sprintf('  Pre-substitution ETR increase: %.2f%%', pre_sub_etr))
  message(sprintf('  Post-substitution ETR increase: %.2f%%', post_sub_etr))

  return(results)
}


# =============================================================================
# Helper function to calculate weighted ETR from aggregate data
# =============================================================================

#' Calculate weighted ETR from country-level aggregate data
#'
#' @param data Dataframe with imports_* and etr_* columns by country
#'
#' @return Overall weighted ETR (as percentage)
calculate_weighted_etr <- function(data) {

  # Import columns
  imports <- c(
    data$imports_chn,
    data$imports_ca,
    data$imports_mx,
    data$imports_uk,
    data$imports_jp,
    data$imports_eu,
    data$imports_row,
    data$imports_fta
  )

  # ETR columns
  etrs <- c(
    data$etr_chn,
    data$etr_ca,
    data$etr_mx,
    data$etr_uk,
    data$etr_jp,
    data$etr_eu,
    data$etr_row,
    data$etr_fta
  )

  # Weighted average
  overall_etr <- sum(imports * etrs) / sum(imports)

  return(overall_etr)
}
