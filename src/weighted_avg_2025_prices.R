# =============================================================================
# weighted_avg_2025_prices.R - Day-weighted average 2025 tariff price effects
# =============================================================================
#
# Computes the price impact of the actual 2025 tariff regime by day-weighting
# the time-varying ETR deltas across the calendar year. Feeds the weighted-
# average tau_M through the Boston Fed I-O price model.
#
# Usage:
#   Rscript src/weighted_avg_2025_prices.R [scenario] [--markup <type>] [--start YYYY-MM-DD]
#
# Defaults:
#   scenario = 2026-04-02 (has full 2025 date history)
#   markup   = average (mean of constant_dollar and constant_percentage)
#   start    = 2025-01-01 (beginning of calendar year)
#
# =============================================================================

library(tidyverse)
library(yaml)

# Source the I-O price model functions
source('src/io_price_model.R')

# ==== Parse CLI args =========================================================

args <- commandArgs(trailingOnly = TRUE)
scenario <- '2026-04-02'
markup_assumption <- 'average'
start_date <- '2025-01-01'

i <- 1
while (i <= length(args)) {
  if (args[i] == '--markup' && i < length(args)) {
    markup_assumption <- args[i + 1]
    i <- i + 2
  } else if (args[i] == '--start' && i < length(args)) {
    start_date <- args[i + 1]
    i <- i + 2
  } else if (!startsWith(args[i], '--')) {
    scenario <- args[i]
    i <- i + 1
  } else {
    i <- i + 1
  }
}

message(sprintf('Scenario: %s', scenario))
message(sprintf('Markup assumption: %s', markup_assumption))
message(sprintf('Start date: %s', start_date))

# ==== Load global assumptions ================================================

assumptions <- read_yaml('config/global_assumptions.yaml')
bea_io_level <- assumptions$bea_io_level %||% 'summary'
io_data_dir <- resolve_io_data_dir(bea_io_level)
message(sprintf('BEA I-O level: %s (%s)', bea_io_level, io_data_dir))

# ==== Read time-varying BEA deltas ==========================================

bea_deltas_file <- file.path('output', scenario, 'tariff_etrs',
                              'bea_deltas_by_commodity.csv')
if (!file.exists(bea_deltas_file)) {
  stop('BEA deltas file not found: ', bea_deltas_file,
       '\n  Run the full model for scenario "', scenario, '" first')
}

bea_deltas_raw <- read_csv(bea_deltas_file, show_col_types = FALSE)

if (!'date' %in% names(bea_deltas_raw)) {
  stop('BEA deltas file has no date column — not a time-varying scenario')
}

bea_deltas <- bea_deltas_raw %>%
  mutate(
    date = as.Date(date),
    valid_from = as.Date(valid_from),
    valid_until = as.Date(valid_until)
  )

n_dates <- length(unique(bea_deltas$date))
date_range <- range(bea_deltas$valid_from)
message(sprintf('Loaded %d date regimes: %s to %s',
                n_dates, date_range[1], date_range[2]))

# ==== Day-weight across 2025 ================================================

YEAR_START <- as.Date(start_date)
YEAR_END <- as.Date('2025-12-31')
TOTAL_DAYS <- as.numeric(YEAR_END - YEAR_START) + 1  # 365

# Filter to regimes that overlap with 2025
deltas_2025 <- bea_deltas %>%
  filter(valid_from <= YEAR_END, valid_until >= YEAR_START) %>%
  mutate(
    clipped_from = pmax(valid_from, YEAR_START),
    clipped_until = pmin(valid_until, YEAR_END),
    days = as.numeric(clipped_until - clipped_from) + 1
  )

# The first tariff date may not be Jan 1 — fill in baseline (delta=0) period
first_tariff_date <- min(deltas_2025$valid_from)
baseline_days <- as.numeric(first_tariff_date - YEAR_START)
message(sprintf('First tariff date: %s (baseline period: %d days at delta=0)',
                first_tariff_date, baseline_days))

# Verify day coverage
covered_days <- deltas_2025 %>%
  distinct(clipped_from, clipped_until, days) %>%
  pull(days) %>%
  sum()

message(sprintf('Covered tariff days: %d / %d (+ %d baseline = %d total)',
                covered_days, TOTAL_DAYS, baseline_days,
                covered_days + baseline_days))

# Compute weighted average etr_delta for each BEA commodity
# Include baseline days at delta=0 in the denominator
tau_M_weighted <- deltas_2025 %>%
  group_by(bea_code) %>%
  summarise(
    weighted_delta = sum(etr_delta * days) / TOTAL_DAYS,
    max_delta = max(etr_delta),
    min_delta = min(etr_delta),
    n_regimes = n(),
    .groups = 'drop'
  )

message(sprintf('\nWeighted average tau_M (2025):'))
message(sprintf('  %d BEA commodities', nrow(tau_M_weighted)))
message(sprintf('  Mean weighted delta: %.4f%% (range: %.4f%% to %.4f%%)',
                mean(tau_M_weighted$weighted_delta) * 100,
                min(tau_M_weighted$weighted_delta) * 100,
                max(tau_M_weighted$weighted_delta) * 100))

# Build named tau_M vector
tau_M <- setNames(tau_M_weighted$weighted_delta, tau_M_weighted$bea_code)

# Disaggregate to detail if needed
if (bea_io_level == 'detail') {
  tau_M <- disaggregate_tau_M(tau_M, io_data_dir)
}

# ==== Load I-O matrices =====================================================

message('\nLoading I-O matrices...')
use_import <- load_bea_use_data('import', io_data_dir)
use_domestic <- load_bea_use_data('domestic', io_data_dir)
industry_output <- load_bea_industry_output(io_data_dir)
commodity_use_totals <- load_bea_commodity_use_totals(io_data_dir)
industry_variable_cost <- load_bea_industry_variable_cost(io_data_dir)
leontief_domestic <- load_bea_requirements('domestic', io_data_dir)
pce_bridge <- load_pce_bridge(io_data_dir)

# ==== Compute prices for each markup assumption =============================

compute_for_markup <- function(ma) {
  message(sprintf('\n--- %s markup ---', ma))

  io_matrices <- build_io_matrices(
    use_import = use_import,
    use_domestic = use_domestic,
    industry_output = industry_output,
    markup_assumption = ma,
    commodity_use_totals = commodity_use_totals,
    industry_variable_cost = industry_variable_cost
  )

  result <- compute_io_prices(
    tau_M = tau_M,
    B_MD = io_matrices$B_MD,
    leontief_domestic = leontief_domestic,
    omega_M = io_matrices$omega_M,
    omega_D = io_matrices$omega_D,
    pce_bridge = pce_bridge,
    usd_offset = assumptions$usd_offset,
    domestic_pricing = assumptions$domestic_pricing,
    markup_assumption = ma
  )

  return(result)
}

if (markup_assumption == 'average') {
  result_cp <- compute_for_markup('constant_percentage')
  result_cd <- compute_for_markup('constant_dollar')

  avg_aggregate <- (result_cp$aggregate + result_cd$aggregate) / 2
  avg_direct <- (result_cp$direct_aggregate + result_cd$direct_aggregate) / 2
  avg_supply <- (result_cp$supply_chain_aggregate + result_cd$supply_chain_aggregate) / 2
} else {
  result <- compute_for_markup(markup_assumption)
  avg_aggregate <- result$aggregate
  avg_direct <- result$direct_aggregate
  avg_supply <- result$supply_chain_aggregate
}

# ==== Report results =========================================================

message('\n=============================================================================')
message('Day-Weighted Average 2025 Tariff Price Effects (Pre-Substitution)')
message('=============================================================================')
message(sprintf('  Scenario:         %s', scenario))
message(sprintf('  Markup:           %s', markup_assumption))
message(sprintf('  USD offset:       %.1f%%', assumptions$usd_offset * 100))
message(sprintf('  Domestic pricing: %.0f%%', assumptions$domestic_pricing * 100))
message(sprintf('  Year:             2025 (%d days)', TOTAL_DAYS))
message(sprintf('  Baseline days:    %d (Jan 1 to %s)',
                baseline_days, first_tariff_date - 1))
message('')
message(sprintf('  Aggregate PCE price effect:  %.3f pp', avg_aggregate * 100))
message(sprintf('    Direct (imported goods):   %.3f pp', avg_direct * 100))
message(sprintf('    Supply chain (intermediates): %.3f pp', avg_supply * 100))
message('')

if (markup_assumption == 'average') {
  message(sprintf('  Constant-percentage (upper): %.3f pp', result_cp$aggregate * 100))
  message(sprintf('  Constant-dollar (lower):     %.3f pp', result_cd$aggregate * 100))
  message('')
}

# Show top 10 most-affected PCE categories
if (markup_assumption == 'average') {
  pce_avg <- result_cp$pce_category_prices %>%
    left_join(
      result_cd$pce_category_prices %>% select(nipa_line, sr_price_effect_cd = sr_price_effect),
      by = 'nipa_line'
    ) %>%
    mutate(sr_price_effect = (sr_price_effect + sr_price_effect_cd) / 2) %>%
    select(-sr_price_effect_cd)
} else {
  pce_avg <- result$pce_category_prices
}

message('Top 15 most-affected PCE categories (pp):')
top_cats <- pce_avg %>%
  arrange(desc(sr_price_effect)) %>%
  head(15)

for (i in seq_len(nrow(top_cats))) {
  row <- top_cats[i, ]
  message(sprintf('  %+.3f pp  %s (line %d, %.1f%% of PCE)',
                  row$sr_price_effect, row$pce_category, row$nipa_line, row$pce_share))
}

message('\n=============================================================================')
