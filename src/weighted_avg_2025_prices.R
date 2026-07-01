#!/usr/bin/env Rscript
# =============================================================================
# weighted_avg_2025_prices.R - Day-weighted average 2025 tariff price effects
# =============================================================================
#
# Computes the consumer price impact of the actual 2025 tariff regime by
# day-weighting the time-varying BEA ETR deltas across a window (default
# April 2 - December 31, 2025, the one-year-retrospective window) and pushing
# the weighted-average tau_M through the Boston Fed I-O price model. Reports the
# PRE-SUBSTITUTION aggregate PCE price effect -- the "modeling updates" input to
# the Figure-4 price waterfall (tariff-retrospective / tariff-update-blog).
#
# This is a SIDE SCRIPT: it reads a scenario's ALREADY-PUBLISHED rate inputs from
# the shared interface tree (never the repo checkout), so run the scenario first.
#
#   <root>/model_data/Tariff-Model/v<ver>/<vintage>/<scenario>/rate_inputs/
#     bea_deltas_by_commodity.csv    (a) statutory basket      - eta OFF
#     bea_deltas_by_commodity_b.csv  (b) eta'-adjusted basket  - eta ON
#
# A single published run therefore yields BOTH the eta-off and eta-on
# annual-average pre-sub price effects; their difference is Figure 4's
# "apply etas (compliance gap)" step.
#
# Usage:
#   Rscript src/weighted_avg_2025_prices.R <scenario> --vintage <id> \
#     [--markup <type>] [--start YYYY-MM-DD] [--end YYYY-MM-DD] [--out <path>] [--write-local]
#
# Defaults: markup = constant_dollar (matches run.R), start = 2025-04-02,
#           end = 2025-12-31.
#
# Besides the console report, writes a summary CSV (one row per basket, with the
# aggregate/direct/supply-chain pre-sub price effects) to --out, defaulting to
# <scenario dir>/weighted_avg_2025_prices.csv in the interface tree. The eta-on
# (b) aggregate is the Figure-4 "modeling updates" (bar 4) input.
#
# SCHEMA NOTE: 00a now writes the time-varying delta file with a single `date`
# column (no valid_from/valid_until). Regime intervals are reconstructed from
# consecutive snapshot dates: the rate at date d_i is in force over [d_i, d_i+1).
# Window days before the first in-window regime are treated as baseline
# (delta = 0) -- they enter the TOTAL_DAYS denominator but not the numerator,
# matching the original side script's day-weighting.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
})

# Sourcing paths.R also sources interfaces.R (read_interface_config,
# tariff_output_root); helpers.R provides `%||%`; io_price_model.R the pricing.
source('src/helpers.R')
source('src/paths.R')
source('src/io_price_model.R')

# ==== Parse CLI args =========================================================

args <- commandArgs(trailingOnly = TRUE)
scenario <- NA_character_
vintage <- NA_character_
markup_assumption <- 'constant_dollar'
start_date <- '2025-04-02'
end_date <- '2025-12-31'
write_local <- FALSE
out_file <- NA_character_

i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a == '--vintage' && i < length(args)) {
    vintage <- args[i + 1]; i <- i + 2
  } else if (a == '--markup' && i < length(args)) {
    markup_assumption <- args[i + 1]; i <- i + 2
  } else if (a == '--start' && i < length(args)) {
    start_date <- args[i + 1]; i <- i + 2
  } else if (a == '--end' && i < length(args)) {
    end_date <- args[i + 1]; i <- i + 2
  } else if (a == '--out' && i < length(args)) {
    out_file <- args[i + 1]; i <- i + 2
  } else if (a == '--write-local') {
    write_local <- TRUE; i <- i + 1
  } else if (!startsWith(a, '--')) {
    scenario <- a; i <- i + 1
  } else {
    i <- i + 1
  }
}

if (is.na(scenario)) {
  stop('scenario is required: Rscript src/weighted_avg_2025_prices.R <scenario> --vintage <id>')
}
if (is.na(vintage)) {
  stop('--vintage <id> is required (which published run to read, e.g. the run vintage under v<ver>/)')
}

message(sprintf('Scenario: %s', scenario))
message(sprintf('Vintage:  %s', vintage))
message(sprintf('Markup:   %s', markup_assumption))
message(sprintf('Window:   %s to %s', start_date, end_date))

# ==== Resolve the published run's rate_inputs dir (interface tree) ============

cfg <- read_interface_config()
output_root <- tariff_output_root(vintage, write_local = write_local, cfg = cfg)
if (is.null(output_root)) {
  stop('No output root configured in config/interfaces/output_roots.yaml')
}
set_run_output_root(output_root)
rate_inputs_dir <- file.path(run_scenario_dir(scenario), 'rate_inputs')
if (!dir.exists(rate_inputs_dir)) {
  stop('rate_inputs not found for this scenario/vintage:\n  ', rate_inputs_dir,
       '\n  Run the scenario at this vintage first (e.g. Rscript run.R ', scenario,
       ' --vintage ', vintage, ').')
}
message(sprintf('Rate inputs: %s', rate_inputs_dir))

# Default the summary CSV next to the run's other outputs in the interface tree
# (never a repo-local dir); override with --out.
if (is.na(out_file)) {
  out_file <- file.path(run_scenario_dir(scenario), 'weighted_avg_2025_prices.csv')
}

# ==== Global assumptions + I-O level =========================================

assumptions <- read_yaml('config/global_assumptions.yaml')
bea_io_level <- assumptions$bea_io_level %||% 'summary'
io_data_dir <- resolve_io_data_dir(bea_io_level)
message(sprintf('BEA I-O level: %s (%s)', bea_io_level, io_data_dir))

# ==== Load I-O matrices once (shared across baskets) =========================

message('\nLoading I-O matrices...')
use_import <- load_bea_use_data('import', io_data_dir)
use_domestic <- load_bea_use_data('domestic', io_data_dir)
industry_output <- load_bea_industry_output(io_data_dir)
commodity_use_totals <- load_bea_commodity_use_totals(io_data_dir)
industry_variable_cost <- load_bea_industry_variable_cost(io_data_dir)
leontief_domestic <- load_bea_requirements('domestic', io_data_dir)
pce_bridge <- load_pce_bridge(io_data_dir)

# ==== Day-weight one delta file to a tau_M vector ============================

WIN_START <- as.Date(start_date)
WIN_END <- as.Date(end_date)
TOTAL_DAYS <- as.numeric(WIN_END - WIN_START) + 1

day_weight_tau_M <- function(delta_file, label) {
  if (!file.exists(delta_file)) return(NULL)
  raw <- read_csv(delta_file, show_col_types = FALSE)
  if (!'date' %in% names(raw)) {
    stop(label, ': no `date` column -- not a time-varying run, cannot day-weight (',
         delta_file, ')')
  }
  raw <- raw %>% mutate(date = as.Date(date))

  # Reconstruct regime intervals from consecutive snapshot dates: date d_i is in
  # force over [d_i, d_i+1); valid_until = next date - 1 (inclusive), and the
  # last date's regime persists to the window end.
  dates <- sort(unique(raw$date))
  intervals <- tibble(
    date = dates,
    valid_from = dates,
    valid_until = c(dates[-1] - 1, WIN_END)
  )

  in_window <- intervals %>%
    filter(valid_from <= WIN_END, valid_until >= WIN_START) %>%
    mutate(
      clipped_from = pmax(valid_from, WIN_START),
      clipped_until = pmin(valid_until, WIN_END),
      days = as.numeric(clipped_until - clipped_from) + 1
    )

  # Baseline (delta = 0) days before the first in-window regime: implicit in the
  # TOTAL_DAYS denominator, reported here for transparency.
  covered_days <- sum(in_window$days)
  baseline_days <- TOTAL_DAYS - covered_days

  weighted <- raw %>%
    inner_join(in_window %>% select(date, days), by = 'date') %>%
    group_by(bea_code) %>%
    summarise(weighted_delta = sum(etr_delta * days) / TOTAL_DAYS, .groups = 'drop')

  message(sprintf(
    '  [%s] %d BEA commodities | %d regime dates | covered %d/%d days (baseline %d at delta=0) | mean weighted delta %.4f%%',
    label, nrow(weighted), length(dates), covered_days, TOTAL_DAYS, baseline_days,
    mean(weighted$weighted_delta) * 100))

  mean_delta_pct <- mean(weighted$weighted_delta) * 100
  tau_M <- setNames(weighted$weighted_delta, weighted$bea_code)
  if (bea_io_level == 'detail') tau_M <- disaggregate_tau_M(tau_M, io_data_dir)
  list(tau_M = tau_M, mean_delta_pct = mean_delta_pct,
       n_commodities = nrow(weighted), n_regime_dates = length(dates),
       covered_days = covered_days, baseline_days = baseline_days)
}

# ==== Pre-sub price effect for a tau_M (single markup, or averaged) ==========

compute_presub <- function(tau_M) {
  one <- function(ma) {
    m <- build_io_matrices(
      use_import = use_import,
      use_domestic = use_domestic,
      industry_output = industry_output,
      markup_assumption = ma,
      commodity_use_totals = commodity_use_totals,
      industry_variable_cost = industry_variable_cost
    )
    compute_io_prices(
      tau_M = tau_M,
      B_MD = m$B_MD,
      leontief_domestic = leontief_domestic,
      omega_M = m$omega_M,
      omega_D = m$omega_D,
      pce_bridge = pce_bridge,
      usd_offset = assumptions$usd_offset,
      domestic_pricing = assumptions$domestic_pricing,
      markup_assumption = ma
    )
  }
  if (markup_assumption == 'average') {
    average_price_results(one('constant_percentage'), one('constant_dollar'))
  } else {
    one(markup_assumption)
  }
}

# ==== Run for statutory (a, eta off) and eta'-adjusted (b, eta on) ===========

baskets <- list(
  list(key = 'a', file = 'bea_deltas_by_commodity.csv',   label = 'statutory (eta OFF)'),
  list(key = 'b', file = 'bea_deltas_by_commodity_b.csv', label = "eta'-adjusted (eta ON)")
)

message('\nDay-weighting tariff deltas over the window...')
results <- list()
basket_diag <- list()
for (b in baskets) {
  dw <- day_weight_tau_M(file.path(rate_inputs_dir, b$file), b$label)
  if (is.null(dw)) {
    message(sprintf('  [%s] %s not present -- skipped', b$label, b$file))
    next
  }
  message(sprintf('\n--- Pricing basket: %s ---', b$label))
  results[[b$key]] <- compute_presub(dw$tau_M)
  basket_diag[[b$key]] <- dw
}

if (length(results) == 0) {
  stop('No BEA delta files found in ', rate_inputs_dir)
}

# ==== Report =================================================================

message('\n=============================================================================')
message('Day-Weighted Average Tariff Price Effects (PRE-SUBSTITUTION)')
message('=============================================================================')
message(sprintf('  Scenario / vintage: %s / %s', scenario, vintage))
message(sprintf('  Window:             %s to %s (%d days)', start_date, end_date, TOTAL_DAYS))
message(sprintf('  Markup:             %s', markup_assumption))
message(sprintf('  USD offset:         %.1f%%', assumptions$usd_offset * 100))
message(sprintf('  Domestic pricing:   %.0f%%', assumptions$domestic_pricing * 100))
message('')
for (b in baskets) {
  r <- results[[b$key]]
  if (is.null(r)) next
  message(sprintf('  %-26s aggregate PCE price effect: %.3f pp  (direct %.3f, supply-chain %.3f)',
                  b$label, r$aggregate * 100, r$direct_aggregate * 100, r$supply_chain_aggregate * 100))
}
if (!is.null(results$a) && !is.null(results$b)) {
  message(sprintf('\n  Apply-etas step (b - a):     %.3f pp',
                  (results$b$aggregate - results$a$aggregate) * 100))
}
message('')

# Top affected PCE categories for the eta'-adjusted (b) basket if present, else (a).
top_src <- if (!is.null(results$b)) results$b else results$a
top_label <- if (!is.null(results$b)) "eta'-adjusted (b)" else 'statutory (a)'
message(sprintf('Top 15 most-affected PCE categories - %s (pp):', top_label))
top_cats <- top_src$pce_category_prices %>%
  arrange(desc(sr_price_effect)) %>%
  head(15)
for (i in seq_len(nrow(top_cats))) {
  row <- top_cats[i, ]
  message(sprintf('  %+.3f pp  %s (line %d, %.1f%% of PCE)',
                  row$sr_price_effect, row$pce_category, row$nipa_line, row$pce_share))
}
message('\n=============================================================================')

# ==== Write summary CSV ======================================================
# One row per basket, self-describing (scenario/vintage/window/markup on each row).
# The eta-on (b) aggregate_pp is the Figure-4 "modeling updates" (bar 4) input.
basket_name <- c(a = 'statutory', b = 'eta_adjusted')
summary_rows <- do.call(rbind, lapply(baskets, function(b) {
  r <- results[[b$key]]; d <- basket_diag[[b$key]]
  if (is.null(r)) return(NULL)
  data.frame(
    scenario = scenario, vintage = vintage,
    window_start = start_date, window_end = end_date, markup = markup_assumption,
    basket = unname(basket_name[[b$key]]), eta_on = (b$key == 'b'),
    aggregate_pp = r$aggregate * 100,
    direct_pp = r$direct_aggregate * 100,
    supply_chain_pp = r$supply_chain_aggregate * 100,
    mean_weighted_delta_pct = d$mean_delta_pct,
    n_regime_dates = d$n_regime_dates,
    stringsAsFactors = FALSE
  )
}))
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(summary_rows, out_file)
message(sprintf('Wrote summary CSV: %s', out_file))
