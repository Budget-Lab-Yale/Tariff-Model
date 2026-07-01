# =============================================================================
# Command line interface for the Tariff Model calibration harness
#
# Standalone — NOT part of run_model.R. The production pipeline consumes the
# OUTPUTS of these stages (eta_by_partner_gtap.csv, alpha_parameters.csv) via
# the scenario's model_params.yaml pointers; it never runs calibration itself.
#
# Stages (--stage):
#   gtap-weights  (DEFAULT) Stage 1: GTAP-implied trade-weights panel (omega^G).
#                 Runs GTAP across the window on the eta'-adjusted shocks and
#                 reads back equilibrium import shares. Alpha's GTAP input.
#   panel         Build the shared IMDB-actuals panel (HS10 x cty x month:
#                 con_val_mo, cal_dut_mo, day-weighted statutory rate). Feeds
#                 BOTH eta and alpha.
#   eta           Calibrate eta -> output/calibration/<scenario>/eta_by_partner_gtap.csv
#                 (assumes the panel is built).
#   check         Reproduction gate: compare the calibrated eta to the golden
#                 fixture (vintage-gated).
#   alpha         Calibrate the two-channel substitution alphas (assumes the
#                 panel and gtap-weights are built).
#
# Usage (from repo root):
#   Rscript calibrate.R <scenario_name> [--stage <stage>] [options]
#
# gtap-weights options:
#   --no-eta               Pure statutory shocks (eta' = 1) instead of eta'-adjusted
#   --force-resolve        Ignore cached GTAP solutions and re-solve
#   --months <a,b,...>     Comma-separated YYYY-MM months (default: all in window)
#   --no-period-average    Skip the whole-window day-weighted-average solve
# panel options:
#   --refresh-imdb         Rebuild the IMDB aggregate from ZIPs
#   --reuse-rates          Reuse the cached statutory_rates_monthly.csv (skip the
#                          snapshot day-weighting pass)
#
# Examples:
#   Rscript calibrate.R tracker_actual_2026-06-13 --stage panel
#   Rscript calibrate.R tracker_actual_2026-06-13 --stage eta
#   Rscript calibrate.R tracker_actual_2026-06-13 --stage check
#   Rscript calibrate.R tracker_actual_2026-06-04                 # Stage 1 (default)
#   Rscript calibrate.R tracker_actual_2026-06-04 --stage alpha
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat('Usage: Rscript calibrate.R <scenario_name> [--stage <stage>] [options]\n')
  cat('\nStages: gtap-weights (default), panel, eta, check, alpha\n')
  cat('\ngtap-weights options:\n')
  cat('  --no-eta               Pure statutory shocks (eta\' = 1) instead of eta\'-adjusted\n')
  cat('  --force-resolve        Ignore cached GTAP solutions and re-solve\n')
  cat('  --months <a,b,...>     Comma-separated YYYY-MM months (default: all in window)\n')
  cat('  --no-period-average    Skip the whole-window day-weighted-average solve\n')
  cat('panel options:\n')
  cat('  --refresh-imdb         Rebuild the IMDB aggregate from ZIPs\n')
  cat('  --reuse-rates          Reuse the cached statutory rate panel\n')
  cat('\nAvailable scenarios:\n')
  scenarios <- list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)
  for (s in scenarios) cat(sprintf('  %s\n', s))
  quit(status = 1)
}

scenario <- args[1]

# --- stage selector (default preserves the original Stage-1 invocation) ---
stage <- 'gtap-weights'
if ('--stage' %in% args) {
  idx <- which(args == '--stage')
  if (idx < length(args)) stage <- args[idx + 1]
}

# Calibration artifacts resolve to the shared model_data interface tree (never
# the repo checkout). See src/paths.R (calibration_dir / imdb_cache_dir).
source('src/paths.R')

run_gtap_weights <- function() {
  use_eta        <- !('--no-eta' %in% args)
  force_resolve  <- '--force-resolve' %in% args
  period_average <- if ('--no-period-average' %in% args) FALSE else NULL
  months <- NULL
  if ('--months' %in% args) {
    idx <- which(args == '--months')
    if (idx < length(args)) months <- trimws(strsplit(args[idx + 1], ',')[[1]])
  }
  source('src/calibration/build_gtap_implied_weights.R')
  build_gtap_implied_weights(scenario, months = months, period_average = period_average,
                             use_eta = use_eta, force_resolve = force_resolve)
}

run_panel <- function() {
  source('src/calibration/build_imdb_actuals_panel.R')
  build_imdb_actuals_panel(scenario,
                           refresh_imdb = '--refresh-imdb' %in% args,
                           reuse_rates  = '--reuse-rates' %in% args)
}

run_eta <- function() {
  source('src/calibration/calibrate_eta.R')
  calibrate_eta(scenario)
}

run_check <- function() {
  source('src/calibration/calibration_helpers.R')   # %||%, calib_output_dir
  source('src/calibration/check_eta_reproduction.R')
  check_eta_reproduction(scenario)
}

run_alpha <- function() {
  source('src/calibration/calibrate_alpha.R')
  calibrate_alpha(scenario)
}

switch(stage,
  'gtap-weights' = run_gtap_weights(),
  'panel'        = run_panel(),
  'eta'          = run_eta(),
  'check'        = run_check(),
  'alpha'        = run_alpha(),
  stop('Unknown --stage: ', stage,
       ' (expected gtap-weights | panel | eta | check | alpha)', call. = FALSE)
)
