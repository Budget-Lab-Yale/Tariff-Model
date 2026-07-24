# =============================================================================
# build_gtap_implied_weights.R
#
# Calibration 2, Stage 1: GTAP-implied trade weights (omega^G = f(tau')).
#
# Standalone harness — NOT part of run_model.R. For each calendar month in the
# calibration window, it summarises that month's statutory rates as the
# calendar-day-weighted AVERAGE rate, eta'-adjusts them, and runs GTAP
# (sequential + cached) to read back the equilibrium import matrix (VIWS). Plus
# one extra solve on the whole-window day-weighted-average ETR ("period_avg").
#
# The result is the slow, cacheable input the later alpha-estimation step will
# consume (observed weights, alpha fitting, etc. are a SEPARATE downstream task).
#
# It reuses the core machinery verbatim: rate-panel reading + pair-frame +
# eta' (00a), GTAP execution (00b), VIWS readback (read_gtap + the trade-weights
# prototype). The one new piece is day-weighted averaging.
#
# Memory note: a single revision snapshot is ~5M rows; binding all revisions in
# the window would be ~100M rows. Day-weighting the rate, import-weighting pairs,
# eta'-scaling and rolling up are ALL linear, so we roll each regime up to the
# tiny (~360-row) partner x GTAP frame FIRST — reading one revision at a time —
# then day-weight those rollups. Identical result, peak memory = one revision.
#
# Output (written to output/calibration/<scenario>/):
#   gtap_implied_weights.csv   period x gtap_code x partner: baseline/postsim
#                              imports ($M) and shares (postsim_share = omega^G)
#   stage1_solve_log.csv       per-period solve metadata
#
# Usage (from repo root): see calibrate.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

source('src/read_gtap.R')               # read_har, GTAP_SLC_HEADERS (also loads HARr)
source('src/00b_run_gtap.R')            # run_gtap
source('src/00a_prepare_rate_inputs.R') # load_aggregation_resources, build_pair_frame,
                                        # load_eta_prime, rollup_gtap, write_shocks,
                                        # SECTOR_ORDER ; sources read_rate_panel.R

GOODS_GRID_FILE <- 'resources/gtap_baseline/import_baseline_dollars.csv'

# =============================================================================
# VIWS readback helpers (lifted from src/util/run_gtap_trade_weights.R)
# =============================================================================

#' Pull a [commodity x source_region] import matrix for imports INTO the USA
#' (dst = region 1) from a parsed .slc header.
extract_viws_matrix <- function(slc, header, regions, commodities,
                                target_region = 1) {
  if (is.null(slc[[header]])) {
    stop('VIWS header not found in .slc: ', header)
  }
  m <- slc[[header]][, , target_region]
  rownames(m) <- commodities
  colnames(m) <- regions
  return(m)
}

#' Tidy a wide [commodity x region] matrix into long form with a value column.
matrix_to_long <- function(m, value_name) {
  as_tibble(m, rownames = 'commodity') %>%
    pivot_longer(-commodity, names_to = 'source_region', values_to = value_name)
}

# =============================================================================
# Regime rollups + day-weighted averaging (the new logic)
# =============================================================================

#' Build the consecutive-interval regime schedule over an overall window
#'
#' A "regime" is a statutory rate state in effect over [valid_from, valid_until)
#' (exclusive). Snapshot mode: one regime per tracker revision. Interval mode:
#' one regime per distinct change date in the dense panel, anchored at the window
#' start. Restricted to regimes overlapping [overall_start, overall_end].
#'
#' @return Tibble(valid_from, valid_until) — valid_until = NA means open-ended
build_regime_schedule <- function(rate_panel, series, overall_start, overall_end,
                                  use_snapshots, full_panel = NULL) {

  overall_start <- as.Date(overall_start)
  overall_end   <- as.Date(overall_end)
  end_excl      <- overall_end + 1L

  if (use_snapshots) {
    dates <- list_rate_snapshot_dates(rate_panel, series)
    schedule <- tibble(valid_from = dates, valid_until = c(dates[-1], as.Date(NA)))
  } else {
    raw_changes <- sort(unique(full_panel$valid_from))
    changes_in  <- raw_changes[raw_changes > overall_start & raw_changes <= overall_end]
    starts      <- c(overall_start, changes_in)
    schedule    <- tibble(valid_from = starts, valid_until = c(starts[-1], as.Date(NA)))
  }

  active <- schedule %>%
    filter(valid_from < end_excl, is.na(valid_until) | valid_until > overall_start)
  if (nrow(active) == 0) {
    stop('No rate regimes overlap ', overall_start, '..', overall_end,
         ' for series "', series, '"')
  }
  return(active)
}

#' Get the static rate slice (hts10, cty_code, rate_total) for one regime start
get_regime_slice <- function(rate_panel, series, valid_from, use_snapshots,
                             full_panel = NULL) {
  if (use_snapshots) {
    read_rate_snapshot(rate_panel, series, valid_from) %>%
      select(hts10, cty_code, rate_total)
  } else {
    slice_panel_at(full_panel, valid_from) %>%
      select(hts10, cty_code, rate_total)
  }
}

#' Roll up every regime to a partner x GTAP shock frame (eta'-adjusted delta + level)
#'
#' Reads one revision at a time and discards the ~5M-row slice immediately, so
#' peak memory is a single revision. eta' is applied at the PAIR level (before
#' rollup) when use_eta: it must be, because rollup_gtap collapses Korea into the
#' ftrow partner while eta' keys on eta_group (Korea split out). Returns the tiny
#' per-regime rollups stacked with their validity interval, ready for day-weighting.
#'
#' @return Tibble(partner, gtap_code, etr, level, valid_from, valid_until)
build_regime_rollups <- function(schedule, rate_panel, series, baseline_slice,
                                 resources, use_snapshots, full_panel = NULL,
                                 eta_prime = NULL, default_eta = NULL, use_eta = TRUE) {
  rolls <- pmap_dfr(
    list(schedule$valid_from, schedule$valid_until),
    function(vf, vu) {
      slice <- get_regime_slice(rate_panel, series, vf, use_snapshots, full_panel)
      pairs <- build_pair_frame(slice, baseline_slice, resources)
      if (use_eta) {
        pairs <- apply_eta_prime(pairs, eta_prime, default_eta)
      }
      rollup_gtap(pairs) %>% mutate(valid_from = vf, valid_until = vu)
    }
  )
  return(rolls)
}

#' Day-weighted average of the per-regime rollups over [W0, W1] (inclusive days)
#'
#' Weights each regime's import-weighted delta/level by its days of overlap with
#' the window. Every regime spans the full partner x GTAP grid, so the average is
#' gapless. The rollups are already eta'-adjusted (rate b) when use_eta, so this
#' yields the period's day-weighted-average (b) shock directly.
#'
#' @return Tibble(partner, gtap_code, etr, level)
average_rollups_over_window <- function(regime_rolls, W0, W1) {
  W0 <- as.Date(W0)
  end_excl <- as.Date(W1) + 1L

  regime_rolls %>%
    mutate(overlap_days = as.integer(
      pmin(coalesce(valid_until, end_excl), end_excl) - pmax(valid_from, W0))) %>%
    filter(overlap_days > 0) %>%
    group_by(partner, gtap_code) %>%
    summarise(etr   = sum(etr   * overlap_days) / sum(overlap_days),
              level = sum(level * overlap_days) / sum(overlap_days),
              .groups = 'drop')
}

# =============================================================================
# Orchestrator
# =============================================================================

#' Build the GTAP-implied trade-weights panel for a scenario
#'
#' @param scenario Scenario name (config/scenarios/<scenario>/)
#' @param months Character vector of 'YYYY-MM' to solve; NULL -> all months in
#'   the configured window
#' @param period_average Also solve the whole-window day-weighted-average ETR;
#'   NULL -> config default (TRUE)
#' @param use_eta Feed GTAP the eta'-adjusted shocks (default TRUE); FALSE -> pure
#'   statutory shocks (eta' = 1)
#' @param force_resolve Ignore cached GTAP solutions and re-solve
#' @return Invisibly, the paths written
build_gtap_implied_weights <- function(scenario, months = NULL, period_average = NULL,
                                       use_eta = TRUE, force_resolve = FALSE) {

  scenario_dir <- file.path('config', 'scenarios', scenario)
  model_params <- yaml::read_yaml(file.path(scenario_dir, 'model_params.yaml'))

  rate_panel <- model_params$rate_panel
  if (is.null(rate_panel)) {
    stop('model_params.yaml must have a rate_panel block (root, vintage, tracker_scenario, baseline_date)')
  }
  series <- rate_panel$tracker_scenario %||% 'actual'

  # ---- calibration config (with code defaults so no config block is required) ----
  calib <- model_params$calibration %||% list()
  window_start <- calib$window_start %||% '2025-06'
  window_end   <- calib$window_end   %||% '2026-02'
  if (is.null(months))         months         <- calib$months          # may stay NULL -> all
  if (is.null(period_average)) period_average <- calib$period_average %||% TRUE
  include_retaliation <- calib$include_retaliation %||%
    model_params$gtap$include_retaliation %||% TRUE

  # ---- period list ----
  last_day <- function(first) ceiling_date(as.Date(first), 'month') - 1L
  all_months <- format(seq(as.Date(paste0(window_start, '-01')),
                           as.Date(paste0(window_end, '-01')), by = 'month'), '%Y-%m')
  # months may arrive as a CLI char vector or a YAML list -> coerce to character.
  month_list <- if (is.null(months)) all_months else as.character(unlist(months))

  periods <- tibble(
    period       = month_list,
    basis        = 'monthly',
    window_start = as.Date(paste0(month_list, '-01')),
    window_end   = last_day(paste0(month_list, '-01'))
  )
  if (isTRUE(period_average)) {
    periods <- bind_rows(periods, tibble(
      period       = 'period_avg',
      basis        = 'period_avg',
      window_start = as.Date(paste0(window_start, '-01')),
      window_end   = last_day(paste0(window_end, '-01'))
    ))
  }
  period_levels <- periods$period

  message(strrep('=', 64))
  message('GTAP-implied trade weights | scenario: ', scenario)
  message('  window: ', window_start, ' .. ', window_end,
          '  |  periods: ', nrow(periods),
          '  |  eta: ', if (use_eta) 'on' else 'off (pure statutory)',
          '  |  retaliation: ', include_retaliation)
  message(strrep('=', 64))

  # ---- one-time setup ----
  resources <- load_aggregation_resources(rate_panel)
  use_snapshots <- has_snapshot_series(rate_panel, series)

  # Interval-mode panels are read once (snapshot mode reads per-revision, lazily).
  full_panel <- if (use_snapshots) NULL else read_rate_panel(rate_panel, series)

  baseline_slice <- if (use_snapshots) {
    read_rate_snapshot(rate_panel, 'actual', rate_panel$baseline_date)
  } else {
    slice_panel_at(read_rate_panel(rate_panel, 'actual'), rate_panel$baseline_date)
  }

  global_assumptions <- yaml::read_yaml('config/global_assumptions.yaml')
  default_eta <- 1 - (global_assumptions$compliance_effect %||% 0)
  eta_prime <- if (use_eta) load_eta_prime(model_params) else NULL

  # ---- per-regime rollups over the overall window (read each revision once) ----
  overall_start <- min(periods$window_start)
  overall_end   <- max(periods$window_end)
  schedule <- build_regime_schedule(rate_panel, series, overall_start, overall_end,
                                    use_snapshots, full_panel)
  message(sprintf('\nRolling up %d rate regime(s) over %s .. %s ...',
                  nrow(schedule), overall_start, overall_end))
  regime_rolls <- build_regime_rollups(schedule, rate_panel, series, baseline_slice,
                                       resources, use_snapshots, full_panel,
                                       eta_prime = eta_prime, default_eta = default_eta,
                                       use_eta = use_eta)

  # Goods grid the model uses downstream (45 goods x 8 partners).
  goods <- tolower(read_csv(GOODS_GRID_FILE, show_col_types = FALSE)[[1]])

  out_root  <- calibration_dir(scenario)
  gtap_root <- file.path(out_root, 'gtap')
  dir.create(gtap_root, showWarnings = FALSE, recursive = TRUE)

  # ---- per-period loop (sequential: GTAP shares one work dir) ----
  panel_rows <- list()
  solve_log  <- list()

  for (i in seq_len(nrow(periods))) {
    P <- periods[i, ]
    message('\n', strrep('-', 64))
    message(sprintf('[%d/%d] %s  (%s .. %s)', i, nrow(periods), P$period,
                    P$window_start, P$window_end))

    # regime rollups are already eta'-adjusted (rate b) when use_eta, so the
    # day-weighted average IS the period's (b) shock frame.
    shock_frame <- average_rollups_over_window(regime_rolls, P$window_start, P$window_end)

    # 4. write per-period shocks.txt
    period_dir <- file.path(gtap_root, P$period)
    dir.create(period_dir, showWarnings = FALSE, recursive = TRUE)
    shocks_path <- file.path(period_dir, 'shocks.txt')
    write_shocks(shock_frame, shocks_path)

    # 5. run GTAP (cached on .sol/.slc/.sl4). The cache is keyed on the exact
    #    shocks: a sidecar .shockhash records the shocks.txt md5 that produced the
    #    cached solution, so a changed rate basket under the same scenario/period
    #    invalidates it rather than silently reusing a stale solve. Absent/stale
    #    hash -> re-solve; --force-resolve overrides regardless.
    sol_name <- paste0(scenario, '_', P$period)
    sol_base <- file.path(period_dir, sol_name)
    hash_path   <- paste0(sol_base, '.shockhash')
    shocks_hash <- unname(tools::md5sum(shocks_path))
    cached <- !force_resolve &&
      file.exists(paste0(sol_base, '.sol')) &&
      file.exists(paste0(sol_base, '.slc')) &&
      file.exists(paste0(sol_base, '.sl4')) &&
      file.exists(hash_path) &&
      identical(readLines(hash_path, warn = FALSE), shocks_hash)

    t0 <- Sys.time()
    if (cached) {
      message('  Reusing cached GTAP solution (shocks unchanged)')
      paths <- list(sol = paste0(sol_base, '.sol'),
                    slc = paste0(sol_base, '.slc'),
                    sl4 = paste0(sol_base, '.sl4'))
      exit_code <- 0L
    } else {
      paths <- run_gtap(scenario,
                        include_retaliation  = include_retaliation,
                        shocks_file_override = shocks_path,
                        output_subdir        = period_dir,
                        solution_name        = sol_name)
      exit_code <- paths$exit_code %||% 0L
      # Stamp the shocks fingerprint alongside the fresh solution (run_gtap stops
      # on a failed solve, so reaching here means the .sol is valid).
      if (identical(exit_code, 0L)) writeLines(shocks_hash, hash_path)
    }
    solve_secs <- as.numeric(difftime(Sys.time(), t0, units = 'secs'))

    # 6. read back baseline + post-sim VIWS (imports INTO USA, dst = region 1)
    sl4 <- read_har(paths$sl4)
    stel <- sl4[['stel']]
    if (is.null(stel)) stop('GTAP .sl4 missing stel metadata: ', paths$sl4)
    regions     <- stel[1:9]
    commodities <- stel[10:74]

    slc <- read_har(paths$slc)
    viws_postsim  <- extract_viws_matrix(slc, GTAP_SLC_HEADERS$viws,
                                         regions, commodities)
    viws_baseline <- extract_viws_matrix(slc, GTAP_SLC_HEADERS$viws_baseline,
                                         regions, commodities)

    panel_rows[[P$period]] <-
      matrix_to_long(viws_baseline, 'baseline_imports') %>%
      left_join(matrix_to_long(viws_postsim, 'postsim_imports'),
                by = c('commodity', 'source_region')) %>%
      filter(source_region != 'usa') %>%          # 'usa' source is a placeholder
      rename(gtap_code = commodity, partner = source_region) %>%
      mutate(period = P$period, basis = P$basis,
             window_start = P$window_start, window_end = P$window_end)

    solve_log[[P$period]] <- tibble(
      period = P$period, basis = P$basis,
      window_start = P$window_start, window_end = P$window_end,
      cached = cached, exit_code = exit_code,
      solve_secs = round(solve_secs, 1), shocks = shocks_path
    )
  }

  # ---- assemble: restrict to goods grid, shares within period x gtap ----
  panel <- bind_rows(panel_rows) %>%
    filter(gtap_code %in% goods) %>%
    group_by(period, gtap_code) %>%
    mutate(baseline_share = baseline_imports / sum(baseline_imports),
           postsim_share  = postsim_imports / sum(postsim_imports)) %>%
    ungroup() %>%
    select(period, basis, window_start, window_end, gtap_code, partner,
           baseline_imports, postsim_imports, baseline_share, postsim_share) %>%
    arrange(match(period, period_levels), match(gtap_code, SECTOR_ORDER), partner)

  panel_path <- file.path(out_root, 'gtap_implied_weights.csv')
  log_path   <- file.path(out_root, 'stage1_solve_log.csv')
  write_csv(panel, panel_path)
  write_csv(bind_rows(solve_log), log_path)

  message('\n', strrep('=', 64))
  message('Done. omega^G panel: ', nrow(panel), ' rows (', n_distinct(panel$gtap_code),
          ' goods x ', n_distinct(panel$partner), ' partners x ', nrow(periods), ' periods)')
  message('  ', normalizePath(panel_path))
  message('  ', normalizePath(log_path))
  message(strrep('=', 64))

  invisible(c(panel_path, log_path))
}
