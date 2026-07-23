# =============================================================================
# build_imdb_actuals_panel.R
#
# Calibration data-prep: assemble the monthly HS10 x country actuals panel that
# BOTH the eta calibration (cal_dut_mo / con_val_mo realized-collections shape)
# and the alpha calibration (con_val_mo actual-window baskets) consume.
#
# Ported from tariff-etr-adj/code/01_data.R, with one deliberate change:
# Section 2 (the statutory rate panel) is rebuilt on Tariff-Model's own snapshot
# reader (src/read_rate_panel.R) instead of etr-adj's tracker resolver, so there
# is ONE rate lineage in this repo. The snapshot read is byte-identical to
# etr-adj's raw total_rate read (verified: each valid_from=<date>/rates.parquet
# is a flat as-of panel, one row per (hts10,country); max abs diff 0), so this
# reproduces the golden panel.
#
# Sections (mirrors 01_data.R):
#   1. Census IMDB bulk    -- HS10 x country x month con_val_mo + cal_dut_mo
#   2. Statutory rates     -- day-weighted monthly total_rate from the scenario's
#                             tracker snapshots, restricted to traded cells
#   3. 2024 import weights -- the tracker's published weight base (announced
#                             baseline; only feeds the non-shipped announced specs)
#   4. Treasury revenue    -- committed snapshot in resources/calibration/
#   5. Processed panel      -- merge 1-4 -> panel.rds + revenue_monthly.csv
#
# Output (output/calibration/<scenario>/panel/):
#   panel.rds, revenue_monthly.csv, statutory_rates_monthly.csv,
#   statutory_rates_meta.csv, import_weights_2024.csv
# Shared IMDB cache (output/calibration/_imdb/): imdb_hs10_country_monthly.csv,
#   imdb_2024_hs10_country.csv, zips/
#
# Usage (from repo root): see calibrate.R --stage panel
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringi)
})

source('src/calibration/calibration_helpers.R')  # window consts, assign_partner_group, safe_divide, msg
source('src/calibration/imdb.R')                  # get_imdb_zip, parse_imdb, build_imdb_agg
source('src/read_rate_panel.R')                   # snapshot resolver/reader (TM lineage)

# =============================================================================
# Section 2 helpers: day-weighted monthly rate panel from tracker snapshots
# =============================================================================

#' Read one tracker snapshot's as-of rates, restricted to traded cells.
#'
#' Raw total_rate read (3 columns) — byte-identical to read_rate_snapshot() but
#' cheaper. Located via TM's snapshot resolver. `country` is read as character so
#' it joins the Census cty_code.
load_snapshot_rates <- function(rate_panel, series, date, trade_keys) {
  path <- resolve_snapshot_path(rate_panel, series, date)
  arrow::read_parquet(path, col_select = c('hts10', 'country', 'total_rate')) %>%
    mutate(hts10 = as.character(hts10), country = as.character(country)) %>%
    semi_join(trade_keys, by = c('hts10', 'country'))
}

#' Build the day-weighted monthly statutory rate panel (HS10 x country x month).
#'
#' Each tracker revision (one valid_from=<date> snapshot) is in effect from its
#' date until the next revision's date. Every calendar day of each window month
#' is assigned to the revision active that day; the month's rate is the day-
#' weighted average of the active revisions' total_rate. Mirrors 01_data.R's
#' month x revision day-weighting exactly, at HS10 x cty grain.
build_monthly_rate_panel <- function(rate_panel, series, all_yms, trade_keys) {
  dates <- list_rate_snapshot_dates(rate_panel, series)        # sorted ascending
  rev_dates <- tibble(effective_date = dates) %>%
    arrange(effective_date) %>%
    mutate(next_eff = lead(effective_date, default = as.Date('2099-12-31')))

  # Month x revision day-weights over the analysis window
  month_starts <- as.Date(paste0(all_yms, '-01'))
  mrw <- bind_rows(lapply(month_starts, function(m1) {
    m_end <- seq.Date(m1, by = 'month', length.out = 2)[2] - 1L
    dim   <- as.integer(m_end - m1) + 1L
    rev_dates %>%
      filter(effective_date <= m_end, next_eff > m1) %>%
      mutate(o_start = pmax(effective_date, m1),
             o_end   = pmin(next_eff - 1L, m_end),
             days    = as.integer(o_end - o_start) + 1L,
             weight  = days / dim) %>%
      filter(days > 0) %>%
      transmute(year_month = format(m1, '%Y-%m'), effective_date, weight)
  }))
  msg('    %d month-revision pairs over %d months', nrow(mrw), length(all_yms))

  needed <- sort(unique(mrw$effective_date))
  parts <- list()
  for (i in seq_along(needed)) {
    d <- needed[i]                                  # [i] preserves Date class
    snap <- load_snapshot_rates(rate_panel, series, d, trade_keys)
    dw   <- mrw[mrw$effective_date == d, , drop = FALSE]
    for (j in seq_len(nrow(dw))) {
      parts[[length(parts) + 1L]] <- snap %>%
        mutate(year_month = dw$year_month[j],
               wtd_rate   = total_rate * dw$weight[j])
    }
    msg('    %s: %s traded cells x %d month(s)', d,
        format(nrow(snap), big.mark = ','), nrow(dw))
    rm(snap); gc(verbose = FALSE)
  }

  bind_rows(parts) %>%
    summarise(rate_h2avg = sum(wtd_rate), .by = c(hts10, country, year_month)) %>%
    rename(hs10 = hts10, cty_code = country)
}

# =============================================================================
# Orchestrator
# =============================================================================

#' Build the IMDB-actuals panel for a scenario.
#'
#' @param scenario Scenario name (config/scenarios/<scenario>/)
#' @param refresh_imdb Force a rebuild of the IMDB aggregate from ZIPs
#' @param reuse_rates Skip the snapshot day-weighting pass and reuse the cached
#'   statutory_rates_monthly.csv (only safe if snapshots are unchanged)
#' @return Invisibly, the paths written
build_imdb_actuals_panel <- function(scenario, refresh_imdb = FALSE, reuse_rates = FALSE) {

  scenario_dir <- file.path('config', 'scenarios', scenario)
  model_params <- yaml::read_yaml(file.path(scenario_dir, 'model_params.yaml'))
  set_calib_window(model_params)   # window from the scenario's calibration: block
  rate_panel <- model_params$rate_panel
  if (is.null(rate_panel)) {
    stop('model_params.yaml must have a rate_panel block (root, vintage, tracker_scenario, ...)')
  }
  series <- rate_panel$tracker_scenario %||% 'actual'

  out_dir   <- file.path(calib_output_dir(scenario), 'panel')
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(IMDB_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

  # ALL_YMS is the FULL pull window (descriptive ladder); calibration uses the
  # [TRAIN_LO, TEST_YM) subset, tagged via `period`.
  all_yms <- format(seq(as.Date(paste0(LADDER_LO, '-01')),
                        as.Date(paste0(TEST_YM, '-01')), by = 'month'), '%Y-%m')

  message(strrep('=', 64))
  message('IMDB-actuals panel | scenario: ', scenario)
  message('  window: ', all_yms[1], ' .. ', all_yms[length(all_yms)],
          '  |  vintage: ', rate_panel$vintage %||% 'latest',
          '  |  series: ', series)
  message(strrep('=', 64))

  # ---------------------------------------------------------------------------
  # 1. Census IMDB (shared, scenario-independent cache)
  # ---------------------------------------------------------------------------
  msg('[1] Census IMDB bulk files (%d months: %s .. %s)...',
      length(all_yms), all_yms[1], all_yms[length(all_yms)])
  imdb_csv  <- file.path(IMDB_CACHE_DIR, 'imdb_hs10_country_monthly.csv')
  imdb_cols <- cols(hs10 = col_character(), cty_code = col_character(),
                    year_month = col_character(),
                    con_val_mo = col_double(), cal_dut_mo = col_double())

  imdb_agg <- NULL
  if (file.exists(imdb_csv) && !refresh_imdb) {
    cached <- read_csv(imdb_csv, col_types = imdb_cols, progress = FALSE)
    if (all(all_yms %in% unique(cached$year_month))) {
      imdb_agg <- filter(cached, year_month %in% all_yms)
      msg('    Reusing existing %s (%s rows; all %d window months present)',
          imdb_csv, format(nrow(imdb_agg), big.mark = ','), length(all_yms))
    }
    rm(cached)
  }
  if (is.null(imdb_agg)) {
    imdb_agg <- build_imdb_agg(all_yms)
    missing_yms <- setdiff(all_yms, unique(imdb_agg$year_month))
    if (length(missing_yms) > 0) {
      stop('IMDB months missing: ', paste(missing_yms, collapse = ', '),
           '\n  Census bulk files lag ~2.5 months; cannot proceed without the full window.')
    }
    write_csv(imdb_agg, imdb_csv)
    msg('    -> %s (%s rows)', imdb_csv, format(nrow(imdb_agg), big.mark = ','))
  }

  # ---------------------------------------------------------------------------
  # 2. Statutory rates: day-weighted monthly panel from tracker snapshots
  # ---------------------------------------------------------------------------
  msg('[2] Statutory rate panel (day-weighted tracker snapshots)...')
  rates_csv      <- file.path(out_dir, 'statutory_rates_monthly.csv')
  rates_meta_csv <- file.path(out_dir, 'statutory_rates_meta.csv')

  if (reuse_rates && file.exists(rates_csv)) {
    rates_monthly <- read_csv(rates_csv,
                              col_types = cols(hs10 = col_character(),
                                               cty_code = col_character(),
                                               year_month = col_character(),
                                               rate_h2avg = col_double()))
    msg('    Reusing existing %s (%s rows)', rates_csv,
        format(nrow(rates_monthly), big.mark = ','))
  } else {
    if (!has_snapshot_series(rate_panel, series)) {
      stop('Panel build requires a snapshot series (valid_from=* dirs) for "', series,
           '"; interval-only panels are not yet supported here.')
    }
    # Restrict each ~5M-row snapshot to cells with observed trade BEFORE
    # accumulating (~350K keys). Keys include each Census code's HS8 parent
    # (xxxxxxxx00) for tracker 8-digit leaf lines.
    imdb_keys <- distinct(imdb_agg, hs10, cty_code)
    trade_keys <- bind_rows(
      imdb_keys,
      imdb_keys %>% mutate(hs10 = paste0(substr(hs10, 1, 8), '00'))
    ) %>%
      distinct() %>%
      rename(hts10 = hs10, country = cty_code)
    msg('    Restricting snapshots to %s traded cells (incl. HS8 parents)',
        format(nrow(trade_keys), big.mark = ','))

    rates_monthly <- build_monthly_rate_panel(rate_panel, series, all_yms, trade_keys)
    write_csv(rates_monthly, rates_csv)
    msg('    -> %s (%s rows)', rates_csv, format(nrow(rates_monthly), big.mark = ','))

    # Vintage sidecar (manifest.json at the bundle vintage dir; JSON is valid
    # YAML). Stamped into eta_summary.csv by calibrate_eta so every result
    # records the tracker publish that produced it.
    vintage_dir <- file.path(resolve_rate_panel_root(rate_panel), rate_panel$vintage %||% 'latest')
    tracker_vintage <- NA_character_
    mf <- file.path(vintage_dir, 'manifest.json')
    if (file.exists(mf) && requireNamespace('yaml', quietly = TRUE)) {
      v <- tryCatch(yaml::read_yaml(mf)$vintage, error = function(e) NULL)
      if (!is.null(v) && nzchar(v)) tracker_vintage <- v
    }
    write_csv(data.frame(tracker_vintage = tracker_vintage,
                         tracker_source  = normalizePath(vintage_dir, mustWork = FALSE),
                         rates_built_at  = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z')),
              rates_meta_csv)
    msg('    tracker vintage: %s', tracker_vintage)
  }

  # ---------------------------------------------------------------------------
  # 3. 2024 import weights (announced baseline fixed basket)
  # ---------------------------------------------------------------------------
  # Only feeds the announced-baseline specs (not the shipped compadj contract).
  # Resolved from the tracker's published weight base (hts10, country, imports);
  # fallback self-builds from 2024 IMDB con_val_mo.
  msg('[3] 2024 import weights...')
  wt_csv  <- file.path(out_dir, 'import_weights_2024.csv')
  wt_path <- resolve_weights_path(rate_panel)

  if (!is.null(wt_path)) {
    raw_wt <- if (grepl('\\.parquet$', wt_path)) {
      arrow::read_parquet(wt_path, col_select = c('hts10', 'country', 'imports'))
    } else {
      read_csv(wt_path, col_types = cols(hts10 = col_character(),
                                         country = col_character(),
                                         imports = col_double(),
                                         .default = col_guess()))
    }
    raw_wt %>%
      transmute(hs10 = as.character(hts10), cty_code = as.character(country),
                imports = as.numeric(imports)) %>%
      summarise(imports = sum(imports, na.rm = TRUE), .by = c(hs10, cty_code)) %>%
      filter(imports > 0) %>%
      write_csv(wt_csv)
    msg('    -> from tracker weight base (%s)', basename(wt_path))
  } else {
    msg('    no published weight base; building from 2024 IMDB (12 months)...')
    yms_2024 <- sprintf('2024-%02d', 1:12)
    imdb_2024 <- build_imdb_agg(yms_2024) %>%
      summarise(con_val_mo = sum(con_val_mo, na.rm = TRUE),
                cal_dut_mo = sum(cal_dut_mo, na.rm = TRUE),
                .by = c(hs10, cty_code))
    write_csv(imdb_2024, file.path(IMDB_CACHE_DIR, 'imdb_2024_hs10_country.csv'))
    wt <- imdb_2024 %>%
      transmute(hs10, cty_code, imports = con_val_mo) %>%
      filter(imports > 0)
    if (nrow(wt) == 0) stop('2024 import-weight fallback produced no rows.')
    write_csv(wt, wt_csv)
    msg('    -> built from IMDB 2024 (%s cells)', format(nrow(wt), big.mark = ','))
  }

  # ---------------------------------------------------------------------------
  # 4. Treasury revenue (committed snapshot)
  # ---------------------------------------------------------------------------
  msg('[4] Treasury revenue (customs duties / imports ratio)...')
  treas_src <- file.path(CALIB_RES_DIR, 'treasury_revenue.csv')
  if (!file.exists(treas_src)) {
    stop('Committed Treasury series not found at: ', treas_src,
         '\n  See resources/calibration/treasury_revenue_SOURCE.md to restore it.')
  }

  # ---------------------------------------------------------------------------
  # 5. Processed panel (verbatim from 01_data.R Section 5)
  # ---------------------------------------------------------------------------
  msg('[5] Building processed analysis panel...')
  weights_2024 <- read_csv(wt_csv,
                           col_types = cols(hs10 = col_character(),
                                            cty_code = col_character(),
                                            imports = col_double()))

  # Exact 10-digit match first; fall back to the HS8 parent (xxxxxxxx00) for
  # Census stat suffixes whose snapshot line is an 8-digit leaf.
  rates_parent <- rates_monthly %>%
    filter(substr(hs10, 9, 10) == '00') %>%
    rename(hs8_parent = hs10, rate_parent = rate_h2avg)

  panel <- imdb_agg %>%
    left_join(rates_monthly, by = c('hs10', 'cty_code', 'year_month')) %>%
    mutate(hs8_parent = paste0(substr(hs10, 1, 8), '00')) %>%
    left_join(rates_parent, by = c('hs8_parent', 'cty_code', 'year_month')) %>%
    left_join(weights_2024,  by = c('hs10', 'cty_code')) %>%
    filter(con_val_mo > 0)

  n_fallback <- sum(is.na(panel$rate_h2avg) & !is.na(panel$rate_parent))
  msg('    HS8-parent rate fallback used for %s cells', format(n_fallback, big.mark = ','))

  panel <- panel %>%
    mutate(rate_h2avg    = coalesce(rate_h2avg, rate_parent, 0),
           imports       = coalesce(imports, 0),
           cal_dut_mo    = coalesce(cal_dut_mo, 0),
           hs2           = substr(hs10, 1, 2),
           partner_group = assign_partner_group(cty_code),
           census_etr    = safe_divide(cal_dut_mo, con_val_mo),
           period        = case_when(year_month == TEST_YM   ~ 'test',
                                     year_month >= TRAIN_LO   ~ 'train',
                                     TRUE                     ~ 'pre')) %>%
    select(-hs8_parent, -rate_parent)

  n_rate_match <- mean(panel$rate_h2avg > 0)
  msg('    panel: %s cells (pre=%s, train=%s, test=%s); %.1f%% with positive statutory rate',
      format(nrow(panel), big.mark = ','),
      format(sum(panel$period == 'pre'), big.mark = ','),
      format(sum(panel$period == 'train'), big.mark = ','),
      format(sum(panel$period == 'test'), big.mark = ','), 100 * n_rate_match)

  panel_path <- file.path(out_dir, 'panel.rds')
  saveRDS(panel, panel_path)
  msg('    -> %s', panel_path)

  # Treasury monthly ETR over the analysis window
  revenue <- read_csv(treas_src,
                      col_types = cols(date = col_date(), .default = col_double())) %>%
    mutate(year_month = format(date, '%Y-%m')) %>%
    filter(year_month %in% all_yms) %>%
    transmute(year_month, customs_duties, imports_value,
              treas_etr = customs_duties / imports_value) %>%
    distinct(year_month, .keep_all = TRUE)

  if (!setequal(revenue$year_month, all_yms)) {
    stop('Treasury revenue missing months: ',
         paste(setdiff(all_yms, revenue$year_month), collapse = ', '))
  }
  rev_path <- file.path(out_dir, 'revenue_monthly.csv')
  write_csv(revenue, rev_path)
  msg('    -> %s (%d months)', rev_path, nrow(revenue))

  message('\nDone: build_imdb_actuals_panel (', scenario, ')')
  invisible(c(panel_path, rev_path, rates_csv))
}
