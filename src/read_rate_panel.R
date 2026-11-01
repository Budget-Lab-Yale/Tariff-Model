# =============================================================================
# read_rate_panel.R
#
# Reader / adapter for the upstream tariff-rate-tracker rate panel.
#
# This is the ONLY layer that knows the tracker's on-disk shape. It quarantines
# everything the upstream contract has not yet frozen (file format, exact column
# names, interval end-date convention, manifest) so that the aggregation core in
# 00a_prepare_rate_inputs.R depends only on a stable canonical panel:
#
#   hts10 (chr), cty_code (chr), valid_from (Date), valid_until (Date, EXCLUSIVE),
#   rate_total (fraction)[, rate_<authority> (fraction) ...]
#
# The tracker output has all USMCA, metal-content, and exemption adjustments`r`n# already reflected in `total_rate`, which we read as `rate_total`.
# =============================================================================

library(tidyverse)

#' Resolve the tracker-bundle root, allowing an environment override
#'
#' Scenario configs carry `rate_panel.root`, but in practice that has been a
#' per-user absolute path (e.g. a Windows `C:/Users/...` dir), which is not
#' portable across machines. To run the same committed scenario on a laptop and
#' on the HPC without editing the config, the env var TARIFF_RATE_TRACKER_ROOT
#' takes precedence when set; otherwise the scenario's `root` is used.
#'
#' @param rate_panel The `rate_panel:` config block
#' @return Effective bundle root (character), or NULL if neither source is set
resolve_rate_panel_root <- function(rate_panel) {
  env_root <- Sys.getenv('TARIFF_RATE_TRACKER_ROOT', unset = NA_character_)
  if (!is.na(env_root) && nzchar(env_root)) return(env_root)
  # Production root from the interface config (config/interfaces/output_roots.yaml),
  # so BOTH the model run and the standalone calibration harness resolve the tracker
  # bundle without a per-scenario root. Falls back to the scenario's own root.
  roots_file <- 'config/interfaces/output_roots.yaml'
  if (file.exists(roots_file)) {
    prod <- yaml::read_yaml(roots_file)$production
    if (!is.null(prod)) return(file.path(prod, 'model_data', 'Tariff-Rate-Tracker'))
  }
  rate_panel$root
}

#' Resolve the on-disk path for one published series in a tracker bundle
#'
#' Mirrors the AuthoritySpec output layout:
#'   <root>/<vintage>/{actual | scenarios/<name>}/timeseries/rate_timeseries.<ext>
#'
#' @param rate_panel The `rate_panel:` config block (root, vintage, tracker_scenario, ...)
#' @param series 'actual' (current-law) or a scenario name under scenarios/
#'
#' @return Absolute path to the series' rate panel file (rds or parquet)
resolve_panel_path <- function(rate_panel, series) {

  root <- resolve_rate_panel_root(rate_panel)
  if (is.null(root)) {
    stop('rate_panel.root not set in model_params.yaml (and env TARIFF_RATE_TRACKER_ROOT unset)')
  }

  vintage_dir <- file.path(root, rate_panel$vintage %||% 'latest')

  series_dir <- if (series == 'actual') {
    file.path(vintage_dir, 'actual')
  } else {
    file.path(vintage_dir, 'scenarios', series)
  }

  ts_dir <- file.path(series_dir, 'timeseries')

  # Prefer parquet (the published format); fall back to rds (today's tracker)
  candidates <- file.path(ts_dir, c('rate_timeseries.parquet', 'rate_timeseries.rds'))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop('No rate panel found for series "', series, '" at: ', ts_dir,
         '\n  Looked for rate_timeseries.{parquet,rds}')
  }

  return(hit[1])
}

resolve_series_dir <- function(rate_panel, series) {
  root <- resolve_rate_panel_root(rate_panel)
  if (is.null(root)) {
    stop('rate_panel.root not set in model_params.yaml (and env TARIFF_RATE_TRACKER_ROOT unset)')
  }

  vintage_dir <- file.path(root, rate_panel$vintage %||% 'latest')
  if (series == 'actual') {
    file.path(vintage_dir, 'actual')
  } else {
    file.path(vintage_dir, 'scenarios', series)
  }
}

snapshot_dir <- function(rate_panel, series) {
  file.path(resolve_series_dir(rate_panel, series), 'snapshots')
}

#' Resolve the bundle's import-weight base
#'
#' The weight base is vintage-specific but NOT series-specific (same import flows
#' for actual + every scenario), so it lives at the vintage root, beside actual/.
#' The tracker publishes weights at every vintage, so the weight base always comes
#' from the rate panel's own vintage. See docs/tariff_rate_tracker_weights_request.md.
#'
#' @return Path to import_weights_hs10_country.{parquet,csv.gz}, or NULL if absent
resolve_weights_path <- function(rate_panel) {
  root <- resolve_rate_panel_root(rate_panel)
  if (is.null(root)) return(NULL)
  wdir <- file.path(root, rate_panel$vintage, 'weights')
  candidates <- file.path(wdir, c('import_weights_hs10_country.parquet',
                                  'import_weights_hs10_country.csv.gz'))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NULL)
  return(hit[1])
}

#' Resolve the bundle's manifest.json path (vintage root).
#'
#' @return Absolute path to <root>/<vintage>/manifest.json, or NULL if the
#'   vintage root can't be resolved.
resolve_manifest_path <- function(rate_panel) {
  root <- resolve_rate_panel_root(rate_panel)
  if (is.null(root)) return(NULL)
  file.path(root, rate_panel$vintage %||% 'latest', 'manifest.json')
}

#' Read the bundle manifest.json (fail loud if absent).
read_bundle_manifest <- function(rate_panel) {
  mpath <- resolve_manifest_path(rate_panel)
  if (is.null(mpath) || !file.exists(mpath)) {
    stop('Tracker bundle manifest.json not found', if (!is.null(mpath)) paste0(': ', mpath) else '',
         '\n  The vintage assertion requires the published manifest.', call. = FALSE)
  }
  jsonlite::read_json(mpath, simplifyVector = FALSE)
}

#' Read the single non-NA hts_vintage stamped into the weights file.
#'
#' The tracker's import_weights_hs10_country.{parquet,csv.gz} stamps every row
#' with the tip revision it was keyed to. There must be exactly one non-NA value.
#'
#' @return the single hts_vintage (character), or stop() on 0 / >1 distinct.
read_weights_hts_vintage <- function(weights_path) {
  hv <- if (str_detect(weights_path, '\\.parquet$')) {
    if (!requireNamespace('arrow', quietly = TRUE)) {
      stop('Reading the weights hts_vintage requires the `arrow` package: ', weights_path)
    }
    arrow::read_parquet(weights_path, col_select = 'hts_vintage')$hts_vintage
  } else {
    read_csv(weights_path, show_col_types = FALSE,
             col_types = cols(hts_vintage = col_character(),
                              .default = col_skip()))$hts_vintage
  }
  vals <- unique(as.character(hv[!is.na(hv)]))
  if (length(vals) != 1) {
    stop(sprintf('Weights file has %d distinct non-NA hts_vintage values (expected exactly 1): %s\n  %s',
                 length(vals), paste(vals, collapse = ', '), weights_path), call. = FALSE)
  }
  vals
}

#' Assert the weights file is keyed to the SAME tip revision the manifest records.
#'
#' Ties three independent facts together so a weights/panel vintage skew (the
#' root cause of the statutory-rate understatement) can never pass silently:
#'   1. The manifest's actual-series tip snapshot (max valid_from) carries a
#'      `revision` (added by the tracker Phase 5 publish).
#'   2. The weights parquet has exactly one non-NA `hts_vintage`.
#'   3. (1) == (2), and the manifest's weights.path resolves to the same file
#'      resolve_weights_path() picks.
#'
#' @return invisibly list(tip_revision, hts_vintage, weights_path).
assert_weights_vintage <- function(rate_panel) {
  man <- read_bundle_manifest(rate_panel)
  snaps <- man$series$actual$snapshots
  if (is.null(snaps) || length(snaps) == 0) {
    stop('Bundle manifest has no series.actual.snapshots — cannot assert the weights vintage.',
         call. = FALSE)
  }
  vfs <- vapply(snaps, function(s) as.character(s$valid_from %||% NA), character(1))
  tip <- snaps[[which.max(as.Date(vfs))]]
  tip_rev <- tip$revision %||% NA_character_
  if (is.na(tip_rev) || !nzchar(tip_rev)) {
    stop('Bundle manifest actual-series tip snapshot (valid_from=', tip$valid_from,
         ') has no `revision`. Republish the tracker vintage with the Phase 5 manifest ',
         '(every snapshot record must carry its revision).', call. = FALSE)
  }
  wpath <- resolve_weights_path(rate_panel)
  if (is.null(wpath)) {
    stop('No weights file in the bundle to assert the vintage against.', call. = FALSE)
  }
  hv <- read_weights_hts_vintage(wpath)
  if (!identical(as.character(hv), as.character(tip_rev))) {
    stop(sprintf(paste0('Weights vintage mismatch: weights hts_vintage = "%s" but the manifest ',
                        'actual-series tip revision = "%s".\n  weights : %s\n  manifest: %s'),
                 hv, tip_rev, wpath, resolve_manifest_path(rate_panel)), call. = FALSE)
  }
  # The manifest's weights.path must resolve to the file we actually read.
  man_wpath <- man$weights$path
  if (!is.null(man_wpath) && nzchar(man_wpath)) {
    vintage_dir <- file.path(resolve_rate_panel_root(rate_panel), rate_panel$vintage %||% 'latest')
    resolved_man <- normalizePath(file.path(vintage_dir, man_wpath), mustWork = FALSE)
    if (!identical(resolved_man, normalizePath(wpath, mustWork = FALSE))) {
      stop(sprintf(paste0('Manifest weights.path (%s) does not resolve to the weights file ',
                          'resolve_weights_path() picked (%s).'),
                   resolved_man, normalizePath(wpath, mustWork = FALSE)), call. = FALSE)
    }
  }
  message(sprintf('  Weights vintage OK: hts_vintage = %s = manifest actual tip revision', tip_rev))
  invisible(list(tip_revision = tip_rev, hts_vintage = hv, weights_path = wpath))
}

has_snapshot_series <- function(rate_panel, series) {
  dir.exists(snapshot_dir(rate_panel, series))
}

list_rate_snapshot_dates <- function(rate_panel, series) {
  snap_dir <- snapshot_dir(rate_panel, series)
  if (!dir.exists(snap_dir)) {
    stop('No snapshot directory found for series "', series, '": ', snap_dir)
  }

  dirs <- list.dirs(snap_dir, recursive = FALSE, full.names = FALSE)
  dates <- str_match(dirs, '^valid_from=(\\d{4}-\\d{2}-\\d{2})$')[, 2]
  dates <- as.Date(dates[!is.na(dates)])
  if (length(dates) == 0) {
    stop('No valid_from=YYYY-MM-DD snapshot directories found in: ', snap_dir)
  }

  sort(dates)
}

resolve_snapshot_path <- function(rate_panel, series, date) {
  date <- as.Date(date)
  path <- file.path(snapshot_dir(rate_panel, series),
                    paste0('valid_from=', date),
                    'rates.parquet')
  if (!file.exists(path)) {
    stop('No rate snapshot found for series "', series, '" at ', date,
         '\n  Looked for: ', path)
  }
  path
}

#' Read a rate panel file (parquet or rds) into a tibble
#'
#' @param path File path ending in .parquet or .rds
#'
#' @return Tibble as stored upstream (pre-normalization)
read_panel_file <- function(path) {
  if (str_detect(path, '\\.parquet$')) {
    if (!requireNamespace('arrow', quietly = TRUE)) {
      stop('Reading parquet rate panels requires the `arrow` package: ', path)
    }
    ds <- arrow::open_dataset(path)
    raw_names <- names(ds)
    keep_cols <- c(
      'hts10', 'cty_code', 'country',
      'rate_total', 'total_rate',
      raw_names[str_detect(raw_names, '^rate_[a-z0-9_]+$')],
      'valid_from', 'valid_until', 'effective_date'
    )
    keep_cols <- intersect(unique(keep_cols), raw_names)
    return(arrow::read_parquet(path, col_select = all_of(keep_cols)))
  }
  return(as_tibble(readRDS(path)))
}

#' Normalize an upstream panel to the canonical contract
#'
#' Renames to canonical column names, coerces types, and converts the interval
#' end-date convention to EXCLUSIVE half-open `[valid_from, valid_until)` so the
#' rest of the model has one consistent semantics. The current tracker emits an
#' INCLUSIVE `valid_until` (last active day); AuthoritySpec has not yet converged
#' the convention (authority_spec.md:244), so it is declared in config.
#'
#' @param raw Tibble from read_panel_file()
#' @param interval_end 'inclusive' (tracker today) or 'exclusive' (target contract)
#'
#' @return Canonical tibble: hts10, cty_code, valid_from, valid_until (exclusive),
#'   rate_total, and any rate_<authority> columns present
normalize_panel <- function(raw, interval_end = 'inclusive') {

  # --- column name mapping (tracker -> canonical) ---
  rate_total_col <- case_when(
    'rate_total'  %in% names(raw) ~ 'rate_total',
    'total_rate'  %in% names(raw) ~ 'total_rate',
    TRUE                          ~ NA_character_
  )
  if (is.na(rate_total_col)) {
    stop('Rate panel has neither `rate_total` nor `total_rate` column. Columns: ',
         paste(names(raw), collapse = ', '))
  }

  cty_col <- if ('cty_code' %in% names(raw)) 'cty_code' else 'country'
  if (!cty_col %in% names(raw)) {
    stop('Rate panel has neither `cty_code` nor `country` column')
  }
  if (!'hts10' %in% names(raw)) {
    stop('Rate panel missing `hts10` column')
  }

  authority_cols <- names(raw)[str_detect(names(raw), '^rate_[a-z0-9_]+$') &
                                 names(raw) != 'rate_total']

  panel <- raw %>%
    transmute(
      hts10      = as.character(.data[['hts10']]),
      cty_code   = as.character(.data[[cty_col]]),
      rate_total = as.numeric(.data[[rate_total_col]]),
      across(all_of(authority_cols), as.numeric),
      valid_from = if ('valid_from' %in% names(raw)) as.Date(.data[['valid_from']]) else as.Date(NA),
      valid_until = if ('valid_until' %in% names(raw)) as.Date(.data[['valid_until']]) else as.Date(NA),
      effective_date = if ('effective_date' %in% names(raw)) as.Date(.data[['effective_date']]) else as.Date(NA)
    )

  # --- intervals: fall back to effective_date for single-revision snapshots ---
  # (today's per-revision snapshots carry effective_date but NA valid_from/until)
  no_intervals <- all(is.na(panel$valid_from))
  if (no_intervals) {
    if (all(is.na(panel$effective_date))) {
      stop('Rate panel has neither interval columns nor effective_date')
    }
    panel <- panel %>%
      mutate(valid_from = effective_date,
             valid_until = as.Date(NA))   # open-ended single snapshot
  }

  # --- converge end-date convention to EXCLUSIVE half-open ---
  if (interval_end == 'inclusive') {
    panel <- panel %>% mutate(valid_until = valid_until + 1L)
  } else if (interval_end != 'exclusive') {
    stop('rate_panel.interval_end must be "inclusive" or "exclusive", got: ', interval_end)
  }

  validate_panel(panel)

  return(panel %>% select(-effective_date))
}

#' Fail loud on contract violations in a canonical panel
validate_panel <- function(panel) {
  if (any(is.na(panel$hts10)) || any(is.na(panel$cty_code))) {
    stop('Rate panel has NA in key columns (hts10 / cty_code)')
  }
  if (any(is.na(panel$rate_total))) {
    stop('Rate panel has NA rate_total — missing values indicate an upstream bug')
  }
  # Fractions, not percentage points: a 4.0 (=400%) all-in rate is plausible; >20 is not
  if (max(panel$rate_total) > 20) {
    stop('rate_total appears to be in percentage points, not fractions (max = ',
         round(max(panel$rate_total), 1), '). Expected fractions per D8.')
  }
  invisible(panel)
}

#' Read one published series as a canonical interval panel
#'
#' @param rate_panel The `rate_panel:` config block
#' @param series 'actual' or a scenario name
#'
#' @return Canonical interval-encoded panel (see file header)
read_rate_panel <- function(rate_panel, series) {
  path <- resolve_panel_path(rate_panel, series)
  message(sprintf('  Reading rate panel [%s]: %s', series, path))
  raw <- read_panel_file(path)
  panel <- normalize_panel(raw, interval_end = rate_panel$interval_end %||% 'inclusive')
  message(sprintf('    %s rows, %d countries, %d intervals',
                  format(nrow(panel), big.mark = ','),
                  length(unique(panel$cty_code)),
                  length(unique(panel$valid_from))))
  return(panel)
}

read_rate_snapshot <- function(rate_panel, series, date) {
  path <- resolve_snapshot_path(rate_panel, series, date)
  message(sprintf('  Reading rate snapshot [%s @ %s]: %s', series, as.Date(date), path))
  raw <- read_panel_file(path)
  panel <- normalize_panel(raw, interval_end = rate_panel$interval_end %||% 'inclusive')
  sliced <- slice_panel_at(panel, date)
  message(sprintf('    %s rows, %d countries',
                  format(nrow(sliced), big.mark = ','),
                  length(unique(sliced$cty_code))))
  sliced
}

#' Slice a canonical interval panel to a static snapshot at one date
#'
#' Half-open semantics: the row in effect on `date` satisfies
#' valid_from <= date < valid_until (open-ended valid_until = NA always covers).
#'
#' @param panel Canonical interval panel
#' @param date A single Date
#'
#' @return Static panel (hts10, cty_code, rate_total[, rate_*]) — one row per pair
slice_panel_at <- function(panel, date) {
  date <- as.Date(date)

  sliced <- panel %>%
    filter(valid_from <= date,
           is.na(valid_until) | date < valid_until)

  # Each (hts10, cty_code) should be covered by exactly one interval. Use vctrs for a
  # C-level duplicate scan — a single-revision snapshot is ~5M rows where every row is
  # its own (hts10, cty_code) group, and dplyr count()/group_by on that is O(minutes).
  # Only pay for the detailed offender list if a duplicate actually exists (rare).
  if (vctrs::vec_duplicate_any(sliced[c('hts10', 'cty_code')])) {
    dups <- sliced %>% count(hts10, cty_code) %>% filter(n > 1)
    stop(sprintf('Overlapping intervals: %d (hts10, cty_code) pairs covered twice at %s',
                 nrow(dups), date))
  }

  return(sliced %>% select(-valid_from, -valid_until))
}
