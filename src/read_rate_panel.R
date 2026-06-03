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
# tariff-etrs and tariff-rate-tracker produce the SAME conceptual object — the
# effective tariff rate at HS10xcountry, with all USMCA / metal-content / exemption
# adjustments already reflected in the tracker output. `total_rate` (= base_rate +
# total_additional) is that combined effective rate; we read it as `rate_total`.
# =============================================================================

library(tidyverse)

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

  root <- rate_panel$root
  if (is.null(root)) {
    stop('rate_panel.root not set in model_params.yaml')
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
    return(arrow::read_parquet(path))
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

  # Each (hts10, cty_code) should be covered by exactly one interval
  dups <- sliced %>% count(hts10, cty_code) %>% filter(n > 1)
  if (nrow(dups) > 0) {
    stop(sprintf('Overlapping intervals: %d (hts10, cty_code) pairs covered twice at %s',
                 nrow(dups), date))
  }

  return(sliced %>% select(-valid_from, -valid_until))
}
