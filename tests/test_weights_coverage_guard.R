# =============================================================================
# Tests: weights coverage guard + manifest vintage assertion (Phase 5, model side)
# =============================================================================
#
# Covers the corrected-direction coverage guard in src/00a_prepare_rate_inputs.R
# (.rate_coverage_tiers / enforce_rate_coverage) and the manifest vintage
# assertion in src/read_rate_panel.R (assert_weights_vintage). Base-R stopifnot
# assertions on in-memory / tempfile fixtures — no bundle, no GTAP, no arrow.
#
# Run from the repo root:
#   Rscript tests/test_weights_coverage_guard.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})
source('src/00a_prepare_rate_inputs.R')   # pulls read_rate_panel.R + interfaces.R

pass_count <- 0
fail_count <- 0
run_test <- function(name, expr) {
  tryCatch({
    force(expr)
    message('  PASS: ', name)
    pass_count <<- pass_count + 1
  }, error = function(e) {
    message('  FAIL: ', name, ' — ', conditionMessage(e))
    fail_count <<- fail_count + 1
  })
}
# Capture only the intended stop() so noisy diagnostics don't fail the test.
errs <- function(expr) tryCatch({ suppressMessages(force(expr)); NA_character_ },
                                error = function(e) conditionMessage(e))
oks  <- function(expr) tryCatch({ suppressMessages(force(expr)); TRUE },
                                error = function(e) FALSE)

# =============================================================================
# 1. .rate_coverage_tiers classification
# =============================================================================
message('\n--- 1: tier classification ---')

run_test('classifies exact / hs8 / hs6 / country_global / unresolved', {
  rate_slice <- tibble(hts10    = c('0101010000', '0202020000'),
                       cty_code = c('1111', '1111'),
                       rate_total = c(0.10, 0.10))
  frame <- tibble(
    hts10    = c('0101010000', '0101010099', '0101019999', '9999999999', '9999999999'),
    cty_code = c('1111',       '1111',       '1111',       '1111',       '7777'),
    imports  = c(100, 50, 25, 10, 5),
    level    = c(0.10, NA, NA, NA, NA))
  tiers <- .rate_coverage_tiers(frame, rate_slice, 'level')
  stopifnot(identical(tiers,
    c('exact', 'hs8', 'hs6', 'country_global', 'unresolved')))
})

# =============================================================================
# 2. enforce_rate_coverage — tip vs historical, unresolved, thresholds
# =============================================================================
message('\n--- 2: coverage enforcement ---')

# Frame helpers: all pairs share cty 1111; the rate_slice determines the tier.
rate_slice <- tibble(hts10 = '0101010000', cty_code = '1111', rate_total = 0.10)

run_test('tip slice with full exact coverage passes', {
  frame <- tibble(hts10 = '0101010000', cty_code = '1111', imports = 100, level = 0.10)
  stopifnot(oks(enforce_rate_coverage(frame, rate_slice, 'level', 'scenario', is_tip = TRUE)))
})

run_test('tip slice with ANY prefix fallback hard-fails', {
  # second pair shares HS8 with the slice but has no exact match -> hs8 fallback
  frame <- tibble(hts10 = c('0101010000', '0101010099'),
                  cty_code = c('1111', '1111'),
                  imports = c(100, 900), level = c(0.10, NA))
  e <- errs(enforce_rate_coverage(frame, rate_slice, 'level', 'scenario', is_tip = TRUE))
  stopifnot(!is.na(e), grepl('TIP slice requires', e))
})

run_test('high-value unresolved pair fails even at tiny row count', {
  frame <- tibble(hts10 = c('0101010000', '9999999999'),
                  cty_code = c('1111', '7777'),
                  imports = c(1, 1e9), level = c(0.10, NA))
  e <- errs(enforce_rate_coverage(frame, rate_slice, 'level', 'scenario', is_tip = FALSE))
  stopifnot(!is.na(e), grepl('NO rate at any tier', e))
})

run_test('historical fallback under threshold passes', {
  # 5% of $ falls to hs8; threshold 15% -> ok
  frame <- tibble(hts10 = c('0101010000', '0101010099'),
                  cty_code = c('1111', '1111'),
                  imports = c(95, 5), level = c(0.10, NA))
  stopifnot(oks(enforce_rate_coverage(frame, rate_slice, 'level', 'scenario', is_tip = FALSE)))
})

run_test('historical fallback over threshold fails', {
  # 40% of $ falls to hs8; threshold 15% -> fail
  frame <- tibble(hts10 = c('0101010000', '0101010099'),
                  cty_code = c('1111', '1111'),
                  imports = c(60, 40), level = c(0.10, NA))
  e <- errs(enforce_rate_coverage(frame, rate_slice, 'level', 'scenario', is_tip = FALSE))
  stopifnot(!is.na(e), grepl('policy threshold', e))
})

run_test('zero-import-free positive frame reports 100% exact (no false alarm)', {
  frame <- tibble(hts10 = c('0101010000', '0101010000'),
                  cty_code = c('1111', '2222'),
                  imports = c(100, 200), level = c(0.10, 0.10))
  rs <- tibble(hts10 = c('0101010000', '0101010000'),
               cty_code = c('1111', '2222'), rate_total = c(0.10, 0.10))
  stopifnot(oks(enforce_rate_coverage(frame, rs, 'level', 'scenario', is_tip = TRUE)))
})

# =============================================================================
# 3. assert_weights_vintage — manifest <-> weights vintage tie
# =============================================================================
message('\n--- 3: manifest vintage assertion ---')

# Build a fake bundle: <root>/vtest/{manifest.json, weights/...csv.gz}
make_bundle <- function(tip_revision = '2026_rev_11',
                        snapshot_revision = '2026_rev_11',
                        weights_hts_vintage = '2026_rev_11',
                        manifest_weights_path = 'weights/import_weights_hs10_country.csv.gz',
                        drop_revision = FALSE) {
  root <- tempfile('bundle_'); dir.create(root)
  vdir <- file.path(root, 'vtest'); dir.create(vdir)
  wdir <- file.path(vdir, 'weights'); dir.create(wdir)
  wfile <- file.path(wdir, 'import_weights_hs10_country.csv.gz')
  readr::write_csv(tibble(hts10 = c('0101010000', '0202020000'),
                          country = c('1111', '1111'),
                          imports = c(100, 50),
                          import_value_year = 2024L,
                          hts_vintage = weights_hts_vintage), wfile)
  snap <- list(valid_from = '2026-07-01',
               path = 'actual/snapshots/valid_from=2026-07-01/rates.parquet')
  if (!drop_revision) snap$revision <- snapshot_revision
  older <- list(valid_from = '2025-01-01', revision = 'basic',
                path = 'actual/snapshots/valid_from=2025-01-01/rates.parquet')
  man <- list(
    series = list(actual = list(snapshots = list(older, snap))),
    weights = list(present = TRUE, path = manifest_weights_path))
  write_json(man, file.path(vdir, 'manifest.json'), auto_unbox = TRUE, pretty = TRUE)
  list(root = root, rate_panel = list(root = root, vintage = 'vtest'))
}

# Force the fake root (output_roots.yaml would otherwise win in resolve_*_root).
old_env <- Sys.getenv('TARIFF_RATE_TRACKER_ROOT', unset = NA_character_)

run_test('happy path: weights hts_vintage == manifest tip revision', {
  b <- make_bundle()
  Sys.setenv(TARIFF_RATE_TRACKER_ROOT = b$root)
  res <- suppressMessages(assert_weights_vintage(b$rate_panel))
  stopifnot(res$tip_revision == '2026_rev_11', res$hts_vintage == '2026_rev_11')
})

run_test('missing manifest revision fails actionably', {
  b <- make_bundle(drop_revision = TRUE)
  Sys.setenv(TARIFF_RATE_TRACKER_ROOT = b$root)
  e <- errs(assert_weights_vintage(b$rate_panel))
  stopifnot(!is.na(e), grepl('no `revision`', e), grepl('Republish', e))
})

run_test('weights vintage != manifest tip revision fails', {
  b <- make_bundle(weights_hts_vintage = '2026_rev_10')
  Sys.setenv(TARIFF_RATE_TRACKER_ROOT = b$root)
  e <- errs(assert_weights_vintage(b$rate_panel))
  stopifnot(!is.na(e), grepl('vintage mismatch', e))
})

run_test('multiple hts_vintage values fail', {
  root <- tempfile('bundle_'); dir.create(root)
  vdir <- file.path(root, 'vtest'); dir.create(vdir)
  wdir <- file.path(vdir, 'weights'); dir.create(wdir)
  readr::write_csv(tibble(hts10 = c('0101010000', '0202020000'),
                          country = c('1111', '1111'), imports = c(100, 50),
                          import_value_year = 2024L,
                          hts_vintage = c('2026_rev_11', '2026_rev_10')),
                   file.path(wdir, 'import_weights_hs10_country.csv.gz'))
  man <- list(series = list(actual = list(snapshots = list(
                list(valid_from = '2026-07-01', revision = '2026_rev_11',
                     path = 'actual/snapshots/valid_from=2026-07-01/rates.parquet')))),
              weights = list(path = 'weights/import_weights_hs10_country.csv.gz'))
  write_json(man, file.path(vdir, 'manifest.json'), auto_unbox = TRUE)
  Sys.setenv(TARIFF_RATE_TRACKER_ROOT = root)
  e <- errs(assert_weights_vintage(list(root = root, vintage = 'vtest')))
  stopifnot(!is.na(e), grepl('distinct non-NA hts_vintage', e))
})

run_test('manifest weights.path disagreement fails', {
  b <- make_bundle(manifest_weights_path = 'weights/some_other_file.csv.gz')
  Sys.setenv(TARIFF_RATE_TRACKER_ROOT = b$root)
  e <- errs(assert_weights_vintage(b$rate_panel))
  stopifnot(!is.na(e), grepl('does not resolve to the weights file', e))
})

run_test('missing manifest.json fails actionably', {
  root <- tempfile('bundle_'); dir.create(root)
  dir.create(file.path(root, 'vtest'))
  Sys.setenv(TARIFF_RATE_TRACKER_ROOT = root)
  e <- errs(assert_weights_vintage(list(root = root, vintage = 'vtest')))
  stopifnot(!is.na(e), grepl('manifest.json not found', e))
})

# restore env
if (is.na(old_env)) Sys.unsetenv('TARIFF_RATE_TRACKER_ROOT') else Sys.setenv(TARIFF_RATE_TRACKER_ROOT = old_env)

# =============================================================================
message('\n==================================================')
message('Coverage-guard tests: ', pass_count, ' passed, ', fail_count, ' failed')
message('==================================================')
if (fail_count > 0) quit(status = 1)
