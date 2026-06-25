# =============================================================================
# check_eta_reproduction.R
#
# Reproduction gate: confirm the in-repo panel + eta calibration reproduce the
# golden eta_by_partner_gtap.csv (and the headline eta constant) that the
# upstream tariff-etr-adj repo shipped and the production pipeline consumes.
#
# Vintage-gated: the golden fixtures are stamped with the tracker vintage that
# produced them (tests/fixtures/eta_golden/<vintage>/). The gate fires only when
# the scenario's panel was built from that same vintage; otherwise it SKIPS with
# a clear message (a different vintage legitimately produces different etas).
#
# Usage (from repo root): see calibrate.R --stage check
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr)
})

FIXTURE_ROOT <- 'tests/fixtures/eta_golden'

#' Compare the scenario's eta outputs against the vintage-matched golden fixture.
#'
#' @param scenario Scenario name; its eta calibration must already have run.
#' @param tol Absolute tolerance for per-cell eta and the headline constant.
#' @return Invisibly TRUE if green (or skipped); stops on mismatch.
check_eta_reproduction <- function(scenario, tol = 1e-8) {

  out_root  <- calib_output_dir(scenario)
  panel_dir <- file.path(out_root, 'panel')

  # --- resolve the producing vintage from the rates sidecar ---
  meta_csv <- file.path(panel_dir, 'statutory_rates_meta.csv')
  vintage <- if (file.exists(meta_csv)) {
    read_csv(meta_csv, show_col_types = FALSE)$tracker_vintage[1]
  } else NA_character_

  fixture_dir <- file.path(FIXTURE_ROOT, vintage)
  if (is.na(vintage) || !dir.exists(fixture_dir)) {
    message(strrep('-', 64))
    message('eta reproduction gate: SKIPPED')
    message('  panel vintage: ', vintage %||% 'NA',
            ' has no golden fixture under ', FIXTURE_ROOT, '/')
    message('  (a non-fixture vintage legitimately produces different etas)')
    message(strrep('-', 64))
    return(invisible(TRUE))
  }

  message(strrep('=', 64))
  message('eta reproduction gate | scenario: ', scenario, ' | vintage: ', vintage)
  message(strrep('=', 64))

  fail <- character(0)

  # --- 1. slim contract: same keys, same eta ---
  got  <- read_csv(file.path(out_root, 'eta_by_partner_gtap.csv'),
                   col_types = cols(eta_group = col_character(),
                                    gtap_code = col_character(), eta = col_double()))
  gold <- read_csv(file.path(fixture_dir, 'eta_by_partner_gtap.csv'),
                   col_types = cols(eta_group = col_character(),
                                    gtap_code = col_character(), eta = col_double()))

  key <- function(d) paste(d$eta_group, d$gtap_code, sep = '|')
  missing_keys <- setdiff(key(gold), key(got))
  extra_keys   <- setdiff(key(got), key(gold))
  if (length(missing_keys) > 0)
    fail <- c(fail, sprintf('%d golden (eta_group,gtap_code) keys missing from output (e.g. %s)',
                            length(missing_keys), paste(head(missing_keys, 3), collapse = ', ')))
  if (length(extra_keys) > 0)
    fail <- c(fail, sprintf('%d extra keys in output not in golden (e.g. %s)',
                            length(extra_keys), paste(head(extra_keys, 3), collapse = ', ')))

  cmp <- inner_join(
    gold %>% transmute(k = key(.), eta_gold = eta),
    got  %>% transmute(k = key(.), eta_got  = eta),
    by = 'k'
  ) %>% mutate(adiff = abs(eta_gold - eta_got))
  max_diff <- if (nrow(cmp)) max(cmp$adiff) else NA_real_
  message(sprintf('  slim contract: %d golden rows, %d output rows, %d matched; max |Δeta| = %.3e',
                  nrow(gold), nrow(got), nrow(cmp), max_diff))
  if (is.na(max_diff) || max_diff > tol) {
    worst <- cmp %>% arrange(desc(adiff)) %>% head(10)
    diff_path <- file.path(out_root, 'eta_repro_diff.csv')
    write_csv(arrange(cmp, desc(adiff)), diff_path)
    fail <- c(fail, sprintf('per-cell eta max diff %.3e exceeds tol %.1e (worst dumped to %s)',
                            max_diff, tol, diff_path))
    message('  worst cells:'); print(as.data.frame(worst), row.names = FALSE)
  }

  # --- 2. headline constant (compadj eta_treas_agg) ---
  got_sum  <- read_csv(file.path(out_root, 'eta_summary.csv'), show_col_types = FALSE)
  gold_sum <- read_csv(file.path(fixture_dir, 'eta_summary.csv'), show_col_types = FALSE)
  pick_const <- function(d) d$eta_treas_agg[d$baseline == 'compadj' & d$spec == 'constant'][1]
  c_got <- pick_const(got_sum); c_gold <- pick_const(gold_sum)
  cdiff <- abs(c_got - c_gold)
  message(sprintf('  headline eta constant (compadj): got %.10f vs golden %.10f; |Δ| = %.3e',
                  c_got, c_gold, cdiff))
  if (!is.finite(cdiff) || cdiff > 1e-9)
    fail <- c(fail, sprintf('headline eta constant diff %.3e exceeds 1e-9', cdiff))

  # --- 3. strips applied must match ---
  strips_got  <- got_sum$strips_applied[1]
  strips_gold <- gold_sum$strips_applied[1]
  if (!identical(strips_got, strips_gold))
    fail <- c(fail, sprintf('strips_applied mismatch: got "%s" vs golden "%s"',
                            strips_got, strips_gold))

  message(strrep('=', 64))
  if (length(fail) == 0) {
    message('eta reproduction gate: PASS ✓')
    message(strrep('=', 64))
    return(invisible(TRUE))
  }
  message('eta reproduction gate: FAIL')
  for (f in fail) message('  - ', f)
  message(strrep('=', 64))
  stop('eta reproduction gate failed (see above).', call. = FALSE)
}
