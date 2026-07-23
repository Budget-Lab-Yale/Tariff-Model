# =============================================================================
# calibrate_eta.R
#
# Calibrate the eta (compliance-gap) schedule and aggregate it to the
# (eta_group x gtap_code) grain the production pipeline consumes via
# model_params.yaml `noncompliance.eta_file`.
#
# FAITHFUL PORT of tariff-etr-adj/code/02a_eta_analysis.R ([A] calibration +
# [B] partner x GTAP aggregation), run as one in-memory pass. Adaptations:
#   - reads the panel from output/calibration/<scenario>/panel/ (this repo's
#     build_imdb_actuals_panel output) instead of data/processed/;
#   - reuses Tariff-Model's own HS10->GTAP crosswalk (gtap_code_lc = tolower);
#   - writes the slim contract + eta_summary to output/calibration/<scenario>/.
#
# Primary deliverable: output/calibration/<scenario>/eta_by_partner_gtap.csv
#   (eta_group, gtap_code, eta) -- the 312-row slim contract the model joins on.
#
# Methodological invariants preserved verbatim (see calibration_helpers.R):
#   1 - eta_g = (value-weighted Census ETR) / (statutory ETR); shape from Census,
#   level pinned to Treasury via k on POSITIVE-eta cells only (negatives kept
#   raw, never clipped); compadj baseline is the shipped one; AD/CVD + de-minimis
#   strips hit the Treasury level only; TEST_YM held out.
#
# Usage (from repo root): see calibrate.R --stage eta
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

source('src/calibration/calibration_helpers.R')
source('src/calibration/adcvd_strip.R')
source('src/calibration/deminimis_strip.R')

TM_HS6_GTAP_CROSSWALK <- 'resources/rate_aggregation/hs6_gtap_crosswalk.csv'

#' Calibrate eta for a scenario and write the slim (eta_group x gtap_code) contract.
#'
#' @param scenario Scenario name (config/scenarios/<scenario>/); its panel must
#'   already be built (build_imdb_actuals_panel).
#' @return Invisibly, the deliverable path.
calibrate_eta <- function(scenario) {

  set_calib_window(scenario)   # window from the scenario's calibration: block
  out_root  <- calib_output_dir(scenario)
  panel_dir <- file.path(out_root, 'panel')
  dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

  # composition-adjusted etas are the shipped path; the announced baseline is
  # computed alongside for the bias-variance table but never shipped.
  compadj_only <- function(df) dplyr::filter(df, baseline == 'compadj')

  # ---------------------------------------------------------------------------
  # 1. Load panel + revenue + tracker vintage sidecar
  # ---------------------------------------------------------------------------
  msg('[1] Loading panel...')
  panel <- readRDS(file.path(panel_dir, 'panel.rds'))
  rev   <- read_csv(file.path(panel_dir, 'revenue_monthly.csv'),
                    col_types = cols(year_month = col_character(),
                                     .default = col_double()))

  rates_meta_csv <- file.path(panel_dir, 'statutory_rates_meta.csv')
  TRACKER_VINTAGE <- if (file.exists(rates_meta_csv)) {
    read_csv(rates_meta_csv, show_col_types = FALSE)$tracker_vintage[1]
  } else NA_character_
  msg('    tracker vintage: %s', TRACKER_VINTAGE)

  # AD/CVD strip (Treasury level; Census shape untouched) then de-minimis strip
  # (Treasury level, months >= onset). Both BEFORE tr/te/treas_* are derived.
  adcvd        <- load_adcvd_collected()
  adcvd_active <- !is.null(adcvd)
  if (adcvd_active) {
    st    <- apply_adcvd_strip(panel, rev, adcvd)
    panel <- st$panel
    rev   <- st$rev
  }
  dm               <- apply_deminimis_strip(panel, rev)
  rev              <- dm$rev
  deminimis_active <- dm$active

  tr <- filter(panel, period == 'train')
  te <- filter(panel, period == 'test')

  rev_tr <- filter(rev, year_month >= TRAIN_LO, year_month != TEST_YM)
  treas_train_etr <- sum(rev_tr$customs_duties) / sum(rev_tr$imports_value)
  treas_test_etr  <- rev$treas_etr[rev$year_month == TEST_YM]
  msg('    train cells: %s | test cells: %s',
      format(nrow(tr), big.mark = ','), format(nrow(te), big.mark = ','))
  msg('    Treasury ETR: train(pooled)=%.4f  test(%s)=%.4f',
      treas_train_etr, TEST_YM, treas_test_etr)

  # ---------------------------------------------------------------------------
  # 2. Calibrate one (baseline x spec x granularity) and score it OOS
  # ---------------------------------------------------------------------------
  calibrate <- function(baseline, spec, c_name = NULL, p_name = NULL, gran_label = '') {
    w <- wcol_for(baseline)

    sh_tr <- assign_shape(tr, spec, baseline, c_name, p_name)
    tr_s  <- sh_tr$df
    te_s  <- assign_shape_to_test(te, tr_s, spec, baseline, c_name, p_name)

    k <- solve_k_pos(tr_s, baseline, treas_train_etr)

    test_pred <- pred_etr(te_s, baseline, om = apply_k_pos(te_s$om, k))
    test_err  <- test_pred - treas_test_etr

    eta_agg <- 1 - (sum(tr_s[[w]] * tr_s$om * tr_s$rate_h2avg) /
                    sum(tr_s[[w]] * tr_s$rate_h2avg))
    eta_treas_agg <- 1 - (sum(tr_s[[w]] * apply_k_pos(tr_s$om, k) * tr_s$rate_h2avg) /
                          sum(tr_s[[w]] * tr_s$rate_h2avg))

    tibble(baseline = baseline, spec = spec, granularity = gran_label,
           eta_agg = eta_agg,
           eta_treas_agg = eta_treas_agg,
           k_train = k,
           train_xsec_rmse = xsec_rmse(tr_s),
           test_xsec_rmse  = xsec_rmse(te_s),
           frac_clipped = sh_tr$frac_clipped,
           test_pred_treas_etr = test_pred,
           test_actual_treas_etr = treas_test_etr,
           test_err_pp = 100 * test_err)
  }

  msg('[2] Calibrating all specifications...')
  grid <- list(
    list(spec = 'constant', c = NULL,            p = NULL,  lab = '—'),
    list(spec = 'oneway',   c = 'hs2',           p = NULL,  lab = 'hs2'),
    list(spec = 'oneway',   c = 'partner_group', p = NULL,  lab = 'partner_group'),
    list(spec = 'oneway',   c = 'cty_code',      p = NULL,  lab = 'country'),
    list(spec = 'twoway',   c = 'partner_group', p = 'hs2', lab = 'partner_group x hs2'),
    list(spec = 'twoway',   c = 'cty_code',      p = 'hs2', lab = 'country x hs2'),
    list(spec = 'full',     c = 'partner_group', p = 'hs2', lab = 'partner_group x hs2'),
    list(spec = 'full',     c = 'cty_code',      p = 'hs2', lab = 'country x hs2')
  )

  eta_summary <- bind_rows(lapply(c('announced', 'compadj'), function(bl) {
    bind_rows(lapply(grid, function(g) {
      msg('    %s / %-8s / %s', bl, g$spec, g$lab)
      calibrate(bl, g$spec, g$c, g$p, g$lab)
    }))
  })) %>%
    mutate(baseline_label = BASELINE_LABELS[baseline], .after = baseline) %>%
    mutate(tracker_vintage = TRACKER_VINTAGE,
           strips_applied = paste0(c(if (adcvd_active) 'adcvd',
                                     if (deminimis_active) 'deminimis',
                                     if (!adcvd_active && !deminimis_active) 'none'),
                                   collapse = '+'))

  write_csv(compadj_only(eta_summary), file.path(out_root, 'eta_summary.csv'))
  msg('    -> eta_summary.csv (compadj %d rows)', nrow(compadj_only(eta_summary)))

  # ---------------------------------------------------------------------------
  # 3. Preferred two-way fits + Treasury level factors (per baseline)
  # ---------------------------------------------------------------------------
  msg('[3] Building preferred two-way fits...')
  twoway_fit <- function(baseline) {
    sh <- assign_shape(tr, 'twoway', baseline, 'partner_group', 'hs2')
    k  <- solve_k_pos(sh$df, baseline, treas_train_etr)
    cp <- group_agg(tr, c('partner_group', 'hs2'), baseline)
    tw <- fit_twoway(cp, 'partner_group', 'hs2')
    list(tw = tw, k = k, cp = cp, df = sh$df)
  }
  constant_eta <- function(baseline) {
    g <- group_agg(tr, character(0), baseline)
    list(om = g$aetr / g$setr,
         eta_treas = 1 - treas_train_etr / g$setr)
  }

  fits   <- list(announced = twoway_fit('announced'), compadj = twoway_fit('compadj'))
  consts <- list(announced = constant_eta('announced'), compadj = constant_eta('compadj'))
  msg('    constants (Treasury-calibrated): announced=%.4f  compadj=%.4f',
      consts$announced$eta_treas, consts$compadj$eta_treas)

  # ---------------------------------------------------------------------------
  # 4. The HS2 x country eta schedule (in-memory; feeds [B])
  # ---------------------------------------------------------------------------
  msg('[4] Building the HS2 x country eta schedule...')
  hs2_names <- load_hs2_names()
  countries <- tr %>%
    distinct(cty_code) %>%
    mutate(partner_group = assign_partner_group(cty_code))
  chapters <- tr %>% distinct(hs2)

  cell_wt <- function(baseline) {
    group_agg(tr, c('cty_code', 'hs2'), baseline) %>%
      transmute(cty_code, hs2, 'statrev_train_{baseline}' := statrev)
  }

  schedule <- crossing(countries, chapters) %>%
    mutate(
      eta_twoway_announced   = 1 - apply_k_pos(
        fits$announced$tw(partner_group, hs2), fits$announced$k),
      eta_twoway_compadj     = 1 - apply_k_pos(
        fits$compadj$tw(partner_group, hs2), fits$compadj$k),
      eta_constant_announced = consts$announced$eta_treas,
      eta_constant_compadj   = consts$compadj$eta_treas
    ) %>%
    left_join(cell_wt('announced'), by = c('cty_code', 'hs2')) %>%
    left_join(cell_wt('compadj'),   by = c('cty_code', 'hs2')) %>%
    mutate(across(starts_with('statrev_train_'), ~ coalesce(., 0))) %>%
    left_join(hs2_names, by = 'hs2') %>%
    relocate(hs2, hs2_name, cty_code, partner_group) %>%
    arrange(hs2, cty_code)

  # ---------------------------------------------------------------------------
  # [B] Aggregate to (eta_group x gtap_code): the slim model-handoff contract
  # ---------------------------------------------------------------------------
  msg('[B] Aggregating to partner group x GTAP...')
  panel_tr <- filter(panel, period == 'train')

  # TM's own HS6->GTAP crosswalk (uppercase gtap_code; derive gtap_code_lc). GTAP
  # sector is determined by the 6-digit heading, so we join on the HS6 prefix.
  xwalk <- read_csv(TM_HS6_GTAP_CROSSWALK,
                    col_types = cols(.default = col_character())) %>%
    transmute(hs6_code, gtap_code, gtap_code_lc = tolower(gtap_code)) %>%
    distinct(hs6_code, gtap_code, gtap_code_lc)

  eta_pg_hs2 <- schedule %>%
    distinct(partner_group, hs2, eta_twoway_announced, eta_twoway_compadj)

  cells <- panel_tr %>%
    summarise(statrev_compadj   = sum(con_val_mo * rate_h2avg),
              statrev_announced = sum(imports * rate_h2avg),
              conval            = sum(con_val_mo),
              .by = c(hs10, partner_group, hs2)) %>%
    mutate(hs6_code = substr(hs10, 1, 6)) %>%
    left_join(xwalk, by = 'hs6_code') %>%
    left_join(eta_pg_hs2, by = c('partner_group', 'hs2'))

  unmatched <- cells %>% filter(is.na(gtap_code))
  msg('    crosswalk coverage: %.2f%% of cells, %.2f%% of trade value unmatched',
      100 * nrow(unmatched) / nrow(cells),
      100 * sum(unmatched$conval) / sum(cells$conval))
  cells <- cells %>%
    mutate(gtap_code    = coalesce(gtap_code, 'UNMAPPED'),
           gtap_code_lc = coalesce(gtap_code_lc, 'unmapped'))

  wmean <- function(x, w) {
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(x[ok] * w[ok]) / sum(w[ok])
  }

  eta_group_map <- c(China = 'china', Canada = 'canada', Mexico = 'mexico',
                     UK = 'uk', Japan = 'japan', EU = 'eu',
                     'S. Korea' = 'korea', ROW = 'row')
  gtap_prime <- cells %>%
    group_by(partner_group, gtap_code_lc) %>%
    summarise(eta = wmean(eta_twoway_compadj, statrev_compadj), .groups = 'drop') %>%
    filter(gtap_code_lc != 'unmapped', !is.na(eta)) %>%
    transmute(eta_group = unname(eta_group_map[partner_group]),
              gtap_code = gtap_code_lc, eta) %>%
    arrange(eta_group, gtap_code)

  stopifnot('eta_group label unmapped' = !any(is.na(gtap_prime$eta_group)),
            '(eta_group, gtap_code) not unique' =
              !any(duplicated(gtap_prime[c('eta_group', 'gtap_code')])))

  below <- filter(gtap_prime, eta < -0.5)
  if (nrow(below) > 0) {
    msg('    WARNING: %d cell(s) have eta < -0.5 (model guard eta_prime > 1.5):', nrow(below))
    print(as.data.frame(below))
  }

  out <- file.path(out_root, 'eta_by_partner_gtap.csv')
  write_csv(gtap_prime, out)
  msg('    -> %s (%d rows; eta_group x gtap_code x eta; min eta=%.3f)',
      out, nrow(gtap_prime), min(gtap_prime$eta))

  invisible(out)
}
