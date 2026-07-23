# =============================================================================
# calibration_helpers.R
#
# Shared constants + the eta-calibration machinery for the in-repo calibration
# harness (src/calibration/*, driven by calibrate.R). FAITHFUL PORT of the
# eta/alpha-specific helpers from tariff-etr-adj/code/utils.R (which were
# themselves ported from tariff-etr-eval/code/R/08_eta_calibration.R).
#
# The calibration math below (group_agg, fit_twoway, assign_shape, solve_k_pos,
# ...) is preserved VERBATIM from the upstream repo so the migrated pipeline
# reproduces the shipped golden eta/alpha numbers bit-for-bit. Do NOT restyle or
# "tidy" these function bodies — any change risks silent numeric drift against
# the reproduction gate (tests/fixtures/eta_golden/).
#
# What is intentionally NOT ported here (Tariff-Model owns these differently):
#   - tracker path resolution (resolve_tracker_data_dir / local_paths.yaml):
#     TM resolves the tracker bundle via the scenario `rate_panel:` block and
#     src/read_rate_panel.R. The panel builder uses those instead.
#   - data/raw + data/processed dir layout: TM writes calibration artifacts to
#     output/calibration/<scenario>/ (gitignored).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

options(dplyr.summarise.inform = FALSE)

# ---------------------------------------------------------------------------
# Paths (relative to repo root; calibrate.R runs from the project root)
# ---------------------------------------------------------------------------
# Calibration-owned resources moved verbatim from tariff-etr-adj/resources/.
CALIB_RES_DIR <- 'resources/calibration'

# Per-scenario calibration output root on the shared interface tree (see
# src/paths.R). Helper for stage modules.
calib_output_dir <- function(scenario) {
  calibration_dir(scenario)
}

# ---------------------------------------------------------------------------
# Analysis window (config-driven; defaults reproduce the shipped IEEPA window)
# ---------------------------------------------------------------------------
# Etas are calibrated on [TRAIN_LO, TEST_YM); TEST_YM (the month after the train
# window) is held out for OOS error. TRAIN_HI is a label = the month immediately
# before TEST_YM. The descriptive ladder pulls months from LADDER_LO; months in
# [LADDER_LO, TRAIN_LO) are tagged period == "pre" and excluded from calibration.
#
# These are DEFAULTS. A scenario overrides the window through its
# model_params.yaml `calibration:` block (window_start / window_end and,
# optionally, test_month / ladder_start) via set_calib_window() below -- so the
# window has ONE source of truth, shared with the gtap-weights stage (which
# already reads the same block). A scenario with a missing or partial
# calibration block reproduces the historical window bit-for-bit, so the
# reproduction gate (tests/fixtures/eta_golden/) is unaffected by this change.
DEFAULT_TRAIN_LO  <- '2025-06'
DEFAULT_TRAIN_HI  <- '2026-02'
DEFAULT_LADDER_LO <- '2025-01'

# Mutable window globals, initialised to the defaults; set_calib_window() resets
# them from a scenario's calibration block before each stage runs.
TRAIN_LO  <- DEFAULT_TRAIN_LO
TRAIN_HI  <- DEFAULT_TRAIN_HI
TEST_YM   <- '2026-03'
LADDER_LO <- DEFAULT_LADDER_LO

# The month after `ym` (both "YYYY-MM").
next_month <- function(ym) {
  format(seq(as.Date(paste0(ym, '-01')), by = 'month', length.out = 2)[2], '%Y-%m')
}

#' Resolve the calibration window from a scenario and set the module-global
#' window constants (TRAIN_LO / TRAIN_HI / TEST_YM / LADDER_LO).
#'
#' @param scenario_or_params Either a scenario name (reads its model_params.yaml)
#'   or an already-parsed model_params list.
#' Every key falls back to its shipped default; TEST_YM defaults to the month
#' after window_end. A scenario with no `calibration:` block therefore reproduces
#' the historical window exactly. This is the single source of truth the panel,
#' eta, and alpha stages share with build_gtap_implied_weights.
set_calib_window <- function(scenario_or_params) {
  mp <- if (is.character(scenario_or_params)) {
    yaml::read_yaml(file.path('config', 'scenarios', scenario_or_params, 'model_params.yaml'))
  } else scenario_or_params
  cw <- mp$calibration %||% list()
  TRAIN_LO  <<- cw$window_start %||% DEFAULT_TRAIN_LO
  TRAIN_HI  <<- cw$window_end   %||% DEFAULT_TRAIN_HI
  TEST_YM   <<- cw$test_month   %||% next_month(TRAIN_HI)
  LADDER_LO <<- cw$ladder_start %||% DEFAULT_LADDER_LO
  stopifnot(
    'TRAIN_HI must be the month immediately before TEST_YM' =
      format(as.Date(paste0(TEST_YM, '-01')) - 1, '%Y-%m') == TRAIN_HI,
    'LADDER_LO must be <= TRAIN_LO' = LADDER_LO <= TRAIN_LO)
  invisible(list(train_lo = TRAIN_LO, train_hi = TRAIN_HI,
                 test_ym = TEST_YM, ladder_lo = LADDER_LO))
}

# EB shrinkage for the full-interaction spec: kappa = KAPPA_FRAC * mean
# statutory revenue per cell (cells with less revenue shrink harder).
KAPPA_FRAC <- 1.0

# The flat compliance assumption the calibration replaces (reference line).
CURRENT_COMPLIANCE <- 0.10

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
msg <- function(...) cat(sprintf(...), '\n')

# ---------------------------------------------------------------------------
# Partner groups (mirrors assign_partner_group in tariff-etr-eval programs.do)
# ---------------------------------------------------------------------------
# NOTE: this is the ETA-side taxonomy -- S. Korea is its OWN group. It is
# DISTINCT from the GTAP regional taxonomy used for the alpha join (which folds
# Korea into ftrow; see load_gtap_partner_map). Never cross-apply the two.
CTY_CHINA  <- '5700'
CTY_CANADA <- '1220'
CTY_MEXICO <- '2010'
CTY_JAPAN  <- '5880'
CTY_SKOREA <- '5800'
CTY_UK     <- '4120'

# EU27 member states (Census Schedule C country codes, verified against the
# tracker's resources/census_codes.csv and config/policy_params.yaml
# eu27_codes; the earlier globals.do list was wrong -- off-by-a-digit codes
# like 4270/4760 sent France, Italy, NL, BE, IE, AT, SE, DK, FI to ROW).
EU_CODES <- c('4330',  # Austria
              '4231',  # Belgium
              '4870',  # Bulgaria
              '4791',  # Croatia
              '4910',  # Cyprus
              '4351',  # Czech Republic
              '4099',  # Denmark (except Greenland)
              '4470',  # Estonia
              '4050',  # Finland
              '4279',  # France
              '4280',  # Germany
              '4840',  # Greece
              '4370',  # Hungary
              '4190',  # Ireland
              '4759',  # Italy
              '4490',  # Latvia
              '4510',  # Lithuania
              '4239',  # Luxembourg
              '4730',  # Malta
              '4210',  # Netherlands
              '4550',  # Poland
              '4710',  # Portugal
              '4850',  # Romania
              '4359',  # Slovakia
              '4792',  # Slovenia
              '4700',  # Spain
              '4010')  # Sweden

PARTNER_LEVELS <- c('China', 'Canada', 'Mexico', 'EU', 'Japan',
                    'S. Korea', 'UK', 'ROW')

assign_partner_group <- function(cty_code) {
  case_when(
    cty_code == CTY_CHINA   ~ 'China',
    cty_code == CTY_CANADA  ~ 'Canada',
    cty_code == CTY_MEXICO  ~ 'Mexico',
    cty_code == CTY_JAPAN   ~ 'Japan',
    cty_code == CTY_SKOREA  ~ 'S. Korea',
    cty_code == CTY_UK      ~ 'UK',
    cty_code %in% EU_CODES  ~ 'EU',
    TRUE                    ~ 'ROW'
  )
}

safe_divide <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# ---------------------------------------------------------------------------
# HS2 chapter names
# ---------------------------------------------------------------------------
load_hs2_names <- function() {
  read_csv(file.path(CALIB_RES_DIR, 'hs2_chapter_names.csv'),
           col_types = cols(hs2 = col_character(), hs2_name = col_character()))
}

# ---------------------------------------------------------------------------
# Calibration machinery (ported VERBATIM from 08_eta_calibration.R via etr-adj)
# ---------------------------------------------------------------------------
# Statutory weight depends on baseline:
#   "compadj"   (composition-adjusted) -> monthly consumption value
#   "announced" (2024 fixed basket)    -> 2024 annual import value
wcol_for <- function(baseline) {
  if (baseline == 'compadj') 'con_val_mo' else 'imports'
}

BASELINE_LABELS <- c(compadj   = 'composition-adjusted',
                     announced = 'announced (2024)')

# Aggregate cells to a grouping; return statutory ETR, Census ETR, raw (1-eta),
# statutory revenue (the regression/shrinkage weight), and trade value.
group_agg <- function(df, gvars, baseline) {
  w <- wcol_for(baseline)
  df %>%
    group_by(across(all_of(gvars))) %>%
    summarise(
      statrev = sum(.data[[w]] * rate_h2avg),
      statwt  = sum(.data[[w]]),
      actduty = sum(cal_dut_mo),
      conval  = sum(con_val_mo),
      .groups = 'drop') %>%
    mutate(setr = statrev / statwt,                 # statutory ETR
           aetr = actduty / conval,                 # Census actual ETR
           om_eta = if_else(setr > 0, aetr / setr, NA_real_))  # 1 - eta (raw)
}

# Two-way model: log(1-eta_cp) = a_c + b_p, weighted by statutory revenue.
# Returns a function mapping (c,p) -> predicted (1-eta), robust to unseen levels.
fit_twoway <- function(cp, c_name, p_name) {
  d <- cp %>% filter(is.finite(om_eta), om_eta > 0, statrev > 0)
  d$.c <- d[[c_name]]; d$.p <- d[[p_name]]
  fit <- lm(log(om_eta) ~ .c + .p, data = d, weights = statrev)
  co <- coef(fit)
  b0 <- co[['(Intercept)']]
  a_c <- co[grepl('^\\.c', names(co))]; names(a_c) <- sub('^\\.c', '', names(a_c))
  b_p <- co[grepl('^\\.p', names(co))]; names(b_p) <- sub('^\\.p', '', names(b_p))
  function(cc, pp) {
    ac <- ifelse(cc %in% names(a_c), a_c[cc], 0); ac[is.na(ac)] <- 0
    bp <- ifelse(pp %in% names(b_p), b_p[pp], 0); bp[is.na(bp)] <- 0
    as.numeric(exp(b0 + ac + bp))
  }
}

# Build the per-cell Census shape (1-eta) lookup for a (spec, granularity) on a
# given sample. Populates column `om` for every cell.
#   spec = "constant" : single economy-wide ratio
#   spec = "oneway"   : group ratio at one margin (c_name only; p_name NULL)
#   spec = "twoway"   : multiplicative country + product effects
#   spec = "full"     : per-(c,p) cell ratio, EB-shrunk toward the two-way fit
assign_shape <- function(df, spec, baseline, c_name = NULL, p_name = NULL) {
  if (spec == 'constant') {
    g <- group_agg(df, character(0), baseline)
    df$om <- g$aetr / g$setr
    return(list(df = df, frac_clipped = NA_real_))
  }
  glob_om <- { g <- group_agg(df, character(0), baseline); g$aetr / g$setr }

  if (spec == 'oneway') {
    gp <- group_agg(df, c_name, baseline) %>%
      select(all_of(c_name), om_use = om_eta)
    df <- df %>% left_join(gp, by = c_name)
    df$om <- coalesce(df$om_use, glob_om)
    df$om_use <- NULL
    return(list(df = df, frac_clipped = NA_real_))
  }

  gv <- c(c_name, p_name)
  cp <- group_agg(df, gv, baseline)
  tw <- fit_twoway(cp, c_name, p_name)
  cp$tw <- tw(cp[[c_name]], cp[[p_name]])
  frac_clipped <- NA_real_
  if (spec == 'twoway') {
    cp$om_use <- cp$tw
  } else {                                          # full interaction + EB shrink
    raw0 <- ifelse(is.finite(cp$om_eta), cp$om_eta, cp$tw)
    raw  <- pmin(pmax(raw0, 0), 2)                  # clip implausible cell ratios
    frac_clipped <- weighted.mean(raw0 != raw, w = pmax(cp$statrev, 0), na.rm = TRUE)
    kappa  <- KAPPA_FRAC * mean(cp$statrev[cp$statrev > 0])
    lambda <- cp$statrev / (cp$statrev + kappa)
    cp$om_use <- lambda * raw + (1 - lambda) * cp$tw
  }
  df <- df %>% left_join(select(cp, all_of(gv), om_use), by = gv)
  df$om <- df$om_use
  unseen <- is.na(df$om)                            # (c,p) not in this sample
  if (any(unseen)) df$om[unseen] <- tw(df[[c_name]][unseen], df[[p_name]][unseen])
  df$om[is.na(df$om)] <- glob_om                    # last-resort global fallback
  df$om_use <- NULL
  list(df = df, frac_clipped = frac_clipped)
}

# Apply a TRAIN-estimated shape to the test cells (freeze the calibration).
# Re-fits the two-way map on train to predict unseen test (c,p) combinations.
assign_shape_to_test <- function(te, tr, spec, baseline, c_name = NULL, p_name = NULL) {
  if (spec == 'constant') {
    te$om <- tr$om[1]
    return(te)
  }
  glob_om <- { g <- group_agg(tr, character(0), baseline); g$aetr / g$setr }

  if (spec == 'oneway') {
    look <- tr %>% distinct(across(all_of(c_name)), om)
    te <- te %>% left_join(look, by = c_name)
    te$om <- coalesce(te$om, glob_om)
    return(te)
  }

  gv <- c(c_name, p_name)
  look <- tr %>% distinct(across(all_of(gv)), om)
  cp_tr <- group_agg(tr, gv, baseline)
  tw <- fit_twoway(cp_tr, c_name, p_name)
  te <- te %>% left_join(look, by = gv)
  unseen <- is.na(te$om)
  if (any(unseen)) te$om[unseen] <- tw(te[[c_name]][unseen], te[[p_name]][unseen])
  te$om[is.na(te$om)] <- glob_om
  te
}

# Weighted cross-sectional RMSE of the predicted Census rate (om * stat) vs the
# observed Census ETR, value-weighted. Measures how well the *shape* fits.
xsec_rmse <- function(df) {
  pr <- df$om * df$rate_h2avg
  sqrt(sum(df$con_val_mo * (pr - df$census_etr)^2) / sum(df$con_val_mo))
}

# Predicted ETR under shape `om` (and optional level k) on baseline weights.
pred_etr <- function(df, baseline, om = df$om) {
  w <- wcol_for(baseline)
  sum(df[[w]] * om * df$rate_h2avg) / sum(df[[w]])
}

# ---------------------------------------------------------------------------
# Positives-only Treasury calibration (decision 2026-06-12; see
# code/k_positive_only_experiment.R for the OOS comparison that motivated it)
# ---------------------------------------------------------------------------
# The level factor k applies only to positive-eta cells (om < 1). Negative-eta
# cells (om >= 1: collections exceed the modeled statutory baseline -- Ch99 /
# IEEPA scope gaps, §201, specific duties) keep their raw Census ratio rather
# than being scaled by a Census->Treasury timing wedge that does not describe
# them. k is re-solved so the calibrated TRAIN aggregate still equals the
# Treasury ETR exactly:
#   k = (T_train * sum(w) - S_neg) / S_pos,  S_g = sum(w * om * rate) over g.
solve_k_pos <- function(df, baseline, target_etr) {
  w <- wcol_for(baseline)
  pos <- df$om < 1
  S <- function(m) sum(df[[w]][m] * df$om[m] * df$rate_h2avg[m])
  (target_etr * sum(df[[w]]) - S(!pos)) / S(pos)
}

# Calibrated (1 - eta) for a shape value under positives-only k.
apply_k_pos <- function(om, k) ifelse(om < 1, k * om, om)

# ---------------------------------------------------------------------------
# GTAP boundary I/O (alpha calibration)
# ---------------------------------------------------------------------------
# cty_code -> GTAP source region (china/canada/mexico/eu/uk/japan/ftrow/row).
# ftrow = "other FTA partners" (incl. Korea); anything unlisted defaults to row.
# This is the GTAP regional taxonomy used for the ALPHA join -- DISTINCT from the
# eta-side partner groups (which keep S. Korea separate; see assign_partner_group).
# Reuses Tariff-Model's own country_partner_mapping (identical cty_code->partner
# pairs to the upstream etr-adj copy, verified).
TM_PARTNER_MAP <- 'resources/rate_aggregation/2024/country_partner_mapping.csv'
load_gtap_partner_map <- function() {
  read_csv(TM_PARTNER_MAP,
           col_types = cols(cty_code = col_character(), partner = col_character(),
                            .default = col_character())) %>%
    dplyr::distinct(cty_code, partner)
}

# GTAP-implied trade weights, produced in-repo by build_gtap_implied_weights.R
# (calibrate.R --stage gtap-weights) at output/calibration/<scenario>/. Schema:
# period, basis (period_avg|monthly), window_start/end, gtap_code (lowercase),
# partner (8 GTAP source regions), baseline_imports/postsim_imports,
# baseline_share/postsim_share (source share WITHIN commodity, sum to 1).
# Returns NULL if absent so the alpha step can no-op cleanly.
load_gtap_weights <- function(scenario) {
  path <- file.path(calib_output_dir(scenario), 'gtap_implied_weights.csv')
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE)
}
