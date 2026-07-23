# =============================================================================
# calibrate_alpha.R
#
# Two-channel, log-ratio alpha calibration: the LEARNED reduced-form correction
# to GTAP's predicted, tariff-driven trade-weight shifts. Fit on the live
# 2024 -> 2025/26 episode; the two scalar parameters ship to the production
# pipeline (consumed ONLY in the revenue transition path).
#
# FAITHFUL PORT of tariff-etr-adj/code/04a_alpha_analysis.R. Adaptations:
#   - reads the shared panel + 2024 IMDB basket from output/calibration/;
#   - reuses Tariff-Model's HS10->GTAP crosswalk (lowercase) + partner map;
#   - reads GTAP-implied weights from this repo's own build_gtap_implied_weights
#     output (output/calibration/<scenario>/gtap_implied_weights.csv);
#   - writes alpha outputs to output/calibration/<scenario>/alpha/.
#
# Channels (each fit by WLS through the origin in log-ratio space):
#   within : log(s_act_after/s_act_before) ~ alpha_within  * log(s_gtap_after/s_gtap_before)
#   between: log(T_act_after/T_act_before) ~ alpha_between * log(T_gtap_after/T_gtap_before)
# delta-vs-delta (each side differenced vs its OWN baseline) kills the omega bug;
# alpha_within EXCLUDES commodity sectors (energy); alpha_between is PROVISIONAL.
#
# Deliverable: output/calibration/<scenario>/alpha/alpha_parameters.csv (the two
#   scalars + the commodity pass-through list = resources/calibration/gtap_commodity_sectors.csv).
# The shipped matrix anchor is the actual-2024 basket; alpha_schedule.csv (GTAP-
# baseline-anchored) is an AUDIT artifact only.
#
# Usage (from repo root): see calibrate.R --stage alpha
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

source('src/calibration/calibration_helpers.R')

TM_HS6_GTAP_CROSSWALK    <- 'resources/rate_aggregation/hs6_gtap_crosswalk.csv'
GTAP_COMMODITY_SECTORS   <- 'resources/calibration/gtap_commodity_sectors.csv'
IMDB_2024_FILE           <- file.path(imdb_cache_dir(), 'imdb_2024_hs10_country.csv')

EPS               <- 1e-6
MIN_GTAP_LOGSHIFT <- 1e-3

#' Calibrate the two substitution alphas for a scenario.
#'
#' @param scenario Scenario name; its panel and gtap_implied_weights must exist.
#' @return Invisibly, the alpha_parameters.csv path (NULL if GTAP weights absent).
calibrate_alpha <- function(scenario) {

  set_calib_window(scenario)   # window from the scenario's calibration: block
  out_root  <- calib_output_dir(scenario)
  panel_dir <- file.path(out_root, 'panel')
  alpha_dir <- file.path(out_root, 'alpha')
  dir.create(alpha_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- panel -> GTAP-grain cells (shared by panel + 2024 source) ----
  xwalk <- read_csv(TM_HS6_GTAP_CROSSWALK,
                    col_types = cols(.default = col_character())) %>%
    transmute(hs6_code, gtap_code = tolower(gtap_code)) %>%
    distinct(hs6_code, gtap_code)
  pmap <- load_gtap_partner_map()

  to_cells <- function(df) df %>%
    mutate(hs6_code = substr(hs10, 1, 6)) %>%
    left_join(xwalk, by = 'hs6_code') %>%
    left_join(pmap,  by = 'cty_code') %>%
    mutate(gtap_code = coalesce(gtap_code, 'unmapped'),
           partner   = coalesce(partner, 'row')) %>%
    filter(gtap_code != 'unmapped') %>%
    group_by(gtap_code, partner) %>%
    summarise(value = sum(value), .groups = 'drop')

  panel <- readRDS(file.path(panel_dir, 'panel.rds')) %>%
    filter(period %in% c('train', 'test'))

  # actual-2024 "before" basket (con_val). Build it from IMDB if not cached.
  if (!file.exists(IMDB_2024_FILE)) {
    msg('    2024 IMDB basket not cached; building from IMDB (12 months)...')
    source('src/calibration/imdb.R')
    imdb_2024 <- build_imdb_agg(sprintf('2024-%02d', 1:12)) %>%
      summarise(con_val_mo = sum(con_val_mo, na.rm = TRUE),
                cal_dut_mo = sum(cal_dut_mo, na.rm = TRUE),
                .by = c(hs10, cty_code))
    write_csv(imdb_2024, IMDB_2024_FILE)
  }
  act_before <- read_csv(IMDB_2024_FILE,
                         col_types = cols(hs10 = col_character(),
                                          cty_code = col_character(),
                                          con_val_mo = col_double(),
                                          cal_dut_mo = col_double())) %>%
    transmute(hs10, cty_code, value = con_val_mo) %>% to_cells()
  act_after <- panel %>% filter(period == 'train') %>%
    transmute(hs10, cty_code, value = con_val_mo) %>% to_cells()
  act_oos   <- panel %>% filter(period == 'test') %>%
    transmute(hs10, cty_code, value = con_val_mo) %>% to_cells()

  # ---- gated on the GTAP run ----
  gtap <- load_gtap_weights(scenario)
  if (is.null(gtap)) {
    msg('[*] GTAP-implied weights not found for %s -- skipping alpha calibration.', scenario)
    msg('    (run calibrate.R --stage gtap-weights first)')
    return(invisible(NULL))
  }
  msg('[*] GTAP-implied weights present -- two-channel log-ratio calibration.')

  commodity_codes <- read_csv(GTAP_COMMODITY_SECTORS, show_col_types = FALSE) %>%
    filter(include) %>% pull(gtap_code)

  gtap_cells <- function(basis_keep, period_keep = NULL) {
    g <- gtap %>% filter(basis == basis_keep)
    if (!is.null(period_keep)) g <- filter(g, period == period_keep)
    g %>% transmute(gtap_code, partner,
                    base_imports = baseline_imports, post_imports = postsim_imports,
                    s_gtap_before = baseline_share, s_gtap_after = postsim_share)
  }
  gtap_pa  <- gtap_cells('period_avg')
  gtap_mar <- gtap_cells('monthly', TEST_YM)
  have_oos <- nrow(gtap_mar) > 0
  if (!have_oos)
    msg('    NOTE: no monthly %s rows in the GTAP file -- OOS deferred.', TEST_YM)
  grid     <- gtap_pa %>% distinct(gtap_code, partner)
  comm_set <- sort(unique(gtap_pa$gtap_code))

  # ---- layer factorisation ----
  within_actual <- function(cells) grid %>%
    left_join(cells, by = c('gtap_code', 'partner')) %>%
    mutate(value = coalesce(value, 0)) %>%
    group_by(gtap_code) %>%
    mutate(tot = sum(value), s = if_else(tot > 0, value / tot, 0)) %>%
    ungroup() %>%
    transmute(gtap_code, partner, s, v = value)
  sa_before <- within_actual(act_before) %>% rename(s_act_before = s, v_act_before = v)
  sa_after  <- within_actual(act_after)  %>% rename(s_act_after  = s, v_act_after  = v)
  sa_oos    <- within_actual(act_oos)    %>% rename(s_act_after  = s, v_act_after  = v)

  T_actual <- function(cells) tibble(gtap_code = comm_set) %>%
    left_join(cells %>% filter(gtap_code %in% comm_set) %>%
                group_by(gtap_code) %>% summarise(ct = sum(value), .groups = 'drop'),
              by = 'gtap_code') %>%
    mutate(ct = coalesce(ct, 0)) %>%
    transmute(gtap_code, T = ct / sum(ct))
  Ta_before <- T_actual(act_before) %>% rename(T_act_before = T)
  Ta_after  <- T_actual(act_after)  %>% rename(T_act_after  = T)
  Ta_oos    <- T_actual(act_oos)    %>% rename(T_act_after  = T)
  gtap_T <- function(g) g %>% group_by(gtap_code) %>%
    summarise(cb = sum(base_imports), cp = sum(post_imports), .groups = 'drop') %>%
    mutate(T_gtap_before = cb / sum(cb), T_gtap_after = cp / sum(cp))
  gtap_T_pa  <- gtap_T(gtap_pa)
  gtap_T_mar <- if (have_oos) gtap_T(gtap_mar) else NULL

  # ---- build the two fit frames (and OOS twins) ----
  lr <- function(a, b) log(pmax(a, EPS)) - log(pmax(b, EPS))

  make_within <- function(gtap_layer, after_tbl) gtap_layer %>%
    left_join(sa_before, by = c('gtap_code', 'partner')) %>%
    left_join(after_tbl, by = c('gtap_code', 'partner')) %>%
    mutate(
      commodity_sector = gtap_code %in% commodity_codes,
      x = lr(s_gtap_after, s_gtap_before),
      y = lr(s_act_after,  s_act_before),
      w = base_imports,
      structural_zero = s_gtap_before <= 0 | s_gtap_after <= 0 |
                        s_act_before  <= 0 | s_act_after  <= 0,
      eff_alpha = if_else(abs(x) >= MIN_GTAP_LOGSHIFT, y / x, NA_real_),
      used_in_fit = !commodity_sector & !structural_zero &
                    abs(x) >= MIN_GTAP_LOGSHIFT & is.finite(x) & is.finite(y) & w > 0)
  within_fit <- make_within(gtap_pa, sa_after)
  within_oos <- if (have_oos) make_within(gtap_mar, sa_oos) else NULL

  make_between <- function(gtap_T_layer, after_tbl) gtap_T_layer %>%
    left_join(Ta_before, by = 'gtap_code') %>%
    left_join(after_tbl, by = 'gtap_code') %>%
    mutate(
      commodity_sector = gtap_code %in% commodity_codes,
      x = lr(T_gtap_after, T_gtap_before),
      y = lr(T_act_after,  T_act_before),
      w = cb,
      structural_zero = T_gtap_before <= 0 | T_gtap_after <= 0 |
                        T_act_before  <= 0 | T_act_after  <= 0,
      eff_alpha = if_else(abs(x) >= MIN_GTAP_LOGSHIFT, y / x, NA_real_),
      used_in_fit = !structural_zero &
                    abs(x) >= MIN_GTAP_LOGSHIFT & is.finite(x) & is.finite(y) & w > 0)
  between_fit <- make_between(gtap_T_pa, Ta_after)
  between_oos <- if (have_oos) make_between(gtap_T_mar, Ta_oos) else NULL

  # ---- fit the scalar alphas (WLS through the origin) ----
  wls_origin <- function(df) { d <- filter(df, used_in_fit)
    if (!nrow(d)) NA_real_ else sum(d$w * d$x * d$y) / sum(d$w * d$x^2) }
  alpha_within  <- wls_origin(within_fit)
  alpha_between <- wls_origin(between_fit)

  # ---- grain diagnostics (justify the scalar choice; not shipped) ----
  alpha_by <- function(df, grp) filter(df, used_in_fit) %>% group_by(.data[[grp]]) %>%
    summarise(a = sum(w * x * y) / sum(w * x^2), .groups = 'drop')
  rmse_w <- function(df, predict_fn) { d <- filter(df, used_in_fit)
    if (!nrow(d)) NA_real_ else sqrt(weighted.mean((d$y - predict_fn(d))^2, d$w)) }
  pred_fn <- function(grain, fit_df, scalar, grp = NULL) {
    if (grain == 'constant') function(d) scalar * d$x
    else { tab <- alpha_by(fit_df, grp)
           function(d) coalesce(left_join(d, tab, by = grp)$a, scalar) * d$x }
  }
  score_grains <- function(channel, fit_df, oos_df, scalar, grains) {
    do.call(rbind, lapply(names(grains), function(g) {
      f <- pred_fn(g, fit_df, scalar, grains[[g]])
      tibble(channel = channel, grain = g,
             rmse_in  = rmse_w(fit_df, f),
             rmse_oos = if (have_oos) rmse_w(oos_df, f) else NA_real_)
    }))
  }
  grain_scores <- bind_rows(
    score_grains('within', within_fit, within_oos, alpha_within,
                 list(constant = NULL, by_gtap = 'gtap_code', by_part = 'partner')),
    score_grains('between', between_fit, between_oos, alpha_between,
                 list(constant = NULL, by_gtap = 'gtap_code')))

  # ---- apply: adjusted (commodity x source) matrix on the period-avg run ----
  apply_within <- function(g) g %>%
    mutate(commodity_sector = gtap_code %in% commodity_codes,
           s_adj_raw = if_else(commodity_sector, pmax(s_gtap_after, EPS),
             exp(log(pmax(s_gtap_before, EPS)) + alpha_within * lr(s_gtap_after, s_gtap_before)))) %>%
    group_by(gtap_code) %>% mutate(s_adj = s_adj_raw / sum(s_adj_raw)) %>% ungroup()
  apply_between <- function(gT) gT %>%
    mutate(T_adj_raw = exp(log(pmax(T_gtap_before, EPS)) +
                           alpha_between * lr(T_gtap_after, T_gtap_before)),
           T_adj = T_adj_raw / sum(T_adj_raw))

  T_adj_pa <- apply_between(gtap_T_pa) %>% select(gtap_code, T_gtap_before, T_gtap_after, T_adj)
  schedule <- apply_within(gtap_pa) %>%
    left_join(T_adj_pa, by = 'gtap_code') %>%
    left_join(transmute(sa_before, gtap_code, partner, s_act_before), by = c('gtap_code', 'partner')) %>%
    left_join(transmute(sa_after,  gtap_code, partner, s_act_after),  by = c('gtap_code', 'partner')) %>%
    mutate(w_gtap = post_imports / sum(post_imports),
           w_adj  = T_adj * s_adj) %>%
    transmute(gtap_code, partner,
              T_gtap_before, T_gtap_after, T_adj,
              s_gtap_before, s_gtap_after, s_adj,
              w_gtap, w_adj, base_imports, commodity_sector,
              s_act_before, s_act_after) %>%
    arrange(gtap_code, partner)

  # ---- weighted-ETR ladder: statutory ETR @ GTAP vs two-channel-alpha weights ----
  # ANCHORED ON ACTUAL 2024 (the model's derived-imports convention): both lines
  # take the 2024 basket and apply GTAP's per-cell shift -- GTAP at full strength,
  # alpha at fitted strengths -- so the GTAP-baseline-vs-2024 level gap drops out.
  d2024 <- act_before %>% transmute(gtap_code, partner, d2024 = value)
  obs_w <- panel %>%
    mutate(hs6_code = substr(hs10, 1, 6)) %>%
    left_join(xwalk, by = 'hs6_code') %>% left_join(pmap, by = 'cty_code') %>%
    mutate(gtap_code = coalesce(gtap_code, 'unmapped'),
           partner   = coalesce(partner, 'row')) %>%
    filter(gtap_code != 'unmapped') %>%
    group_by(year_month, gtap_code, partner) %>%
    summarise(w_obs = sum(con_val_mo),
              rate  = sum(con_val_mo * rate_h2avg) / sum(con_val_mo), .groups = 'drop')

  mon <- gtap %>% filter(basis == 'monthly') %>%
    transmute(year_month = period, gtap_code, partner,
              gb = baseline_imports, gp = postsim_imports,
              s_gb = baseline_share, s_ga = postsim_share)
  mon_T <- mon %>% group_by(year_month, gtap_code) %>%
    summarise(cb = sum(gb), cp = sum(gp), .groups = 'drop') %>%
    group_by(year_month) %>%
    mutate(between_ratio = exp(lr(cp / sum(cp), cb / sum(cb)))) %>%
    ungroup() %>% select(year_month, gtap_code, between_ratio)
  wetr <- mon %>%
    mutate(commodity_sector = gtap_code %in% commodity_codes,
           within_ratio = exp(lr(s_ga, s_gb))) %>%
    left_join(mon_T, by = c('year_month', 'gtap_code')) %>%
    left_join(d2024, by = c('gtap_code', 'partner')) %>%
    inner_join(obs_w, by = c('year_month', 'gtap_code', 'partner')) %>%
    mutate(d2024 = coalesce(d2024, 0)) %>%
    filter(d2024 > 0, gb > 0, w_obs > 0) %>%
    mutate(w_gtap = d2024 * between_ratio * within_ratio,
           w_adj  = d2024 * between_ratio^alpha_between *
                    within_ratio^if_else(commodity_sector, 1, alpha_within)) %>%
    group_by(year_month) %>%
    summarise(etr_stat_obs   = sum(w_obs  * rate) / sum(w_obs),
              etr_stat_gtap  = sum(w_gtap * rate) / sum(w_gtap),
              etr_stat_alpha = sum(w_adj  * rate) / sum(w_adj), .groups = 'drop') %>%
    arrange(year_month)

  # ---- headline parameters ----
  gs <- grain_scores %>% filter(grain == 'constant')
  params <- tibble(
    channel      = c('within', 'between'),
    alpha        = c(alpha_within, alpha_between),
    neutral      = c(1, 1),
    n_obs        = c(sum(within_fit$used_in_fit), sum(between_fit$used_in_fit)),
    weight_total = c(sum(within_fit$w[within_fit$used_in_fit]),
                     sum(between_fit$w[between_fit$used_in_fit])),
    rmse_in      = c(gs$rmse_in[gs$channel == 'within'],  gs$rmse_in[gs$channel == 'between']),
    rmse_oos     = c(gs$rmse_oos[gs$channel == 'within'], gs$rmse_oos[gs$channel == 'between']),
    note         = c(if (have_oos) '' else 'PROVISIONAL -- OOS pending March GTAP run',
                     'PROVISIONAL -- confounder-prone (memo §5)'))

  # ---- write outputs ----
  write_csv(params,       file.path(alpha_dir, 'alpha_parameters.csv'))
  write_csv(schedule,     file.path(alpha_dir, 'alpha_schedule.csv'))   # AUDIT only
  write_csv(within_fit %>%
              transmute(gtap_code, partner, base_imports,
                        s_gtap_before, s_gtap_after, s_act_before, s_act_after,
                        gtap_logshift = x, act_logshift = y, w, eff_alpha,
                        commodity_sector, structural_zero, used_in_fit),
            file.path(alpha_dir, 'alpha_within_fit.csv'))
  write_csv(between_fit %>%
              transmute(gtap_code, T_gtap_before, T_gtap_after, T_act_before, T_act_after,
                        gtap_logshift = x, act_logshift = y, w, eff_alpha,
                        commodity_sector, structural_zero, used_in_fit),
            file.path(alpha_dir, 'alpha_between_fit.csv'))
  write_csv(grain_scores, file.path(alpha_dir, 'alpha_grain_scores.csv'))
  write_csv(wetr,         file.path(alpha_dir, 'weighted_etr_ladder.csv'))

  msg('    alpha_within  = %.4f  (n=%d cells, commodity sectors excluded)',
      alpha_within, params$n_obs[1])
  msg('    alpha_between = %.4f  (n=%d commodities) [PROVISIONAL]',
      alpha_between, params$n_obs[2])
  msg('    -> %s', file.path(alpha_dir, 'alpha_parameters.csv'))
  invisible(file.path(alpha_dir, 'alpha_parameters.csv'))
}
