# =============================================================================
# deminimis_strip.R — remove carrier-remitted de-minimis duty from Treasury
# =============================================================================
# FAITHFUL PORT of tariff-etr-adj/code/deminimis_strip.R (logic verbatim).
#
# WHY: when duty-free de-minimis ended (China/HK 2025-05-02, globally 2025-08-29
# under EO 14324), the small-parcel POSTAL channel began paying duty by monthly
# CARRIER REMITTANCE — no entry summaries, so the dollars reach Treasury
# customs_duties with no Census cal_dut_mo counterpart (same collection-channel
# class as AD/CVD). Stripping it makes the deliverable eta a FORMAL-ENTRY
# compliance parameter: statutory x (1 - eta) reproduces formal-entry
# collections, and the Tariff Model must add postal-channel revenue SEPARATELY.
#
# ESTIMATOR: the step in the Treasury-minus-Census duty gap — mean monthly gap
# over train months >= DEMINIMIS_ONSET minus the mean over the pre-break baseline
# [DEMINIMIS_PRE_LO, DEMINIMIS_ONSET) (~$1.0B/mo). Differencing removes the common
# AD/CVD structural gap + payment timing. TRAIN months only (TEST_YM never enters
# the estimate, but IS stripped, so the OOS comparison stays apples-to-apples).
#
# DO EXACTLY ONE (double-count guard): strip the postal duty here XOR hand the
# model a total-collections eta. With the strip active the model MUST add
# postal-channel revenue back outside eta.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr)
})

# Master switch (set FALSE for a total-collections sensitivity run).
DEMINIMIS_STRIP_ENABLED <- TRUE

# First full month of the global (EO 14324) de-minimis duty channel.
DEMINIMIS_ONSET  <- '2025-09'
# Post-ramp, pre-break baseline months start here (IEEPA rates settled).
DEMINIMIS_PRE_LO <- '2025-06'

#' Estimate the carrier-remitted de-minimis duty, $/month, from the gap step.
#'
#' @return list(step_usd, pre_gap, post_gap, n_pre, n_post). step_usd is NA if
#'   either segment has < 2 usable months.
estimate_deminimis_step <- function(panel, rev) {
  cen <- panel %>%
    group_by(year_month) %>%
    summarise(cen_duty = sum(cal_dut_mo), .groups = 'drop')
  gaps <- rev %>%
    inner_join(cen, by = 'year_month') %>%
    filter(year_month != TEST_YM) %>%                  # train evidence only
    mutate(gap = customs_duties * 1e6 - cen_duty)
  pre  <- gaps %>% filter(year_month >= DEMINIMIS_PRE_LO,
                          year_month <  DEMINIMIS_ONSET)
  post <- gaps %>% filter(year_month >= DEMINIMIS_ONSET)
  if (nrow(pre) < 2 || nrow(post) < 2)
    return(list(step_usd = NA_real_, pre_gap = NA_real_, post_gap = NA_real_,
                n_pre = nrow(pre), n_post = nrow(post)))
  list(step_usd = mean(post$gap) - mean(pre$gap),
       pre_gap  = mean(pre$gap), post_gap = mean(post$gap),
       n_pre = nrow(pre), n_post = nrow(post))
}

#' Strip the estimated de-minimis duty from Treasury for months >= onset.
#'
#' Treasury side ONLY — the postal channel has no Census counterpart.
#'
#' @return list(rev = stripped rev, active = logical, step_usd = $/month).
apply_deminimis_strip <- function(panel, rev) {
  if (!DEMINIMIS_STRIP_ENABLED) {
    msg('    De-minimis strip: DISABLED — Treasury level unchanged.')
    return(list(rev = rev, active = FALSE, step_usd = NA_real_))
  }
  est <- estimate_deminimis_step(panel, rev)
  if (!is.finite(est$step_usd)) {
    msg('    De-minimis strip: not estimable (%d pre / %d post months) — no-op.',
        est$n_pre, est$n_post)
    return(list(rev = rev, active = FALSE, step_usd = NA_real_))
  }
  if (est$step_usd <= 0) {
    msg('    De-minimis strip: estimated step $%.3gB/mo <= 0 — no-op (check the gap series).',
        est$step_usd / 1e9)
    return(list(rev = rev, active = FALSE, step_usd = NA_real_))
  }

  hit <- rev$year_month >= DEMINIMIS_ONSET
  rev$customs_duties[hit] <- pmax(rev$customs_duties[hit] - est$step_usd / 1e6, 0)
  if ('imports_value' %in% names(rev))
    rev <- mutate(rev, treas_etr = safe_divide(customs_duties, imports_value))

  msg('    De-minimis strip: removed $%.3fB/mo from Treasury for %d months >= %s',
      est$step_usd / 1e9, sum(hit), DEMINIMIS_ONSET)
  msg('      (gap step: $%.3fB/mo pre [%d mo] -> $%.3fB/mo post [%d mo])',
      est$pre_gap / 1e9, est$n_pre, est$post_gap / 1e9, est$n_post)
  list(rev = rev, active = TRUE, step_usd = est$step_usd)
}
