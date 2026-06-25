# =============================================================================
# adcvd_strip.R — remove AD/CVD from the COLLECTED side before calibrating eta
# =============================================================================
# FAITHFUL PORT of tariff-etr-adj/code/adcvd_strip.R (logic verbatim; only the
# curated-input path moves to resources/calibration/). See that file's header
# and ../tariff-rate-tracker/docs/adcvd_layer_design.md for the full rationale.
#
# WHY: calibrated eta = 1 - k * (collected ETR / statutory ETR). The Treasury
# collected level (customs_duties) carries AD/CVD; the tracker statutory rate
# does not, so AD/CVD inflates treas_train_etr and the aggregate eta. Census
# cal_dut_mo structurally EXCLUDES AD/CVD (resolved 2026-06-10:
# CENSUS_INCLUDES_ADCVD = FALSE), so only the Treasury level is stripped and the
# per-cell Census distribution is left untouched.
#
# DO EXACTLY ONE (double-count guard): strip AD/CVD here XOR model it as a
# statutory rung in the tracker — never both. Project decision (2026-06-08) is
# the strip.
#
# Curated input — resources/calibration/adcvd_collected.csv (interim; FY19
# Appendix A partner shares x ~$2.0B/yr). Columns: partner_group (one of
# PARTNER_LEVELS — Korea is "S. Korea" with the space), year_month ("YYYY-MM" or
# blank = spread over window), hs2 (2-digit or blank = spread over chapters),
# adcvd_usd (DOLLARS). Comment lines beginning '#' are skipped.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

ADCVD_COLLECTED_PATH <- file.path('resources', 'calibration', 'adcvd_collected.csv')

# Census cal_dut_mo structurally excludes AD/CVD (resolved 2026-06-10): strip the
# Treasury level only; leave the per-cell Census distribution untouched. The TRUE
# branch (matched both-sided strip) is kept inert should new evidence overturn it.
CENSUS_INCLUDES_ADCVD <- FALSE

#' Load the curated CBP AD/CVD-collected table.
#'
#' @return tibble(partner_group, year_month, hs2, adcvd_usd) with NA for blank
#'   dimension columns; NULL if the file is absent or has no data rows (caller
#'   then no-ops — the pipeline is unchanged).
load_adcvd_collected <- function(path = ADCVD_COLLECTED_PATH) {
  if (!file.exists(path)) {
    msg('    AD/CVD strip: %s not found — no-op (eta unchanged).', path)
    return(NULL)
  }
  ct <- cols(partner_group = col_character(), year_month = col_character(),
             hs2 = col_character(), adcvd_usd = col_double())
  adcvd <- read_csv(path, col_types = ct, comment = '#', show_col_types = FALSE)
  adcvd <- adcvd %>%
    mutate(across(c(partner_group, year_month, hs2),
                  ~ ifelse(is.na(.) | !nzchar(.), NA_character_, .)))
  if (nrow(adcvd) == 0) return(NULL)
  bad <- setdiff(na.omit(unique(adcvd$partner_group)), PARTNER_LEVELS)
  if (length(bad)) warning('AD/CVD strip: unknown partner_group(s): ',
                           paste(bad, collapse = ', '), ' (expected one of: ',
                           paste(PARTNER_LEVELS, collapse = ', '), ')')
  adcvd
}

#' Allocate coarse AD/CVD dollars onto IN-SCOPE panel cells by import value.
#'
#' Distribute each row's adcvd_usd across matching panel cells in proportion to
#' con_val_mo, restricted to cells whose period is in `periods` (default train +
#' test). Blank year_month / hs2 spread across all in-scope months / chapters.
#'
#' @return panel + numeric column `adcvd_dut`; attribute "unplaced_usd".
allocate_adcvd_to_cells <- function(adcvd, panel, periods = c('train', 'test')) {
  stopifnot(all(c('partner_group', 'hs2', 'year_month', 'con_val_mo', 'period')
                %in% names(panel)))
  panel$.row <- seq_len(nrow(panel))
  in_scope <- panel$period %in% periods
  alloc <- numeric(nrow(panel))
  unplaced <- 0

  for (i in seq_len(nrow(adcvd))) {
    r <- adcvd[i, ]
    keep <- in_scope & panel$partner_group == r$partner_group
    if (!is.na(r$year_month)) keep <- keep & panel$year_month == r$year_month
    if (!is.na(r$hs2))        keep <- keep & panel$hs2 == r$hs2
    block <- panel[keep, , drop = FALSE]
    denom <- sum(block$con_val_mo, na.rm = TRUE)
    if (nrow(block) == 0 || denom <= 0) { unplaced <- unplaced + r$adcvd_usd; next }
    alloc[block$.row] <- alloc[block$.row] +
      r$adcvd_usd * (block$con_val_mo / denom)
  }

  if (unplaced > 0)
    msg('    AD/CVD strip: $%.3gB could not be placed (no matching in-scope trade).',
        unplaced / 1e9)
  panel$.row <- NULL
  panel$adcvd_dut <- alloc
  attr(panel, 'unplaced_usd') <- unplaced
  panel
}

#' Strip AD/CVD from the Treasury level (and, under the TRUE branch, the Census
#' shape too, matched). The single entry point used by the pipeline.
#'
#' @return list(panel = stripped panel, rev = stripped rev).
apply_adcvd_strip <- function(panel, rev, adcvd, periods = c('train', 'test')) {
  panel <- allocate_adcvd_to_cells(adcvd, panel, periods)
  unplaced <- attr(panel, 'unplaced_usd')

  if (CENSUS_INCLUDES_ADCVD) {
    removed  <- pmin(panel$adcvd_dut, panel$cal_dut_mo)
    floored  <- sum(panel$adcvd_dut - removed)
    etr_before <- sum(panel$cal_dut_mo[panel$period %in% periods]) /
                  sum(panel$con_val_mo[panel$period %in% periods])
    panel$cal_dut_mo <- panel$cal_dut_mo - removed
    panel$census_etr <- safe_divide(panel$cal_dut_mo, panel$con_val_mo)
    etr_after <- sum(panel$cal_dut_mo[panel$period %in% periods]) /
                 sum(panel$con_val_mo[panel$period %in% periods])
    rm_by_month <- tibble(year_month = panel$year_month, rm = removed) %>%
      group_by(year_month) %>% summarise(rm = sum(rm), .groups = 'drop')
    matched <- sum(removed)
  } else {
    floored <- 0
    placed  <- sum(panel$adcvd_dut)
    wt      <- ifelse(panel$period %in% periods, panel$con_val_mo, 0)
    wsum    <- sum(wt)
    rm_by_month <- tibble(year_month = panel$year_month,
                          rm = if (wsum > 0) placed * wt / wsum else 0) %>%
      group_by(year_month) %>% summarise(rm = sum(rm), .groups = 'drop')
    matched <- placed
  }
  panel$adcvd_dut <- NULL

  rev <- rev %>%
    left_join(rm_by_month, by = 'year_month') %>%
    mutate(rm = coalesce(rm, 0),
           customs_duties = pmax(customs_duties - rm / 1e6, 0))
  if ('imports_value' %in% names(rev))
    rev <- mutate(rev, treas_etr = safe_divide(customs_duties, imports_value))
  rev$rm <- NULL

  if (CENSUS_INCLUDES_ADCVD) {
    msg('    AD/CVD strip: removed Treasury $%.3gB == Census $%.3gB (matched); collected ETR %.4f -> %.4f (-%.0fbp)',
        matched / 1e9, matched / 1e9, etr_before, etr_after,
        (etr_before - etr_after) * 1e4)
    if (floored > 0)
      msg('    AD/CVD strip: $%.3gB floored (allocated AD/CVD exceeded declared duty in some cells).',
          floored / 1e9)
  } else {
    msg('    AD/CVD strip (Treasury level only; CENSUS_INCLUDES_ADCVD=FALSE): removed $%.3gB.',
        matched / 1e9)
  }
  if (unplaced > 0)
    msg('    AD/CVD strip: note $%.3gB unplaced was NOT removed from either side (kept matched).',
        unplaced / 1e9)

  list(panel = panel, rev = rev)
}
