# =============================================================================
# 02_calculate_etr.R - Calculate weighted effective tariff rates
# =============================================================================
# Calculates pre-substitution and post-substitution weighted ETRs based on
# country-level tariff rates and import weights from GTAP.
#
# This module now calculates ETRs from:
# - etr_matrix: rate input output (sector x country ETR matrix)
# - viws: GTAP post-simulation imports by commodity x country
# - baselines$viws_baseline: GTAP baseline imports
#
# Country columns: China, Canada, Mexico, UK, Japan, EU, ROW, FTA

library(tidyverse)

# =============================================================================
# Country-level ETR calculation from etr_matrix + VIWS
# =============================================================================

COUNTRY_CONFIG <- tibble(
  viws = c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow'),
  etr = c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow'),
  suffix = c('chn', 'ca', 'mx', 'uk', 'jp', 'eu', 'row', 'fta')
)

#' Calculate country-level ETRs from sector ETR matrix and VIWS imports
#'
#' Uses Excel-compatible formula for derived imports:
#'   derived_import = (postsim_VIWS / baseline_VIWS) × baseline_import_dollars
#'
#' For each country: weighted_etr = sum(sector_etr * derived_imports) / sum(derived_imports)
#'
#' @param etr_matrix Data frame with gtap_code and country columns (china, canada, etc.)
#' @param viws Matrix of imports by commodity x source country (from GTAP post-sim)
#' @param viws_baseline Matrix of baseline imports by commodity x source country
#' @param import_baseline_dollars Matrix of baseline imports in dollars
#' @param alpha_within Within-commodity-source substitution exponent. Default 1
#'   uses the full GTAP cell shift.
#' @param alpha_between Cross-commodity substitution exponent. Default 1 uses
#'   the full GTAP commodity-total shift.
#' @param passthrough Character vector of lowercase gtap_codes that keep the full
#'   within shift when alpha_within is not 1.
#' @return Data frame with imports_* and etr_* columns (same format as aggregates.csv)
calculate_country_etrs <- function(etr_matrix, viws, viws_baseline,
                                   import_baseline_dollars,
                                   alpha_within = 1, alpha_between = 1,
                                   passthrough = character(0)) {

  if (is.null(viws_baseline) || is.null(import_baseline_dollars)) {
    stop('viws_baseline and import_baseline_dollars are required for derived imports')
  }

  missing_viws <- setdiff(COUNTRY_CONFIG$viws, colnames(viws))
  missing_viws_baseline <- setdiff(COUNTRY_CONFIG$viws, colnames(viws_baseline))
  missing_import_dollars <- setdiff(COUNTRY_CONFIG$viws, colnames(import_baseline_dollars))
  missing_etr_cols <- setdiff(COUNTRY_CONFIG$etr, names(etr_matrix))
  if (length(missing_viws) > 0) {
    stop('VIWS missing required column(s): ', paste(missing_viws, collapse = ', '))
  }
  if (length(missing_viws_baseline) > 0) {
    stop('VIWS baseline missing required column(s): ',
         paste(missing_viws_baseline, collapse = ', '))
  }
  if (length(missing_import_dollars) > 0) {
    stop('Import baseline dollars missing required column(s): ',
         paste(missing_import_dollars, collapse = ', '))
  }
  if (length(missing_etr_cols) > 0) {
    stop('ETR matrix missing required column(s): ', paste(missing_etr_cols, collapse = ', '))
  }

  all_viws_sectors <- rownames(viws)
  goods_sectors <- rownames(import_baseline_dollars)
  sectors <- intersect(all_viws_sectors, goods_sectors)

  missing_baseline <- setdiff(sectors, rownames(viws_baseline))
  missing_dollars <- setdiff(sectors, rownames(import_baseline_dollars))
  if (length(missing_baseline) > 0 || length(missing_dollars) > 0) {
    stop('Baseline rows missing for sector(s): ',
         paste(union(missing_baseline, missing_dollars), collapse = ', '))
  }

  missing_etr <- setdiff(sectors, etr_matrix$gtap_code)
  if (length(missing_etr) > 0) {
    stop('ETR matrix missing gtap_code for sector(s): ', paste(missing_etr, collapse = ', '))
  }

  alpha_active <- !identical(alpha_within, 1) ||
    !identical(alpha_between, 1) ||
    length(passthrough) > 0
  if (alpha_active) {
    vp <- viws[sectors, COUNTRY_CONFIG$viws, drop = FALSE]
    vb <- viws_baseline[sectors, COUNTRY_CONFIG$viws, drop = FALSE]
    between <- if_else(rowSums(vb) > 0, rowSums(vp) / rowSums(vb), 0)
    within_exp <- if_else(sectors %in% passthrough, 1, alpha_within)
  }

  result <- list()

  for (i in seq_len(nrow(COUNTRY_CONFIG))) {
    viws_col <- COUNTRY_CONFIG$viws[i]
    etr_col <- COUNTRY_CONFIG$etr[i]
    output_suffix <- COUNTRY_CONFIG$suffix[i]

    raw_imports <- viws[sectors, viws_col]
    baseline_viws_vec <- viws_baseline[sectors, viws_col]
    baseline_dollars_vec <- import_baseline_dollars[sectors, viws_col]

    ratio <- if_else(baseline_viws_vec > 0, raw_imports / baseline_viws_vec, 0)
    if (alpha_active) {
      within <- if_else(between > 0 & ratio > 0, ratio / between, 0)
      ratio <- if_else(within > 0 & between > 0,
                       within^within_exp * between^alpha_between, 0)
    }
    country_imports <- ratio * baseline_dollars_vec

    total_imports <- sum(country_imports)
    result[[paste0('imports_', output_suffix)]] <- total_imports

    if (total_imports <= 0) {
      stop('Total imports are non-positive for country: ', viws_col)
    }

    etr_lookup <- setNames(etr_matrix[[etr_col]], etr_matrix$gtap_code)
    sector_etrs <- etr_lookup[sectors]
    matched_imports <- sum(country_imports)

    if (matched_imports > 0) {
      result[[paste0('etr_', output_suffix)]] <- sum(sector_etrs * country_imports) / matched_imports
    } else {
      stop('No matched imports for ETR weighting in country: ', viws_col)
    }
  }

  return(as.data.frame(result))
}


#' Calculate etr_increase from post-sim and baseline weighted ETRs
#'
#' @param post_sub_etr Post-substitution weighted ETR (percentage)
#' @param baseline_etr Baseline weighted ETR (percentage)
#' @return etr_increase value (as decimal, matching aggregates.csv format)
calculate_etr_increase <- function(post_sub_etr, baseline_etr) {
  # etr_increase is the difference in weighted average ETRs
  return((post_sub_etr - baseline_etr) / 100)
}


# =============================================================================
# Per-date weighted ETR calculation (time-varying support)
# =============================================================================

#' Calculate weighted ETR for each date in a time-varying ETR matrix
#'
#' For each unique date, computes the import-weighted average ETR using
#' baseline VIWS/import weights (pre-substitution concept).
#'
#' @param etr_matrix_by_date Stacked ETR data with date, gtap_code, and country columns
#' @param etr_dates Sorted vector of unique dates
#' @param viws_baseline Matrix of baseline imports by commodity x source country
#' @param import_baseline_dollars Matrix of baseline imports in dollars
#'
#' @return Tibble with date and weighted_etr columns
calculate_per_date_weighted_etrs <- function(etr_matrix_by_date, etr_dates,
                                              viws_baseline, import_baseline_dollars) {

  results <- tibble(date = as.Date(character()), weighted_etr = numeric())

  for (d in etr_dates) {
    d_date <- as.Date(d, origin = '1970-01-01')
    etr_slice <- etr_matrix_by_date %>%
      filter(date == d_date) %>%
      select(-date)

    # Use baseline VIWS as both pre and post (pre-substitution weighting)
    country_etrs <- calculate_country_etrs(
      etr_slice, viws_baseline,
      viws_baseline, import_baseline_dollars
    )
    weighted <- calculate_weighted_etr(country_etrs)

    results <- bind_rows(results, tibble(date = d_date, weighted_etr = weighted))
  }

  return(results)
}


# =============================================================================
# Main calculation function
# =============================================================================

#' Calculate weighted effective tariff rates
#'
#' @param inputs List containing:
#'   - etr_matrix: rate input output (sector x country ETR matrix)
#'   - viws: GTAP post-simulation imports matrix
#'   - baselines$gtap: Baseline imports and ETRs
#'   - assumptions: Global assumptions including baseline_etr
#'
#' @return List with pre/post substitution ETR results
calculate_etr <- function(inputs) {

  # -------------------------------------------------------------------------
  # Get etr_increase from GTAP mtax (primary source for revenue calculations)
  # -------------------------------------------------------------------------

  if (is.null(inputs$etr_increase)) {
    stop('GTAP mtax-based etr_increase not found in inputs')
  }

  # etr_increase already calculated from mtax in read_gtap.R
  etr_increase <- inputs$etr_increase
  message(sprintf('  Using etr_increase from GTAP mtax: %.4f (%.2f%%)',
                  etr_increase, etr_increase * 100))

  # Also report mtax details if available
  if (is.null(inputs$mtax_data)) {
    stop('GTAP mtax data not found in inputs$mtax_data')
  }
  mtax <- inputs$mtax_data
  message(sprintf('    Scenario: mtax=%.0f, imports=%.0f, rate=%.2f%%',
                  mtax$mtax_scenario, mtax$imports_scenario, mtax$etr_scenario * 100))
  message(sprintf('    Baseline: mtax=%.0f, imports=%.0f, rate=%.2f%%',
                  mtax$mtax_baseline, mtax$imports_baseline, mtax$etr_baseline * 100))

  # -------------------------------------------------------------------------
  # Calculate weighted ETRs from etr_matrix + VIWS (for display purposes)
  # These are the traditional goods-only-concept ETRs
  # -------------------------------------------------------------------------

  # Initialize country-level data to NULL (will be set if available)
  postsim_country_etrs <- NULL
  presim_country_etrs <- NULL

  if (is.null(inputs$etr_matrix)) {
    stop('ETR matrix not found in inputs$etr_matrix')
  }
  if (is.null(inputs$viws)) {
    stop('VIWS matrix not found in inputs$viws')
  }

  message('  Calculating goods-weighted ETRs from etr_matrix + VIWS...')

  # Get baseline data for Excel-compatible derived imports
  viws_baseline <- inputs$baselines$viws_baseline
  import_baseline_dollars <- inputs$baselines$import_baseline_dollars

  # Post-substitution = rate (c): the eta'-adjusted (b) matrix re-weighted by
  # post-sim VIWS (GTAP's source-composition shift). Uses etr_matrix_b so the
  # published post-sub ETR matches the post-sub price (which also starts from b).
  # (b) == (a) when noncompliance is inactive, so legacy behavior is preserved.
  postsim_country_etrs <- calculate_country_etrs(
    inputs$etr_matrix_b, inputs$viws,
    viws_baseline, import_baseline_dollars
  )
  post_sub_etr <- calculate_weighted_etr(postsim_country_etrs)

  # Store the calculated aggregates for downstream use
  inputs$gtap_postsim <- postsim_country_etrs

  # Alpha-corrected post-sub ETR (substitution correction) — computed ALONGSIDE
  # the full-GTAP post-sub ETR purely to drive the revenue alpha->GTAP transition
  # (04_calculate_revenue.R). The published post-sub ETR (pe_postsub_increase),
  # prices, and USMM all stay FULL GTAP. R_alpha = 1 when alpha is inactive, so
  # revenue is then bit-for-bit unchanged. (post_sub_etr is the same rate (c)
  # matrix etr_matrix_b — only the import WEIGHTS differ via the alpha exponents.)
  post_sub_etr_alpha <- post_sub_etr
  R_alpha <- 1
  if (isTRUE(inputs$alpha_active)) {
    postsim_alpha_etrs <- calculate_country_etrs(
      inputs$etr_matrix_b, inputs$viws, viws_baseline, import_baseline_dollars,
      alpha_within  = inputs$alpha_within,
      alpha_between = inputs$alpha_between,
      passthrough   = inputs$alpha_passthrough
    )
    post_sub_etr_alpha <- calculate_weighted_etr(postsim_alpha_etrs)
    R_alpha <- if (post_sub_etr != 0) post_sub_etr_alpha / post_sub_etr else 1
    message(sprintf(paste0('  Alpha-corrected post-sub ETR: %.4f%% vs full-GTAP %.4f%% ',
                           '-> R_alpha = %.4f (revenue near-term anchor)'),
                    post_sub_etr_alpha, post_sub_etr, R_alpha))
  }

  # Pre-substitution (using baseline - ratio is 1, so imports = baseline_dollars)
  presim_country_etrs <- calculate_country_etrs(
    inputs$etr_matrix, viws_baseline,
    viws_baseline, import_baseline_dollars
  )
  pre_sub_etr <- calculate_weighted_etr(presim_country_etrs)

  # (b) eta'-adjusted statutory, baseline-weighted (pre-substitution). Drives the
  # USMM impulse and is reported as a diagnostic. Equals (a) when noncompliance is
  # inactive (etr_matrix_b == etr_matrix).
  presim_b_country_etrs <- calculate_country_etrs(
    inputs$etr_matrix_b, viws_baseline,
    viws_baseline, import_baseline_dollars
  )
  b_pre_sub_etr <- calculate_weighted_etr(presim_b_country_etrs)

  # -------------------------------------------------------------------------
  # Calculate all-in ETRs from levels matrices
  # -------------------------------------------------------------------------

  if (is.null(inputs$baseline_levels_matrix)) {
    stop('baseline_levels_matrix is required for all-in ETR calculation')
  }
  if (is.null(inputs$levels_matrix)) {
    stop('levels_matrix is required for all-in ETR calculation')
  }

  # Compute baseline ETR from baseline levels (replaces hardcoded assumption)
  baseline_country_levels <- calculate_country_etrs(
    inputs$baseline_levels_matrix, viws_baseline,
    viws_baseline, import_baseline_dollars
  )
  baseline_etr <- calculate_weighted_etr(baseline_country_levels) / 100
  message(sprintf('  Computed baseline ETR from levels: %.3f%%', baseline_etr * 100))

  # Compute all-in ETR from scenario levels (pre-substitution)
  presim_levels_country <- calculate_country_etrs(
    inputs$levels_matrix, viws_baseline,
    viws_baseline, import_baseline_dollars
  )
  pre_sub_all_in <- calculate_weighted_etr(presim_levels_country) / 100

  # (b) all-in level (baseline + eta'*delta), baseline-weighted — diagnostic
  presim_b_levels_country <- calculate_country_etrs(
    inputs$levels_matrix_b, viws_baseline,
    viws_baseline, import_baseline_dollars
  )
  b_all_in <- calculate_weighted_etr(presim_b_levels_country) / 100

  # All-in ETR from scenario levels, post-substitution = rate (c) levels:
  # eta'-adjusted (b) levels re-weighted by post-sim VIWS.
  postsim_levels_country <- calculate_country_etrs(
    inputs$levels_matrix_b, inputs$viws,
    viws_baseline, import_baseline_dollars
  )
  post_sub_all_in <- calculate_weighted_etr(postsim_levels_country) / 100

  # Sanity check: all-in should approximately equal baseline + delta
  delta_check_pre <- baseline_etr + (pre_sub_etr / 100)
  delta_check_post <- baseline_etr + (post_sub_etr / 100)
  message(sprintf('  All-in pre-sub:  levels=%.3f%% vs baseline+delta=%.3f%%',
                  pre_sub_all_in * 100, delta_check_pre * 100))
  message(sprintf('  All-in post-sub: levels=%.3f%% vs baseline+delta=%.3f%%',
                  post_sub_all_in * 100, delta_check_post * 100))

  # -------------------------------------------------------------------------
  # Time-varying: per-date ETR scaling
  # -------------------------------------------------------------------------

  etr_increase_by_date <- NULL
  presub_etr_increase_by_date <- NULL
  b_etr_increase_by_date <- NULL
  per_date_levels <- NULL

  if (isTRUE(inputs$is_time_varying)) {
    message('  Computing per-date weighted ETRs for time-varying scenario...')

    # (a) applied-statutory per-date weighted ETRs
    per_date_etrs <- calculate_per_date_weighted_etrs(
      inputs$etr_matrix_by_date,
      inputs$etr_dates,
      viws_baseline,
      import_baseline_dollars
    )

    # (b) eta'-adjusted per-date weighted ETRs (baseline-weighted). Equals (a)
    # when noncompliance is inactive (etr_matrix_by_date_b == etr_matrix_by_date).
    per_date_etrs_b <- calculate_per_date_weighted_etrs(
      inputs$etr_matrix_by_date_b,
      inputs$etr_dates,
      viws_baseline,
      import_baseline_dollars
    )

    ref_date <- inputs$gtap_reference_date

    # Revenue time profile: the GTAP etr_increase anchor (inputs$etr_increase, the
    # mtax of the reference-date GTAP run) is shocked with the (b) eta'-adjusted
    # rates when noncompliance is active, so its cross-date profile must track the
    # (b) effective-rate path -- not the (a) applied-statutory path -- otherwise a
    # (b) level is scaled by an (a) shape (zero only when eta' is uniform across the
    # tariffed mix). (a) and (b) coincide when noncompliance is inactive, so legacy
    # runs are bit-for-bit unchanged.
    profile_etrs <- if (isTRUE(inputs$noncompliance_active)) per_date_etrs_b else per_date_etrs
    profile_label <- if (isTRUE(inputs$noncompliance_active)) 'b' else 'a'

    # Reference date weighted ETR (on the same profile that scales the anchor)
    ref_etr <- profile_etrs %>%
      filter(date == ref_date) %>%
      pull(weighted_etr)

    if (length(ref_etr) != 1 || ref_etr == 0) {
      stop('Reference date weighted ETR is zero or missing for: ', ref_date)
    }

    # Scale GTAP etr_increase proportionally by each date's weighted ETR
    # (post-sub, for revenue calculations)
    etr_increase_by_date <- profile_etrs %>%
      mutate(
        etr_increase = etr_increase * (weighted_etr / ref_etr)
      ) %>%
      select(date, etr_increase)

    message(sprintf('  Scaled etr_increase for %d dates (ref=%.4f%%, profile=%s)',
                    nrow(etr_increase_by_date), ref_etr, profile_label))
    for (i in seq_len(nrow(etr_increase_by_date))) {
      message(sprintf('    %s: etr_increase=%.4f',
                      etr_increase_by_date$date[i],
                      etr_increase_by_date$etr_increase[i]))
    }

    # Pre-sub per-date ETR increases (raw, for USMM)
    presub_etr_increase_by_date <- per_date_etrs %>%
      mutate(etr_increase = weighted_etr / 100) %>%
      select(date, etr_increase)

    message('  Pre-sub etr_increase by date (for USMM):')
    for (i in seq_len(nrow(presub_etr_increase_by_date))) {
      message(sprintf('    %s: etr_increase=%.4f',
                      presub_etr_increase_by_date$date[i],
                      presub_etr_increase_by_date$etr_increase[i]))
    }

    # (b) per-date eta'-adjusted increases (for USMM); baseline-weighted
    b_etr_increase_by_date <- per_date_etrs_b %>%
      mutate(etr_increase = weighted_etr / 100) %>%
      select(date, etr_increase)

    # Compute per-date all-in levels (for output transparency)
    if (!is.null(inputs$levels_matrix_by_date)) {
      message('  Computing per-date all-in ETR levels...')
      per_date_levels <- calculate_per_date_weighted_etrs(
        inputs$levels_matrix_by_date,
        inputs$etr_dates,
        viws_baseline,
        import_baseline_dollars
      )
      for (i in seq_len(nrow(per_date_levels))) {
        message(sprintf('    %s: all-in level=%.4f%%',
                        per_date_levels$date[i],
                        per_date_levels$weighted_etr[i]))
      }
    } else {
      per_date_levels <- NULL
    }
  }

  # -------------------------------------------------------------------------
  # Compile results
  # -------------------------------------------------------------------------

  # Pre-sub etr_increase for USMM (raw rate input, as fraction)
  presub_etr_increase <- pre_sub_etr / 100
  # (b) eta'-adjusted increase: the USMM impulse when noncompliance is active
  b_etr_increase <- b_pre_sub_etr / 100

  results <- list(
    # Main ETR results (as percentages) - goods-weighted for display
    pre_sub_increase = pre_sub_etr,
    pre_sub_all_in = pre_sub_all_in * 100,
    pe_postsub_increase = post_sub_etr,
    pe_postsub_all_in = post_sub_all_in * 100,
    # Baseline ETR computed from levels (as percentage)
    baseline_etr = baseline_etr * 100,
    # etr_increase for revenue calculations (from mtax, post-sub)
    etr_increase = etr_increase,
    # Alpha substitution correction for the revenue alpha->GTAP transition.
    # R_alpha = (alpha-corrected post-sub ETR)/(full-GTAP post-sub ETR); 1 when
    # alpha is inactive (revenue then bit-for-bit unchanged). post_sub_etr_alpha
    # is a diagnostic; the published post-sub ETR stays full-GTAP.
    R_alpha = R_alpha,
    post_sub_etr_alpha = post_sub_etr_alpha,
    # Per-date etr_increase for revenue (NULL for static scenarios)
    etr_increase_by_date = etr_increase_by_date,
    # Pre-sub etr_increase for USMM (raw rate input)
    presub_etr_increase = presub_etr_increase,
    # Per-date pre-sub etr_increase for USMM (NULL for static scenarios)
    presub_etr_increase_by_date = presub_etr_increase_by_date,
    # (b) eta'-adjusted statutory: USMM impulse (when active) + diagnostic
    b_increase = b_pre_sub_etr,
    b_all_in = b_all_in * 100,
    b_etr_increase = b_etr_increase,
    b_etr_increase_by_date = b_etr_increase_by_date,
    # Country-level data for output (deltas)
    postsim_country = postsim_country_etrs,
    presim_country = presim_country_etrs,
    # Country-level data from levels
    baseline_country_levels = baseline_country_levels,
    presim_country_levels = presim_levels_country,
    postsim_country_levels = postsim_levels_country,
    # Per-date all-in levels (NULL for static scenarios)
    per_date_levels = per_date_levels
  )

  message(sprintf('  Goods-weighted pre-sub ETR (a): %.2f%%', pre_sub_etr))
  message(sprintf('  Goods-weighted eta\'-adjusted ETR (b): %.2f%%', b_pre_sub_etr))
  message(sprintf('  Goods-weighted PE post-sub ETR: %.2f%%', post_sub_etr))

  return(results)
}


# =============================================================================
# Helper function to calculate weighted ETR from aggregate data
# =============================================================================

#' Calculate weighted ETR from country-level aggregate data
#'
#' @param data Dataframe with imports_* and etr_* columns by country
#'
#' @return Overall weighted ETR (as percentage)
calculate_weighted_etr <- function(data) {

  import_cols <- paste0('imports_', COUNTRY_CONFIG$suffix)
  etr_cols <- paste0('etr_', COUNTRY_CONFIG$suffix)
  missing_cols <- setdiff(c(import_cols, etr_cols), names(data))
  if (length(missing_cols) > 0) {
    stop('Country ETR data missing required column(s): ', paste(missing_cols, collapse = ', '))
  }

  imports <- as.numeric(data[1, import_cols])
  etrs <- as.numeric(data[1, etr_cols])

  # Weighted average
  total_imports <- sum(imports)
  if (total_imports <= 0) {
    stop('Total imports must be positive to calculate weighted ETR')
  }
  overall_etr <- sum(imports * etrs) / total_imports

  return(overall_etr)
}
