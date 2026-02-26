# =============================================================================
# 02_calculate_etr.R - Calculate weighted effective tariff rates
# =============================================================================
# Calculates pre-substitution and post-substitution weighted ETRs based on
# country-level tariff rates and import weights from GTAP.
#
# This module now calculates ETRs from:
# - etr_matrix: Tariff-ETRs output (sector x country ETR matrix)
# - viws: GTAP post-simulation imports by commodity x country
# - baselines$viws_baseline: GTAP baseline imports
#
# Country columns: China, Canada, Mexico, UK, Japan, EU, ROW, FTA

library(tidyverse)

# =============================================================================
# Country-level ETR calculation from etr_matrix + VIWS
# =============================================================================

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
#' @return Data frame with imports_* and etr_* columns (same format as aggregates.csv)
calculate_country_etrs <- function(etr_matrix, viws, viws_baseline,
                                   import_baseline_dollars) {

  # Country mapping: VIWS column names -> ETR column names -> output suffix
  # VIWS uses: china, canada, mexico, uk, japan, eu, row, ftrow
  # ETR matrix uses same names as VIWS
  # Output uses: chn, ca, mx, uk, jp, eu, row, fta (matching aggregates.csv format)
  country_config <- list(
    list(viws = 'china', etr = 'china', suffix = 'chn'),
    list(viws = 'canada', etr = 'canada', suffix = 'ca'),
    list(viws = 'mexico', etr = 'mexico', suffix = 'mx'),
    list(viws = 'uk', etr = 'uk', suffix = 'uk'),
    list(viws = 'japan', etr = 'japan', suffix = 'jp'),
    list(viws = 'eu', etr = 'eu', suffix = 'eu'),
    list(viws = 'row', etr = 'row', suffix = 'row'),
    list(viws = 'ftrow', etr = 'ftrow', suffix = 'fta')
  )

  if (is.null(viws_baseline) || is.null(import_baseline_dollars)) {
    stop('viws_baseline and import_baseline_dollars are required for derived imports')
  }

  result <- list()

  for (cfg in country_config) {
    viws_col <- cfg$viws
    etr_col <- cfg$etr
    output_suffix <- cfg$suffix

    # Get raw imports for this country from VIWS
    if (!viws_col %in% colnames(viws)) {
      stop('VIWS missing required column: ', viws_col)
    }

    # Only process sectors that exist in import_baseline_dollars (goods sectors only)
    # Services sectors don't have tariffs and should be excluded
    all_viws_sectors <- rownames(viws)
    goods_sectors <- rownames(import_baseline_dollars)
    sectors <- intersect(all_viws_sectors, goods_sectors)

    # Get raw imports only for goods sectors
    sector_indices <- match(sectors, all_viws_sectors)
    raw_imports <- viws[sector_indices, viws_col]

    if (!viws_col %in% colnames(viws_baseline)) {
      stop('VIWS baseline missing required column: ', viws_col)
    }
    if (!viws_col %in% colnames(import_baseline_dollars)) {
      stop('Import baseline dollars missing required column: ', viws_col)
    }

    baseline_viws <- viws_baseline[, viws_col]
    baseline_dollars <- import_baseline_dollars[, viws_col]

    # derived_import = (postsim / baseline) * baseline_dollars
    # Handle division by zero: if baseline is 0, use 0

    # Validate all sectors exist in baseline matrices (vectorized check)
    missing_baseline <- setdiff(sectors, rownames(viws_baseline))
    missing_dollars <- setdiff(sectors, rownames(import_baseline_dollars))
    if (length(missing_baseline) > 0 || length(missing_dollars) > 0) {
      stop('Baseline rows missing for sector(s): ',
           paste(union(missing_baseline, missing_dollars), collapse = ', '))
    }

    # Vectorized calculation using direct row indexing by sector name
    baseline_viws_vec <- baseline_viws[sectors]
    baseline_dollars_vec <- baseline_dollars[sectors]

    # Calculate ratio where baseline > 0, else 0
    ratio <- if_else(baseline_viws_vec > 0, raw_imports / baseline_viws_vec, 0)
    country_imports <- ratio * baseline_dollars_vec

    total_imports <- sum(country_imports)
    result[[paste0('imports_', output_suffix)]] <- total_imports

    # Calculate weighted ETR if column exists and has imports
    # IMPORTANT: Only include sectors with ETR values in the weighted average
    # (services sectors without tariffs should not dilute the average)
    if (!etr_col %in% names(etr_matrix)) {
      stop('ETR matrix missing required column: ', etr_col)
    }
    if (total_imports <= 0) {
      stop('Total imports are non-positive for country: ', viws_col)
    }

    # Validate all sectors exist in ETR matrix (vectorized check)
    missing_etr <- setdiff(sectors, etr_matrix$gtap_code)
    if (length(missing_etr) > 0) {
      stop('ETR matrix missing gtap_code for sector(s): ', paste(missing_etr, collapse = ', '))
    }

    # Build named lookup vector for ETR values (gtap_code -> etr)
    etr_lookup <- setNames(etr_matrix[[etr_col]], etr_matrix$gtap_code)

    # Vectorized weighted ETR calculation
    sector_etrs <- etr_lookup[sectors]
    weighted_etr <- sum(sector_etrs * country_imports)
    matched_imports <- sum(country_imports)

    # Use matched imports as denominator (excludes services without tariffs)
    if (matched_imports > 0) {
      result[[paste0('etr_', output_suffix)]] <- weighted_etr / matched_imports
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
#'   - etr_matrix: Tariff-ETRs output (sector x country ETR matrix)
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

  # Post-substitution (using post-sim VIWS with derived import formula)
  postsim_country_etrs <- calculate_country_etrs(
    inputs$etr_matrix, inputs$viws,
    viws_baseline, import_baseline_dollars
  )
  post_sub_etr <- calculate_weighted_etr(postsim_country_etrs)

  # Store the calculated aggregates for downstream use
  inputs$gtap_postsim <- postsim_country_etrs

  # Pre-substitution (using baseline - ratio is 1, so imports = baseline_dollars)
  presim_country_etrs <- calculate_country_etrs(
    inputs$etr_matrix, viws_baseline,
    viws_baseline, import_baseline_dollars
  )
  pre_sub_etr <- calculate_weighted_etr(presim_country_etrs)

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

  # Compute all-in ETR from scenario levels (post-substitution)
  postsim_levels_country <- calculate_country_etrs(
    inputs$levels_matrix, inputs$viws,
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
  per_date_levels <- NULL

  if (isTRUE(inputs$is_time_varying)) {
    message('  Computing per-date weighted ETRs for time-varying scenario...')

    per_date_etrs <- calculate_per_date_weighted_etrs(
      inputs$etr_matrix_by_date,
      inputs$etr_dates,
      viws_baseline,
      import_baseline_dollars
    )

    # Reference date weighted ETR
    ref_date <- inputs$gtap_reference_date
    ref_etr <- per_date_etrs %>%
      filter(date == ref_date) %>%
      pull(weighted_etr)

    if (length(ref_etr) != 1 || ref_etr == 0) {
      stop('Reference date weighted ETR is zero or missing for: ', ref_date)
    }

    # Scale GTAP etr_increase proportionally by each date's weighted ETR
    etr_increase_by_date <- per_date_etrs %>%
      mutate(
        etr_increase = etr_increase * (weighted_etr / ref_etr)
      ) %>%
      select(date, etr_increase)

    message(sprintf('  Scaled etr_increase for %d dates (ref=%.4f%%)',
                    nrow(etr_increase_by_date), ref_etr))
    for (i in seq_len(nrow(etr_increase_by_date))) {
      message(sprintf('    %s: etr_increase=%.4f',
                      etr_increase_by_date$date[i],
                      etr_increase_by_date$etr_increase[i]))
    }

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

  results <- list(
    # Main ETR results (as percentages) - goods-weighted for display
    pre_sub_increase = pre_sub_etr,
    pre_sub_all_in = pre_sub_all_in * 100,
    post_sub_increase = post_sub_etr,
    post_sub_all_in = post_sub_all_in * 100,
    # Baseline ETR computed from levels (as percentage)
    baseline_etr = baseline_etr * 100,
    # etr_increase for revenue calculations (from mtax, includes all imports)
    etr_increase = etr_increase,
    # Per-date etr_increase (NULL for static scenarios)
    etr_increase_by_date = etr_increase_by_date,
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

  message(sprintf('  Goods-weighted pre-sub ETR: %.2f%%', pre_sub_etr))
  message(sprintf('  Goods-weighted post-sub ETR: %.2f%%', post_sub_etr))

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

  # Import columns
  imports <- c(
    data$imports_chn,
    data$imports_ca,
    data$imports_mx,
    data$imports_uk,
    data$imports_jp,
    data$imports_eu,
    data$imports_row,
    data$imports_fta
  )

  # ETR columns
  etrs <- c(
    data$etr_chn,
    data$etr_ca,
    data$etr_mx,
    data$etr_uk,
    data$etr_jp,
    data$etr_eu,
    data$etr_row,
    data$etr_fta
  )

  # Weighted average
  total_imports <- sum(imports)
  if (total_imports <= 0) {
    stop('Total imports must be positive to calculate weighted ETR')
  }
  overall_etr <- sum(imports * etrs) / total_imports

  return(overall_etr)
}
