# =============================================================================
# 05a_usmm_surrogate.R - USMM IRF-based surrogate for macro effects
# =============================================================================
#
# Replaces the MAUS surrogate with USMM impulse response functions.
# Decomposes tariff scenarios into permanent, temporary, and refund components,
# scales/interpolates the corresponding IRFs, and sums them.
#
# Key functions:
#   load_usmm_irfs()        - Load all IRF CSVs from resources/usmm/
#   decompose_shocks()      - Break scenario into perm/temp/extra components
#   interpolate_irf()       - Linear interpolation between bracket IRFs
#   construct_usmm_response() - Build composite response from components
#   convert_usmm_to_levels() - Add diffs to baseline levels
#
# =============================================================================

library(tidyverse)


# =============================================================================
# CONSTANTS
# =============================================================================

# IRF bracket sizes (percentage points of tariff shock)
PERM_SIZES <- c(2, 5, 10)
TEMP_SIZES <- c(2, 5, 10)

# Refund calibration value (billions) used in USMM IRF generation
REFUND_CALIBRATION_BN <- 168

# USMM variables
USMM_VARS <- c('gdpr', 'jpc', 'ruc', 'ehhc', 'rmff')

# Quarter index of shock onset in USMM IRFs (2025Q2 = quarter index 6, 1-based)
# The IRFs are 0 through 2025Q1, shock hits in 2025Q2
USMM_SHOCK_QUARTER <- 6

# The permanent and temporary IRF families have DIFFERENT native onset quarters:
# the perm IRFs first respond in 2025Q2 (index 6); the temp IRFs first respond in
# 2026Q1 (index 9). A component must therefore be re-centered relative to the
# onset of the family it is drawn from. validate_irf_onsets() (called in
# load_usmm_irfs) hard-stops if a re-extracted IRF vintage ever breaks these.
USMM_TEMP_SHOCK_QUARTER <- 9


# =============================================================================
# LOADING
# =============================================================================

#' Load all USMM IRF CSVs from resources/usmm/
#'
#' @param usmm_dir Path to USMM resource directory
#' @return List with perm, temp, refund, and baseline tibbles
load_usmm_irfs <- function(usmm_dir = 'resources/usmm') {

  load_irf <- function(filename) {
    path <- file.path(usmm_dir, filename)
    if (!file.exists(path)) {
      stop('USMM IRF file not found: ', path)
    }
    read_csv(path, show_col_types = FALSE)
  }

  irfs <- list(
    perm = list(
      `2`  = load_irf('perm2_fed.csv'),
      `5`  = load_irf('perm5_fed.csv'),
      `10` = load_irf('perm10_fed.csv')
    ),
    temp = list(
      `2`  = load_irf('temp2_fed.csv'),
      `5`  = load_irf('temp5_fed.csv'),
      `10` = load_irf('temp10_fed.csv')
    ),
    refund = load_irf('refund_fed.csv'),
    baseline = load_irf('baseline.csv')
  )

  message(sprintf('  Loaded USMM IRFs: %d quarters each', nrow(irfs$perm$`2`)))

  validate_irf_onsets(irfs)

  return(irfs)
}


#' First quarter index (1-based) at which an IRF becomes materially nonzero
#'
#' @param irf_df An IRF tibble (year, quarter, USMM_VARS columns)
#' @param tol Magnitude below which a response is treated as numerical zero
#' @return Integer quarter index of the first material response
first_material_quarter <- function(irf_df, tol = 1e-8) {
  response_mass <- rowSums(abs(as.matrix(irf_df[, USMM_VARS])))
  hit <- which(response_mass > tol)
  if (length(hit) == 0) {
    stop('IRF has no material response in any quarter')
  }
  return(min(hit))
}


#' Assert each IRF family's native onset matches its assumed shock-quarter constant
#'
#' The perm and temp IRF families are re-centered using different onset constants
#' (USMM_SHOCK_QUARTER vs USMM_TEMP_SHOCK_QUARTER). Those constants are only valid
#' if the loaded IRFs actually first respond at the assumed quarter. This guards
#' against a silently desynced IRF re-extraction (the failure mode that caused the
#' temp components to be shifted by the perm onset).
#'
#' @param irfs IRF list from load_usmm_irfs()
#' @return TRUE invisibly; stops on mismatch
validate_irf_onsets <- function(irfs) {
  for (sz in names(irfs$perm)) {
    onset_q <- first_material_quarter(irfs$perm[[sz]])
    if (onset_q != USMM_SHOCK_QUARTER) {
      stop(sprintf(
        'perm%s IRF first responds at quarter index %d, but USMM_SHOCK_QUARTER = %d. The IRF vintage changed; update the constant (and check the temp/perm onset split).',
        sz, onset_q, USMM_SHOCK_QUARTER))
    }
  }
  for (sz in names(irfs$temp)) {
    onset_q <- first_material_quarter(irfs$temp[[sz]])
    if (onset_q != USMM_TEMP_SHOCK_QUARTER) {
      stop(sprintf(
        'temp%s IRF first responds at quarter index %d, but USMM_TEMP_SHOCK_QUARTER = %d. The IRF vintage changed; update the constant.',
        sz, onset_q, USMM_TEMP_SHOCK_QUARTER))
    }
  }
  return(invisible(TRUE))
}


# =============================================================================
# INTERPOLATION
# =============================================================================

#' Linearly interpolate between bracket IRFs for a given shock size
#'
#' Given bracket IRFs at 2, 5, 10 pp, interpolates (or extrapolates) to
#' produce the IRF for an arbitrary shock size. Interpolation is linear
#' between adjacent brackets.
#'
#' @param irf_set Named list of IRF tibbles (names are shock sizes as strings)
#' @param shock_pp Shock size in percentage points
#' @param sizes Numeric vector of bracket sizes (default: c(2, 5, 10))
#' @return Tibble with interpolated IRF (same structure as input IRFs)
interpolate_irf <- function(irf_set, shock_pp, sizes = c(2, 5, 10)) {

  # Handle zero shock
  if (abs(shock_pp) < 1e-10) {
    result <- irf_set[[1]]
    for (var in USMM_VARS) {
      result[[var]] <- 0
    }
    return(result)
  }

  abs_shock <- abs(shock_pp)
  sign_shock <- sign(shock_pp)

  # Find bracketing sizes
  if (abs_shock <= sizes[1]) {
    # Below smallest bracket: linear scale from 0
    lower_idx <- 1
    weight <- abs_shock / sizes[1]
    result <- irf_set[[1]]
    for (var in USMM_VARS) {
      result[[var]] <- sign_shock * weight * irf_set[[as.character(sizes[1])]][[var]]
    }
    return(result)
  }

  if (abs_shock >= sizes[length(sizes)]) {
    # Above largest bracket: linear extrapolation from last two
    n <- length(sizes)
    lower <- sizes[n - 1]
    upper <- sizes[n]
    weight <- (abs_shock - lower) / (upper - lower)
    result <- irf_set[[1]]
    for (var in USMM_VARS) {
      lower_vals <- irf_set[[as.character(lower)]][[var]]
      upper_vals <- irf_set[[as.character(upper)]][[var]]
      result[[var]] <- sign_shock * (lower_vals + weight * (upper_vals - lower_vals))
    }
    return(result)
  }

  # Between brackets: find adjacent pair
  for (i in seq_along(sizes[-length(sizes)])) {
    if (abs_shock >= sizes[i] && abs_shock <= sizes[i + 1]) {
      lower <- sizes[i]
      upper <- sizes[i + 1]
      weight <- (abs_shock - lower) / (upper - lower)
      result <- irf_set[[1]]
      for (var in USMM_VARS) {
        lower_vals <- irf_set[[as.character(lower)]][[var]]
        upper_vals <- irf_set[[as.character(upper)]][[var]]
        result[[var]] <- sign_shock * (lower_vals + weight * (upper_vals - lower_vals))
      }
      return(result)
    }
  }

  # Every finite shock_pp is covered by the branches above (<= smallest, >=
  # largest, or one of the adjacent brackets). Falling through means shock_pp is
  # NA/non-finite or the size grid is malformed; fail loudly rather than return
  # an implicit NULL that surfaces as a cryptic error downstream.
  stop(sprintf('shock_pp not bracketed by IRF size grid (shock_pp=%s, sizes=[%s])',
               shock_pp, paste(sizes, collapse = ', ')))
}


# =============================================================================
# SHOCK DECOMPOSITION
# =============================================================================

#' Convert a date to a USMM quarter index (1-based, 2024Q1 = 1)
#'
#' @param d Date object
#' @return Integer quarter index
date_to_quarter_index <- function(d) {
  y <- as.integer(format(d, '%Y'))
  q <- quarter(d)
  # 2024Q1 = index 1, 2024Q2 = index 2, etc.
  (y - 2024) * 4 + q
}


# Each permanent rate change enters in its own quarter. The undated scenario
# retains the onset of the original permanent-response experiment, 2025 Q2.
decompose_shocks <- function(etr_increase_by_date = NULL, etr_increase = NULL,
                             refund_2026 = 0, temp_component_dates = NULL) {
  schedule <- etr_increase_by_date
  if (is.null(schedule)) {
    if (is.null(etr_increase)) stop('A dated rate schedule or a static rate is required')
    schedule <- tibble(date = as.Date('2025-04-01'), etr_increase = etr_increase)
  }
  schedule <- arrange(schedule, date)
  if (!nrow(schedule) || anyNA(schedule$date) || anyDuplicated(schedule$date) ||
      any(!is.finite(schedule$etr_increase))) stop('Invalid USMM rate schedule')

  steps <- function(dates, level) {
    tibble(onset_q = date_to_quarter_index(dates),
           delta_pp = level - lag(level, default = 0)) %>%
      filter(abs(delta_pp) > 1e-10)
  }
  all_perm <- steps(schedule$date, 100 * schedule$etr_increase)
  permanent <- all_perm
  temporary <- all_perm[0, ]

  if (!is.null(temp_component_dates) && !is.null(etr_increase_by_date)) {
    window <- as.Date(unlist(temp_component_dates, use.names = FALSE))
    if (length(window) != 2L || anyNA(window) || window[2] <= window[1]) {
      stop('temp_component_dates requires an onset followed by an end date')
    }
    dates <- sort(unique(c(schedule$date, window)))
    level <- c(0, 100 * schedule$etr_increase)[1L + findInterval(dates, schedule$date)]
    inside <- dates >= window[1] & dates < window[2]
    prior <- c(0, level[dates < window[1]])
    permanent_level <- level
    permanent_level[inside] <- tail(prior, 1)
    permanent <- steps(dates, permanent_level)

    # The temporary experiment already includes the reversal. At onset apply
    # the announced window's current size. Later revisions affect only the
    # remaining fraction of the window, starting when the revision takes effect.
    # This retains duration weighting without using future rates at onset.
    revisions <- (level - lag(level, default = 0))[inside]
    remaining <- as.numeric(window[2] - dates[inside]) / as.numeric(diff(window))
    temporary <- tibble(onset_q = date_to_quarter_index(dates[inside]),
                        delta_pp = revisions * remaining) %>%
      filter(abs(delta_pp) > 1e-10)
  }

  list(permanent = permanent, temporary = temporary, all_perm = all_perm,
       refund_scalar = refund_2026 / REFUND_CALIBRATION_BN)
}


# Moving a response earlier can expose quarters beyond the available series.
# Hold its last observed response in those quarters.
shift_irf <- function(irf_vals, shift) {
  index <- seq_along(irf_vals) - shift
  ifelse(index < 1L, 0, irf_vals[pmin(length(irf_vals), pmax(1L, index))])
}


# Interpolate each change separately, then add the shifted responses. This
# assumes the effects of separate tariff decisions add together. The size of
# each individual response still follows the original 2/5/10-point brackets.
construct_usmm_response <- function(shocks, irfs, use_temp_irf = TRUE) {
  result <- select(irfs$perm[['2']], year, quarter)
  for (var in USMM_VARS) result[[var]] <- 0

  add_steps <- function(result, steps, family, sizes, native_onset) {
    for (i in seq_len(nrow(steps))) {
      response <- interpolate_irf(family, steps$delta_pp[i], sizes)
      for (var in USMM_VARS) {
        values <- response[[var]]
        values[seq_len(native_onset - 1L)] <- 0
        result[[var]] <- result[[var]] + shift_irf(values, steps$onset_q[i] - native_onset)
      }
    }
    result
  }
  permanent <- if (use_temp_irf) shocks$permanent else shocks$all_perm
  result <- add_steps(result, permanent, irfs$perm, PERM_SIZES, USMM_SHOCK_QUARTER)
  if (use_temp_irf) {
    result <- add_steps(result, shocks$temporary, irfs$temp, TEMP_SIZES, USMM_TEMP_SHOCK_QUARTER)
  }

  # 3. Refund component
  if (abs(shocks$refund_scalar) > 1e-10) {
    for (var in USMM_VARS) {
      result[[var]] <- result[[var]] + shocks$refund_scalar * irfs$refund[[var]]
    }
  }

  return(result)
}


# =============================================================================
# CONVERSION TO LEVELS
# =============================================================================

#' Convert USMM diffs to levels by adding to baseline
#'
#' Takes the diff-from-baseline response and the baseline levels, and produces
#' a combined tibble with both baseline and tariff-scenario columns.
#' Output structure provides inputs$macro_quarterly for the pipeline.
#'
#' @param usmm_diffs Tibble with year, quarter, and diff columns from construct_usmm_response()
#' @param baseline Tibble with year, quarter, and level columns from USMM baseline
#' @return Tibble with baseline and tariff columns for GDP, employment, unemployment, PCE, fed funds
convert_usmm_to_levels <- function(usmm_diffs, baseline) {

  # Join diffs with baseline
  combined <- baseline %>%
    select(year, quarter, gdpr, jpc, ruc, ehhc, rmff) %>%
    rename(
      gdp_baseline = gdpr,
      pce_baseline = jpc,
      urate_baseline = ruc,
      employment_baseline = ehhc,
      fed_funds_baseline = rmff
    ) %>%
    left_join(
      usmm_diffs %>%
        select(year, quarter, gdpr, jpc, ruc, ehhc, rmff) %>%
        rename(
          gdpr_diff = gdpr,
          jpc_diff = jpc,
          ruc_diff = ruc,
          ehhc_diff = ehhc,
          rmff_diff = rmff
        ),
      by = c('year', 'quarter')
    )

  # Apply diffs to get tariff-scenario levels
  # gdpr and jpc diffs are % diff -> tariff = baseline * (1 + diff/100)
  # ruc and rmff diffs are %pt diff -> tariff = baseline + diff
  # ehhc diff is % diff -> tariff = baseline * (1 + diff/100)
  result <- combined %>%
    mutate(
      gdp_tariff = gdp_baseline * (1 + gdpr_diff / 100),
      pce_tariff = pce_baseline * (1 + jpc_diff / 100),
      urate_tariff = urate_baseline + ruc_diff,
      employment_tariff = employment_baseline * (1 + ehhc_diff / 100),
      fed_funds_tariff = fed_funds_baseline + rmff_diff
    ) %>%
    select(
      year, quarter,
      gdp_baseline, gdp_tariff,
      employment_baseline, employment_tariff,
      urate_baseline, urate_tariff,
      pce_baseline, pce_tariff,
      fed_funds_baseline, fed_funds_tariff
    )

  return(result)
}


# =============================================================================
# HIGH-LEVEL ENTRY POINT
# =============================================================================

#' Run the full USMM surrogate pipeline
#'
#' Decomposes shocks, constructs response (base + all-perm variants),
#' and converts to levels.
#'
#' When model_params$usmm_dates is specified, the full ETR schedule is filtered
#' to just those dates before decomposition. This keeps the number of USMM
#' components small (2-3) even when ETRs produces dozens of intermediate dates.
#'
#' @param etr_results ETR calculation results
#' @param inputs Model inputs (with model_params, irfs, etc.)
#' @return List with macro_quarterly and macro_quarterly_allperm tibbles
run_usmm_surrogate <- function(etr_results, inputs) {

  irfs <- inputs$usmm_irfs
  baseline <- irfs$baseline

  # USMM impulse: the eta'-adjusted (b) rate when noncompliance is active, else the
  # pre-sub (a) rate (legacy). Both are baseline-weighted, pre-substitution — the
  # IRFs supply the dynamic macro response, so the impulse must NOT carry GTAP's
  # substitution (which would double-count). Using (b) here is what deflates the
  # macro block (GDP/unemployment/PCE) for noncompliance.
  if (isTRUE(inputs$noncompliance_active)) {
    etr_increase_by_date <- etr_results$b_etr_increase_by_date
    etr_increase <- etr_results$b_etr_increase
  } else {
    etr_increase_by_date <- etr_results$presub_etr_increase_by_date
    etr_increase <- etr_results$presub_etr_increase
  }

  # Filter to usmm_dates if specified (keeps decomposition to a few components)
  usmm_dates <- inputs$model_params$usmm_dates
  if (!is.null(usmm_dates) && !is.null(etr_increase_by_date)) {
    usmm_dates_parsed <- as.Date(usmm_dates)
    available <- etr_increase_by_date$date
    missing <- setdiff(as.character(usmm_dates_parsed), as.character(available))
    if (length(missing) > 0) {
      stop('usmm_dates not found in ETR schedule: ',
           paste(missing, collapse = ', '),
           '\n  Available dates: ',
           paste(head(sort(available), 10), collapse = ', '),
           if (length(available) > 10) ', ...' else '')
    }
    # Each retained date now enters at its own quarter, so tariffs in force
    # before the first retained date would be dropped from the response.
    first_tariff <- min(etr_increase_by_date$date[abs(etr_increase_by_date$etr_increase) > 1e-10])
    if (min(usmm_dates_parsed) > first_tariff) {
      stop('usmm_dates starts at ', min(usmm_dates_parsed), ' but tariffs are in force from ',
           first_tariff, '; the earlier tariffs would have no macro response. ',
           'Add an earlier date to usmm_dates or remove usmm_dates.')
    }
    message(sprintf('  Filtering ETR schedule to %d usmm_dates (from %d total)',
                    length(usmm_dates_parsed), nrow(etr_increase_by_date)))
    etr_increase_by_date <- etr_increase_by_date %>%
      filter(date %in% usmm_dates_parsed)
  }

  # Get parameters
  refund_2026 <- inputs$model_params$refund_2026 %||% 0
  temp_component_dates <- inputs$model_params$temp_component_dates

  # Decompose shocks
  shocks <- decompose_shocks(
    etr_increase_by_date = etr_increase_by_date,
    etr_increase = etr_increase,
    refund_2026 = refund_2026,
    temp_component_dates = temp_component_dates
  )

  message(sprintf('  USMM rate changes: %d permanent, %d temporary',
                  nrow(shocks$permanent), nrow(shocks$temporary)))
  if (abs(shocks$refund_scalar) > 1e-10) {
    message(sprintf('  Refund scalar: %.4f (= $%.0fB / $%dB)',
                    shocks$refund_scalar, refund_2026, REFUND_CALIBRATION_BN))
  }

  # Construct base-case response (respects use_temp_irf flags)
  message('  Constructing USMM base-case response...')
  diffs_base <- construct_usmm_response(shocks, irfs, use_temp_irf = TRUE)
  macro_quarterly <- convert_usmm_to_levels(diffs_base, baseline)

  # Construct all-perm variant (all components use shifted-perm)
  message('  Constructing USMM all-perm response...')
  diffs_allperm <- construct_usmm_response(shocks, irfs, use_temp_irf = FALSE)
  macro_quarterly_allperm <- convert_usmm_to_levels(diffs_allperm, baseline)

  return(list(
    macro_quarterly = macro_quarterly,
    macro_quarterly_allperm = macro_quarterly_allperm,
    shocks = shocks,
    diffs_base = diffs_base,
    diffs_allperm = diffs_allperm
  ))
}
