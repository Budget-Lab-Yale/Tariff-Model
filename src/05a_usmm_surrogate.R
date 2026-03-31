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

  return(irfs)
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


#' Decompose a tariff scenario into permanent, temporary, and refund components
#'
#' Given a time-varying ETR schedule, decomposes it into:
#' - Permanent component: the final (last-date) ETR level
#' - Intermediate components: each step change that later reverses
#' - Refund component: scaled by actual/calibration ratio
#'
#' @param etr_increase_by_date Tibble with date and etr_increase columns (or NULL for static)
#' @param etr_increase Scalar ETR increase (used for static scenarios)
#' @param refund_2026 Refund amount in billions (default: 0)
#' @param temp_component_dates Flat list of two dates c(onset, offset) defining a single
#'   temporary window that uses the temp IRF (from model_params$temp_component_dates).
#'   Only one onset/offset pair is supported. NULL means all components use shifted-perm.
#' @return List with perm_pp, components tibble, and refund_scalar
decompose_shocks <- function(etr_increase_by_date = NULL,
                              etr_increase = NULL,
                              refund_2026 = 0,
                              temp_component_dates = NULL) {

  refund_scalar <- refund_2026 / REFUND_CALIBRATION_BN

  # Static scenario: single permanent shock, no components

  if (is.null(etr_increase_by_date)) {
    if (is.null(etr_increase)) {
      stop('Either etr_increase_by_date or etr_increase must be provided')
    }
    return(list(
      perm_pp = etr_increase * 100,  # Convert to percentage points
      components = tibble(
        onset_date = as.Date(character(0)),
        offset_date = as.Date(character(0)),
        onset_q = integer(0),
        offset_q = integer(0),
        delta_pp = numeric(0),
        use_temp_irf = logical(0)
      ),
      refund_scalar = refund_scalar
    ))
  }

  # Time-varying scenario: N-date decomposition
  schedule <- etr_increase_by_date %>%
    arrange(date) %>%
    mutate(etr_pp = etr_increase * 100)

  n_dates <- nrow(schedule)
  perm_pp <- schedule$etr_pp[n_dates]  # Last date = permanent level

  # Parse temp_component_dates into a single onset/offset pair for matching.
  # Only one pair is supported; temp_component_dates must be a flat list of
  # exactly two date strings: [onset, offset].
  temp_pairs <- NULL
  if (!is.null(temp_component_dates)) {
    stopifnot(length(temp_component_dates) == 2)
    temp_pairs <- tibble(
      onset = as.Date(temp_component_dates[[1]]),
      offset = as.Date(temp_component_dates[[2]])
    )
  }

  # Build components representing the excess above the permanent level at each
  # intermediate date. Each component captures the CUMULATIVE excess (date[i]
  # minus perm), active from date[i] to date[i+1]. This ensures the components
  # telescope correctly: at any date, perm + active component = scheduled ETR.
  #
  # For schedule [d1, d2, d3] with perm = d3:
  #   comp1: delta = d1-perm, onset=date1, offset=date2
  #   comp2: delta = d2-perm, onset=date2, offset=date3
  # At date1: d3 + (d1-d3) = d1  ✓
  # At date2: d3 + (d2-d3) = d2  ✓
  # At date3: d3            = d3  ✓

  components <- tibble(
    onset_date = as.Date(character(0)),
    offset_date = as.Date(character(0)),
    onset_q = integer(0),
    offset_q = integer(0),
    delta_pp = numeric(0),
    use_temp_irf = logical(0)
  )

  if (n_dates > 1) {
    for (i in 1:(n_dates - 1)) {
      delta <- schedule$etr_pp[i] - perm_pp
      if (abs(delta) < 1e-10) next  # Skip zero-delta components

      onset <- schedule$date[i]
      offset <- schedule$date[i + 1]

      # Check if this component matches temp_component_dates
      use_temp <- FALSE
      if (!is.null(temp_pairs)) {
        use_temp <- any(temp_pairs$onset == onset & temp_pairs$offset == offset)
      }

      components <- bind_rows(components, tibble(
        onset_date = onset,
        offset_date = offset,
        onset_q = date_to_quarter_index(onset),
        offset_q = date_to_quarter_index(offset),
        delta_pp = delta,
        use_temp_irf = use_temp
      ))
    }
  }

  return(list(
    perm_pp = perm_pp,
    components = components,
    refund_scalar = refund_scalar
  ))
}


# =============================================================================
# RESPONSE CONSTRUCTION
# =============================================================================

#' Shift an IRF by a number of quarters (zero-pad the front)
#'
#' @param irf_vals Numeric vector of IRF values
#' @param shift Number of quarters to shift forward (positive = delay)
#' @return Shifted numeric vector (same length as input)
shift_irf <- function(irf_vals, shift) {
  n <- length(irf_vals)
  if (shift <= 0) return(irf_vals)
  if (shift >= n) return(rep(0, n))
  c(rep(0, shift), irf_vals[1:(n - shift)])
}


#' Construct composite USMM response from decomposed shocks
#'
#' Builds the full macro response by summing:
#' 1. Permanent component: scaled perm IRF
#' 2. Intermediate components: either temp IRF or shifted-perm difference
#' 3. Refund component: scaled refund IRF
#'
#' @param shocks Decomposition from decompose_shocks()
#' @param irfs IRF data from load_usmm_irfs()
#' @param use_temp_irf If FALSE, ALL components use shifted-perm (for all-perm variant)
#' @return Tibble with year, quarter, and diff columns for each USMM variable
construct_usmm_response <- function(shocks, irfs, use_temp_irf = TRUE) {

  n_quarters <- nrow(irfs$perm$`2`)

  # Start with year/quarter from any IRF
  result <- irfs$perm$`2` %>%
    select(year, quarter)

  for (var in USMM_VARS) {
    result[[var]] <- 0
  }

  # 1. Permanent component
  if (abs(shocks$perm_pp) > 1e-10) {
    perm_irf <- interpolate_irf(irfs$perm, shocks$perm_pp, PERM_SIZES)
    for (var in USMM_VARS) {
      result[[var]] <- result[[var]] + perm_irf[[var]]
    }
  }

  # 2. Intermediate components
  if (nrow(shocks$components) > 0) {
    for (i in seq_len(nrow(shocks$components))) {
      comp <- shocks$components[i, ]
      delta <- comp$delta_pp

      # Quarter shift relative to USMM shock onset (2025Q2 = index 6)
      onset_shift <- comp$onset_q - USMM_SHOCK_QUARTER
      offset_shift <- comp$offset_q - USMM_SHOCK_QUARTER

      if (isTRUE(comp$use_temp_irf) && use_temp_irf) {
        # Use temp IRF (expectations channel for announced temporary tariffs)
        temp_irf <- interpolate_irf(irfs$temp, delta, TEMP_SIZES)
        for (var in USMM_VARS) {
          shifted <- shift_irf(temp_irf[[var]], onset_shift)
          result[[var]] <- result[[var]] + shifted
        }
      } else {
        # Shifted-perm: perm_irf(t - onset) - perm_irf(t - offset)
        perm_irf <- interpolate_irf(irfs$perm, delta, PERM_SIZES)
        for (var in USMM_VARS) {
          on_vals <- shift_irf(perm_irf[[var]], onset_shift)
          off_vals <- shift_irf(perm_irf[[var]], offset_shift)
          result[[var]] <- result[[var]] + (on_vals - off_vals)
        }
      }
    }
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

  # Get pre-sub ETR data for decomposition (raw from Tariff-ETRs, no GTAP scaling)
  etr_increase_by_date <- etr_results$presub_etr_increase_by_date
  etr_increase <- etr_results$presub_etr_increase

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

  # Log decomposition
  message(sprintf('  Permanent shock: %.2f pp', shocks$perm_pp))
  if (nrow(shocks$components) > 0) {
    for (i in seq_len(nrow(shocks$components))) {
      comp <- shocks$components[i, ]
      irf_type <- if (comp$use_temp_irf) 'temp' else 'shifted-perm'
      message(sprintf('  Component %d: %+.2f pp (%s to %s, %s)',
                      i, comp$delta_pp, comp$onset_date, comp$offset_date, irf_type))
    }
  }
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
