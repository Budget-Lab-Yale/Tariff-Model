# =============================================================================
# helpers.R - Shared utility functions and constants
# =============================================================================

# =============================================================================
# GTAP REGION MAPPINGS
# =============================================================================

# Map GTAP region names to standard abbreviations
# Usage: GTAP_TO_ABBR[names(gtap_data$qgdp)]
GTAP_TO_ABBR <- c(
  usa = 'usa',
  china = 'chn',
  row = 'row',
  canada = 'can',
  mexico = 'mex',
  ftrow = 'fta',
  japan = 'jpn',
  eu = 'eu',
  uk = 'gbr'
)

# Inverse map: abbreviations to GTAP region names
# Usage: ABBR_TO_GTAP[region_abbreviation]
ABBR_TO_GTAP <- c(
  usa = 'usa',
  chn = 'china',
  row = 'row',
  can = 'canada',
  mex = 'mexico',
  fta = 'ftrow',
  jpn = 'japan',
  eu = 'eu',
  gbr = 'uk'
)

# =============================================================================
# VALIDATION HELPERS
# =============================================================================

#' Assert that a data frame has required columns
#'
#' Stops with an informative error if any required columns are missing.
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param context Optional context string for error message (e.g., 'sector_outputs')
#'
#' @examples
#' assert_has_columns(macro, c('year', 'quarter', 'gdp_baseline'), 'USMM data')
assert_has_columns <- function(data, required_cols, context = NULL) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    msg <- paste('Missing required columns:', paste(missing_cols, collapse = ', '))
    if (!is.null(context)) {
      msg <- paste(msg, 'in', context)
    }
    stop(msg)
  }
  invisible(TRUE)
}


# =============================================================================
# MACRO HELPERS
# =============================================================================

#' Blend USMM quarterly GDP deviations into a GTAP long-run target
#'
#' Applies the shared linear blend used across macro reporting:
#' 2025Q1 starts at the raw USMM deviation, then over 16 quarters the deviation
#' transitions linearly to the GTAP long-run GDP effect. Quarters before 2025Q1
#' are left unblended.
#'
#' @param year Integer vector of calendar years
#' @param quarter Integer vector of calendar quarters (1-4)
#' @param raw_deviation Numeric vector of raw USMM GDP deviations in percent
#' @param gtap_long_run_gdp Scalar GTAP long-run GDP effect in percent; NULL
#'   returns raw_deviation unchanged
#' @param blend_start_year Start year for the blend window (default: 2025)
#' @param blend_start_quarter Start quarter for the blend window (default: 1)
#' @param blend_horizon_quarters Number of quarters until pure GTAP LR (default: 16)
#' @return Numeric vector of blended GDP deviations in percent
blend_usmm_gdp_deviation <- function(year, quarter, raw_deviation,
                                     gtap_long_run_gdp = NULL,
                                     blend_start_year = 2025L,
                                     blend_start_quarter = 1L,
                                     blend_horizon_quarters = 16L) {
  if (is.null(gtap_long_run_gdp)) {
    return(raw_deviation)
  }

  q_index <- (year - blend_start_year) * 4 + (quarter - blend_start_quarter)
  blend_weight <- pmax(0, 1 - q_index / blend_horizon_quarters)

  ifelse(
    q_index < 0,
    raw_deviation,
    blend_weight * raw_deviation + (1 - blend_weight) * gtap_long_run_gdp
  )
}
