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
#' assert_has_columns(maus, c('year', 'quarter', 'gdp_baseline'), 'MAUS data')
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
