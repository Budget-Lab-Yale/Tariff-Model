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

# The same clock governs GDP, tariff collections and the response of imports.
long_run_weight <- function(year, quarter, timing = NULL) {
  if (is.null(timing)) {
    timing <- yaml::read_yaml('config/global_assumptions.yaml')$long_run_transition
  }
  required <- c('start_year', 'start_quarter', 'quarters')
  if (!all(required %in% names(timing)) ||
      !all(vapply(timing[required], function(x) {
        is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)
      }, logical(1))) || !timing$start_quarter %in% 1:4 || timing$quarters < 1) {
    stop('Invalid long_run_transition in global assumptions')
  }
  elapsed <- 4 * (year - timing$start_year) + quarter - timing$start_quarter
  pmin(1, pmax(0, elapsed / timing$quarters))
}

blend_usmm_gdp_deviation <- function(year, quarter, raw_deviation,
                                     gtap_long_run_gdp = NULL, timing = NULL) {
  if (is.null(gtap_long_run_gdp)) return(raw_deviation)
  weight <- long_run_weight(year, quarter, timing)
  (1 - weight) * raw_deviation + weight * gtap_long_run_gdp
}
