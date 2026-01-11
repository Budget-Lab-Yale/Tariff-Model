# =============================================================================
# 05a_predict_maus.R - Predict MAUS outputs using surrogate model
# =============================================================================
#
# This module predicts MAUS quarterly outputs using pre-estimated interpolators.
# Replaces the manual MAUS workflow with instant surrogate predictions.
#
# The surrogate is indexed by UTFIBC (the actual MAUS input shock) rather than
# ETR, which provides more accurate predictions when tariff structure varies.
#
# Usage:
#   source('src/05a_predict_maus.R')
#   maus_data <- predict_maus(maus_inputs = maus_shocks, baseline_maus = inputs$baselines$maus)
#
# =============================================================================

library(tidyverse)

# Cache for loaded surrogate (avoid re-reading RDS on every call)
.maus_surrogate_cache <- new.env(parent = emptyenv())


#' Load MAUS surrogate model (with caching)
#'
#' @param surrogate_file Path to surrogate RDS file
#' @param force_reload If TRUE, reload even if already cached
#' @return Surrogate model object
load_maus_surrogate <- function(
  surrogate_file = 'resources/maus_surrogate/interpolators.rds',
  force_reload = FALSE
) {

  # Check cache
  if (!force_reload && exists('surrogate', envir = .maus_surrogate_cache)) {
    return(get('surrogate', envir = .maus_surrogate_cache))
  }

  # Load from file
  if (!file.exists(surrogate_file)) {
    stop('MAUS surrogate file not found: ', surrogate_file,
         '\nRun estimate_maus_surrogate() first to create it')
  }

  surrogate <- readRDS(surrogate_file)

  # Cache it
  assign('surrogate', surrogate, envir = .maus_surrogate_cache)

  return(surrogate)
}


#' Predict MAUS outputs using surrogate model
#'
#' Uses pre-estimated interpolators to predict quarterly GDP, employment,
#' and unemployment rate based on UTFIBC shock magnitude.
#'
#' @param maus_inputs Tibble with MAUS input shocks containing columns:
#'   year, quarter, utfibc (from generate_maus_inputs())
#' @param baseline_maus Baseline MAUS data tibble with columns:
#'   year, quarter, gdp_baseline, employment_baseline, urate_baseline
#' @param surrogate_file Path to surrogate RDS file (default: resources/maus_surrogate/interpolators.rds)
#'
#' @return Tibble with columns:
#'   year, quarter, gdp_baseline, gdp_tariff, employment_baseline,
#'   employment_tariff, urate_baseline, urate_tariff
predict_maus <- function(
  maus_inputs,
  baseline_maus,
  surrogate_file = 'resources/maus_surrogate/interpolators.rds'
) {

  # Validate inputs
  if (!is.data.frame(maus_inputs)) {
    stop('maus_inputs must be a data frame with year, quarter, utfibc columns')
  }
  required_cols <- c('year', 'quarter', 'utfibc')
  missing_cols <- setdiff(required_cols, names(maus_inputs))
  if (length(missing_cols) > 0) {
    stop('maus_inputs missing columns: ', paste(missing_cols, collapse = ', '))
  }

  # Load surrogate
  surrogate <- load_maus_surrogate(surrogate_file)

  # Extract UTFIBC at reference quarter for interpolation lookup
  ref_year <- surrogate$utfibc_ref_year
  ref_quarter <- surrogate$utfibc_ref_quarter

  ref_row <- maus_inputs %>%
    filter(year == ref_year, quarter == ref_quarter)

  if (nrow(ref_row) != 1) {
    stop('Reference quarter ', ref_year, ' Q', ref_quarter, ' not found in maus_inputs')
  }

  utfibc_ref <- ref_row$utfibc

  # Check if UTFIBC is within training range
  utfibc_min <- surrogate$utfibc_range[1]
  utfibc_max <- surrogate$utfibc_range[2]

  if (utfibc_ref < utfibc_min || utfibc_ref > utfibc_max) {
    warning(sprintf(
      'UTFIBC %.2f is outside training range [%.2f, %.2f]. Extrapolating.',
      utfibc_ref, utfibc_min, utfibc_max
    ))
  }

  # Predict for each quarter
  quarters <- surrogate$quarters
  n_quarters <- nrow(quarters)

  gdp_tariff <- numeric(n_quarters)
  emp_tariff <- numeric(n_quarters)
  ur_tariff <- numeric(n_quarters)

  for (i in seq_len(n_quarters)) {
    key <- paste0(quarters$year[i], '_Q', quarters$quarter[i])
    interp <- surrogate$interpolators[[key]]

    gdp_tariff[i] <- interp$gdp(utfibc_ref)
    emp_tariff[i] <- interp$emp(utfibc_ref)
    ur_tariff[i] <- interp$ur(utfibc_ref)
  }

  # Build prediction tibble
  predictions <- tibble(
    year = quarters$year,
    quarter = quarters$quarter,
    gdp_tariff = gdp_tariff,
    employment_tariff = emp_tariff,
    urate_tariff = ur_tariff
  )

  # Join with baseline data
  result <- baseline_maus %>%
    select(year, quarter, gdp_baseline, employment_baseline, urate_baseline) %>%
    left_join(predictions, by = c('year', 'quarter'))

  # Validate join worked
  if (any(is.na(result$gdp_tariff))) {
    missing <- result %>%
      filter(is.na(gdp_tariff)) %>%
      mutate(yq = paste0(year, 'Q', quarter)) %>%
      pull(yq)
    stop('Surrogate missing quarters: ', paste(missing, collapse = ', '))
  }

  message(sprintf('  Predicted MAUS outputs for UTFIBC = %.2f (at %d Q%d)',
                  utfibc_ref, ref_year, ref_quarter))

  return(result)
}


#' Check if MAUS surrogate is available
#'
#' @param surrogate_file Path to surrogate RDS file
#' @return TRUE if surrogate file exists, FALSE otherwise
maus_surrogate_available <- function(
  surrogate_file = 'resources/maus_surrogate/interpolators.rds'
) {
  return(file.exists(surrogate_file))
}
