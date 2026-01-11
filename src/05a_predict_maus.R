# =============================================================================
# 05a_predict_maus.R - Predict MAUS outputs using surrogate model
# =============================================================================
#
# This module predicts MAUS quarterly outputs using pre-estimated interpolators.
# Replaces the manual MAUS workflow with instant surrogate predictions.
#
# Usage:
#   source('src/05a_predict_maus.R')
#   maus_data <- predict_maus(etr = 0.10, baseline_maus = inputs$baselines$maus)
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
#' and unemployment rate for a given ETR magnitude.
#'
#' @param etr Post-substitution ETR increase as decimal (e.g., 0.10 for 10%)
#' @param baseline_maus Baseline MAUS data tibble with columns:
#'   year, quarter, gdp_baseline, employment_baseline, urate_baseline
#' @param surrogate_file Path to surrogate RDS file (default: resources/maus_surrogate/interpolators.rds)
#'
#' @return Tibble with columns:
#'   year, quarter, gdp_baseline, gdp_tariff, employment_baseline,
#'   employment_tariff, urate_baseline, urate_tariff
predict_maus <- function(
  etr,
  baseline_maus,
  surrogate_file = 'resources/maus_surrogate/interpolators.rds'
) {

  # Validate inputs
  if (!is.numeric(etr) || length(etr) != 1) {
    stop('etr must be a single numeric value')
  }
  if (etr < 0) {
    stop('etr must be non-negative')
  }

  # Load surrogate
  surrogate <- load_maus_surrogate(surrogate_file)

  # Check if ETR is within training range
  etr_min <- surrogate$etr_range[1]
  etr_max <- surrogate$etr_range[2]

  if (etr < etr_min || etr > etr_max) {
    warning(sprintf(
      'ETR %.2f%% is outside training range [%.2f%%, %.2f%%]. Extrapolating.',
      etr * 100, etr_min * 100, etr_max * 100
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

    gdp_tariff[i] <- interp$gdp(etr)
    emp_tariff[i] <- interp$emp(etr)
    ur_tariff[i] <- interp$ur(etr)
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

  message(sprintf('  Predicted MAUS outputs for ETR = %.2f%%', etr * 100))

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
