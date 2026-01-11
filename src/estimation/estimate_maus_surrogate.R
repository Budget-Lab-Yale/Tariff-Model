# =============================================================================
# estimate_maus_surrogate.R - Build MAUS surrogate model from training runs
# =============================================================================
#
# This script builds interpolation-based surrogate models for MAUS outputs.
# Run this once after adding training data to resources/maus_training/.
#
# Usage:
#   source('src/estimation/estimate_maus_surrogate.R')
#   estimate_maus_surrogate()
#
# =============================================================================

library(tidyverse)

#' Load a single MAUS training run
#'
#' @param run_dir Path to training run directory containing:
#'   - etr.txt: single value with post-sub ETR increase (decimal)
#'   - quarterly.csv OR shocks.csv with GDP, LEB, LURC columns
#' @return List with etr (numeric) and quarterly (tibble)
load_training_run <- function(run_dir) {

  etr_file <- file.path(run_dir, 'etr.txt')

  if (!file.exists(etr_file)) {
    stop('ETR file not found: ', etr_file)
  }

  # Read ETR value
  etr <- as.numeric(readLines(etr_file, n = 1))
  if (is.na(etr)) {
    stop('Invalid ETR value in: ', etr_file)
  }

  # Try quarterly.csv first, then shocks.csv (which may have MAUS outputs appended)
  quarterly_file <- file.path(run_dir, 'quarterly.csv')
  shocks_file <- file.path(run_dir, 'shocks.csv')

  if (file.exists(quarterly_file)) {
    quarterly <- read_csv(quarterly_file, show_col_types = FALSE)
  } else if (file.exists(shocks_file)) {
    quarterly <- read_csv(shocks_file, show_col_types = FALSE)
  } else {
    stop('No quarterly.csv or shocks.csv found in: ', run_dir)
  }

  # Validate columns
  required_cols <- c('year', 'quarter', 'GDP', 'LEB', 'LURC')
  missing_cols <- setdiff(required_cols, names(quarterly))
  if (length(missing_cols) > 0) {
    stop('Missing columns in ', run_dir, ': ', paste(missing_cols, collapse = ', '))
  }

  return(list(
    etr = etr,
    quarterly = quarterly
  ))
}


#' Build interpolators from training data
#'
#' Creates approxfun interpolators for each quarter × variable combination.
#'
#' @param training_runs List of training runs from load_training_run()
#' @return List with:
#'   - interpolators: nested list of approxfun objects [quarter_key][variable]
#'   - etr_range: c(min, max) of training ETR values
#'   - quarters: tibble of year/quarter combinations
build_interpolators <- function(training_runs) {

  # Extract ETR values
  etrs <- sapply(training_runs, function(x) x$etr)
  etr_range <- c(min(etrs), max(etrs))

  message(sprintf('  Building interpolators from %d training runs', length(training_runs)))
  message(sprintf('  ETR range: %.2f%% to %.2f%%', etr_range[1] * 100, etr_range[2] * 100))

  # Get quarter structure from first run
  quarters <- training_runs[[1]]$quarterly %>%
    select(year, quarter)

  n_quarters <- nrow(quarters)

  # Build interpolators for each quarter × variable
  interpolators <- list()

  for (i in seq_len(n_quarters)) {
    yr <- quarters$year[i]
    qtr <- quarters$quarter[i]
    key <- paste0(yr, '_Q', qtr)

    # Extract values for this quarter across all training runs
    gdp_vals <- sapply(training_runs, function(x) x$quarterly$GDP[i])
    emp_vals <- sapply(training_runs, function(x) x$quarterly$LEB[i])
    ur_vals <- sapply(training_runs, function(x) x$quarterly$LURC[i])

    # Build approxfun interpolators
    # rule = 2: use nearest value for extrapolation
    interpolators[[key]] <- list(
      gdp = approxfun(etrs, gdp_vals, rule = 2),
      emp = approxfun(etrs, emp_vals, rule = 2),
      ur = approxfun(etrs, ur_vals, rule = 2)
    )
  }

  return(list(
    interpolators = interpolators,
    etr_range = etr_range,
    quarters = quarters
  ))
}


#' Estimate MAUS surrogate model
#'
#' Reads training runs from resources/maus_training/, builds interpolators,
#' and saves them to resources/maus_surrogate/interpolators.rds.
#'
#' @param training_dir Directory containing training run subdirectories
#' @param output_file Path to save interpolators RDS file
#' @param force_recalculate If TRUE, recalculate even if output file exists
#'
#' @return Invisibly returns the surrogate object
estimate_maus_surrogate <- function(
  training_dir = 'resources/maus_training',
  output_file = 'resources/maus_surrogate/interpolators.rds',
  force_recalculate = FALSE
) {

  message('=== Estimating MAUS Surrogate Model ===')

  # Check if already exists
  if (file.exists(output_file) && !force_recalculate) {
    message('Surrogate file already exists: ', output_file)
    message('Use force_recalculate = TRUE to rebuild')
    return(invisible(readRDS(output_file)))
  }

  # Find training run directories
  run_dirs <- list.dirs(training_dir, recursive = FALSE, full.names = TRUE)

  if (length(run_dirs) == 0) {
    stop('No training runs found in: ', training_dir,
         '\nAdd training data subdirectories (e.g., run_01/, run_02/)')
  }

  message(sprintf('Found %d training run(s) in %s', length(run_dirs), training_dir))

  # Load all training runs
  training_runs <- lapply(run_dirs, function(dir) {
    message(sprintf('  Loading: %s', basename(dir)))
    load_training_run(dir)
  })

  # Validate all runs have same quarter structure
  n_quarters <- sapply(training_runs, function(x) nrow(x$quarterly))
  if (length(unique(n_quarters)) > 1) {
    stop('Training runs have different numbers of quarters: ', paste(n_quarters, collapse = ', '))
  }

  # Build interpolators
  surrogate <- build_interpolators(training_runs)

  # Add metadata
  surrogate$n_training_runs <- length(training_runs)
  surrogate$training_etrs <- sapply(training_runs, function(x) x$etr)
  surrogate$created_at <- Sys.time()

  # Save to file
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(surrogate, output_file)
  message(sprintf('Saved surrogate to: %s', output_file))

  # Summary
  message('\n=== Surrogate Model Summary ===')
  message(sprintf('Training runs: %d', surrogate$n_training_runs))
  message(sprintf('ETR range: %.2f%% to %.2f%%',
                  surrogate$etr_range[1] * 100, surrogate$etr_range[2] * 100))
  message(sprintf('Quarters: %d', nrow(surrogate$quarters)))
  message(sprintf('Interpolators: %d (3 variables x %d quarters)',
                  length(surrogate$interpolators) * 3, nrow(surrogate$quarters)))

  return(invisible(surrogate))
}


#' Print summary of existing surrogate model
#'
#' @param surrogate_file Path to surrogate RDS file
print_surrogate_summary <- function(surrogate_file = 'resources/maus_surrogate/interpolators.rds') {

  if (!file.exists(surrogate_file)) {
    message('No surrogate file found at: ', surrogate_file)
    message('Run estimate_maus_surrogate() to create one')
    return(invisible(NULL))
  }

  surrogate <- readRDS(surrogate_file)

  message('=== MAUS Surrogate Model ===')
  message(sprintf('File: %s', surrogate_file))
  message(sprintf('Created: %s', surrogate$created_at))
  message(sprintf('Training runs: %d', surrogate$n_training_runs))
  message(sprintf('Training ETRs: %s', paste(sprintf('%.2f%%', surrogate$training_etrs * 100), collapse = ', ')))
  message(sprintf('ETR range: %.2f%% to %.2f%%',
                  surrogate$etr_range[1] * 100, surrogate$etr_range[2] * 100))
  message(sprintf('Quarters: %d (%d Q%d to %d Q%d)',
                  nrow(surrogate$quarters),
                  surrogate$quarters$year[1], surrogate$quarters$quarter[1],
                  surrogate$quarters$year[nrow(surrogate$quarters)],
                  surrogate$quarters$quarter[nrow(surrogate$quarters)]))

  return(invisible(surrogate))
}
