# =============================================================================
# estimate_maus_surrogate.R - Build MAUS surrogate model from training runs
# =============================================================================
#
# This script builds interpolation-based surrogate models for MAUS outputs.
# Run this once after adding training data to resources/maus_training/.
#
# The surrogate interpolates on UTFIBC (the actual MAUS input shock) rather
# than ETR, which provides more accurate predictions when tariff structure
# varies (e.g., changing rates for specific countries).
#
# Usage:
#   source('src/estimation/estimate_maus_surrogate.R')
#   estimate_maus_surrogate()
#
# =============================================================================

library(tidyverse)

# Reference quarter for UTFIBC indexing (2026 Q1 - in peak impact period)
UTFIBC_REF_YEAR <- 2026
UTFIBC_REF_QUARTER <- 1

#' Load a single MAUS training run
#'
#' @param run_dir Path to training run directory containing:
#'   - shocks.csv with utfibc column AND GDP, LEB, LURC columns (MAUS outputs)
#' @return List with utfibc (numeric, reference quarter value) and quarterly (tibble)
load_training_run <- function(run_dir) {

  # Load shocks.csv which contains both UTFIBC inputs and MAUS outputs
  shocks_file <- file.path(run_dir, 'shocks.csv')

  if (!file.exists(shocks_file)) {
    stop('shocks.csv not found in: ', run_dir)
  }

  quarterly <- read_csv(shocks_file, show_col_types = FALSE)

  # Validate columns - need utfibc for indexing and MAUS outputs
  required_cols <- c('year', 'quarter', 'utfibc', 'GDP', 'LEB', 'LURC')
  missing_cols <- setdiff(required_cols, names(quarterly))
  if (length(missing_cols) > 0) {
    stop('Missing columns in ', run_dir, ': ', paste(missing_cols, collapse = ', '))
  }

  # Extract UTFIBC at reference quarter for indexing
  ref_row <- quarterly %>%
    filter(year == UTFIBC_REF_YEAR, quarter == UTFIBC_REF_QUARTER)

  if (nrow(ref_row) != 1) {
    stop('Reference quarter ', UTFIBC_REF_YEAR, ' Q', UTFIBC_REF_QUARTER,
         ' not found in: ', run_dir)
  }

  utfibc_ref <- ref_row$utfibc

  return(list(
    utfibc = utfibc_ref,
    quarterly = quarterly
  ))
}


#' Build interpolators from training data
#'
#' Creates approxfun interpolators for each quarter × variable combination.
#' Uses UTFIBC (at reference quarter) as the x-axis for interpolation.
#'
#' @param training_runs List of training runs from load_training_run()
#' @return List with:
#'   - interpolators: nested list of approxfun objects [quarter_key][variable]
#'   - utfibc_range: c(min, max) of training UTFIBC values
#'   - quarters: tibble of year/quarter combinations
build_interpolators <- function(training_runs) {

  # Extract UTFIBC values (at reference quarter)
  utfibcs <- sapply(training_runs, function(x) x$utfibc)
  utfibc_range <- c(min(utfibcs), max(utfibcs))

  message(sprintf('  Building interpolators from %d training runs', length(training_runs)))
  message(sprintf('  UTFIBC range: %.2f to %.2f (at %d Q%d)',
                  utfibc_range[1], utfibc_range[2], UTFIBC_REF_YEAR, UTFIBC_REF_QUARTER))

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

    # Build approxfun interpolators indexed by UTFIBC
    # rule = 2: use nearest value for extrapolation
    interpolators[[key]] <- list(
      gdp = approxfun(utfibcs, gdp_vals, rule = 2),
      emp = approxfun(utfibcs, emp_vals, rule = 2),
      ur = approxfun(utfibcs, ur_vals, rule = 2)
    )
  }

  return(list(
    interpolators = interpolators,
    utfibc_range = utfibc_range,
    utfibc_ref_year = UTFIBC_REF_YEAR,
    utfibc_ref_quarter = UTFIBC_REF_QUARTER,
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
  surrogate$training_utfibcs <- sapply(training_runs, function(x) x$utfibc)
  surrogate$created_at <- Sys.time()

  # Save to file
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(surrogate, output_file)
  message(sprintf('Saved surrogate to: %s', output_file))

  # Summary
  message('\n=== Surrogate Model Summary ===')
  message(sprintf('Training runs: %d', surrogate$n_training_runs))
  message(sprintf('UTFIBC range: %.2f to %.2f (at %d Q%d)',
                  surrogate$utfibc_range[1], surrogate$utfibc_range[2],
                  surrogate$utfibc_ref_year, surrogate$utfibc_ref_quarter))
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
  message(sprintf('Training UTFIBCs: %s', paste(sprintf('%.2f', surrogate$training_utfibcs), collapse = ', ')))
  message(sprintf('UTFIBC range: %.2f to %.2f (at %d Q%d)',
                  surrogate$utfibc_range[1], surrogate$utfibc_range[2],
                  surrogate$utfibc_ref_year, surrogate$utfibc_ref_quarter))
  message(sprintf('Quarters: %d (%d Q%d to %d Q%d)',
                  nrow(surrogate$quarters),
                  surrogate$quarters$year[1], surrogate$quarters$quarter[1],
                  surrogate$quarters$year[nrow(surrogate$quarters)],
                  surrogate$quarters$quarter[nrow(surrogate$quarters)]))

  return(invisible(surrogate))
}
