# Build MAUS surrogate from calibration data
# Run this script from the project root
#
# The surrogate is indexed by UTFIBC (the actual MAUS input shock) rather
# than ETR, which provides more accurate predictions when tariff structure
# varies (e.g., changing rates for specific countries).

library(readr)

message('=== Building MAUS Surrogate (UTFIBC-indexed) ===')

# Parameters
training_dir <- 'output/maus_calibration'
output_file <- 'resources/maus_surrogate/interpolators.rds'

# Reference quarter for UTFIBC indexing (2026 Q1 - in peak impact period)
UTFIBC_REF_YEAR <- 2026
UTFIBC_REF_QUARTER <- 1

# Find training runs
run_dirs <- list.dirs(training_dir, recursive = FALSE, full.names = TRUE)
message(sprintf('Found %d training runs in %s', length(run_dirs), training_dir))

# Load all runs
training_runs <- list()
for (dir in run_dirs) {
  name <- basename(dir)
  shocks_file <- file.path(dir, 'shocks.csv')

  quarterly <- read_csv(shocks_file, show_col_types = FALSE)

  # Extract UTFIBC at reference quarter
  ref_row <- quarterly[quarterly$year == UTFIBC_REF_YEAR &
                         quarterly$quarter == UTFIBC_REF_QUARTER, ]
  utfibc_ref <- ref_row$utfibc

  training_runs[[name]] <- list(utfibc = utfibc_ref, quarterly = quarterly)
  message(sprintf('  Loaded %s: UTFIBC = %.2f (at %d Q%d)',
                  name, utfibc_ref, UTFIBC_REF_YEAR, UTFIBC_REF_QUARTER))
}

# Extract UTFIBCs and sort
utfibcs <- sapply(training_runs, function(x) x$utfibc)
utfibc_order <- order(utfibcs)
utfibcs <- utfibcs[utfibc_order]
training_runs <- training_runs[utfibc_order]

utfibc_range <- c(min(utfibcs), max(utfibcs))
message(sprintf('UTFIBC range: %.2f to %.2f', utfibc_range[1], utfibc_range[2]))

# Get quarter structure from first run
quarters <- training_runs[[1]]$quarterly[, c('year', 'quarter')]
n_quarters <- nrow(quarters)
message(sprintf('Quarters: %d', n_quarters))

# Build interpolators for each quarter
message('Building interpolators (indexed by UTFIBC)...')
interpolators <- list()

for (i in seq_len(n_quarters)) {
  yr <- quarters$year[i]
  qtr <- quarters$quarter[i]
  key <- paste0(yr, '_Q', qtr)

  # Extract values for this quarter across all training runs
  gdp_vals <- sapply(training_runs, function(x) x$quarterly$GDP[i])
  emp_vals <- sapply(training_runs, function(x) x$quarterly$LEB[i])
  ur_vals <- sapply(training_runs, function(x) x$quarterly$LURC[i])

  # Build approxfun interpolators indexed by UTFIBC (rule = 2 for constant extrapolation)
  interpolators[[key]] <- list(
    gdp = approxfun(utfibcs, gdp_vals, rule = 2),
    emp = approxfun(utfibcs, emp_vals, rule = 2),
    ur = approxfun(utfibcs, ur_vals, rule = 2)
  )
}

# Build surrogate object
surrogate <- list(
  interpolators = interpolators,
  utfibc_range = utfibc_range,
  utfibc_ref_year = UTFIBC_REF_YEAR,
  utfibc_ref_quarter = UTFIBC_REF_QUARTER,
  quarters = quarters,
  n_training_runs = length(training_runs),
  training_utfibcs = utfibcs,
  created_at = Sys.time()
)

# Save
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(surrogate, output_file)

message(sprintf('\nSaved surrogate to: %s', output_file))
message('\n=== Surrogate Model Summary ===')
message(sprintf('Training runs: %d', surrogate$n_training_runs))
message(sprintf('Training UTFIBCs: %s', paste(sprintf('%.2f', surrogate$training_utfibcs), collapse = ', ')))
message(sprintf('UTFIBC range: %.2f to %.2f (at %d Q%d)',
                utfibc_range[1], utfibc_range[2], UTFIBC_REF_YEAR, UTFIBC_REF_QUARTER))
message(sprintf('Quarters: %d', n_quarters))
message('Done!')
