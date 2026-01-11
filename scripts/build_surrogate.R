# Build MAUS surrogate from calibration data
# Run this script from the project root

library(readr)

message('=== Building MAUS Surrogate ===')

# Parameters
training_dir <- 'output/maus_calibration'
output_file <- 'resources/maus_surrogate/interpolators.rds'

# Find training runs
run_dirs <- list.dirs(training_dir, recursive = FALSE, full.names = TRUE)
message(sprintf('Found %d training runs in %s', length(run_dirs), training_dir))

# Load all runs
training_runs <- list()
for (dir in run_dirs) {
  name <- basename(dir)
  etr_file <- file.path(dir, 'etr.txt')
  shocks_file <- file.path(dir, 'shocks.csv')

  etr <- as.numeric(readLines(etr_file, n = 1))
  quarterly <- read_csv(shocks_file, show_col_types = FALSE)

  training_runs[[name]] <- list(etr = etr, quarterly = quarterly)
  message(sprintf('  Loaded %s: ETR = %.4f (%.2f%%)', name, etr, etr * 100))
}

# Extract ETRs and sort
etrs <- sapply(training_runs, function(x) x$etr)
etr_order <- order(etrs)
etrs <- etrs[etr_order]
training_runs <- training_runs[etr_order]

etr_range <- c(min(etrs), max(etrs))
message(sprintf('ETR range: %.2f%% to %.2f%%', etr_range[1] * 100, etr_range[2] * 100))

# Get quarter structure from first run
quarters <- training_runs[[1]]$quarterly[, c('year', 'quarter')]
n_quarters <- nrow(quarters)
message(sprintf('Quarters: %d', n_quarters))

# Build interpolators for each quarter
message('Building interpolators...')
interpolators <- list()

for (i in seq_len(n_quarters)) {
  yr <- quarters$year[i]
  qtr <- quarters$quarter[i]
  key <- paste0(yr, '_Q', qtr)

  # Extract values for this quarter across all training runs
  gdp_vals <- sapply(training_runs, function(x) x$quarterly$GDP[i])
  emp_vals <- sapply(training_runs, function(x) x$quarterly$LEB[i])
  ur_vals <- sapply(training_runs, function(x) x$quarterly$LURC[i])

  # Build approxfun interpolators (rule = 2 for constant extrapolation)
  interpolators[[key]] <- list(
    gdp = approxfun(etrs, gdp_vals, rule = 2),
    emp = approxfun(etrs, emp_vals, rule = 2),
    ur = approxfun(etrs, ur_vals, rule = 2)
  )
}

# Build surrogate object
surrogate <- list(
  interpolators = interpolators,
  etr_range = etr_range,
  quarters = quarters,
  n_training_runs = length(training_runs),
  training_etrs = etrs,
  created_at = Sys.time()
)

# Save
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(surrogate, output_file)

message(sprintf('\nSaved surrogate to: %s', output_file))
message('\n=== Surrogate Model Summary ===')
message(sprintf('Training runs: %d', surrogate$n_training_runs))
message(sprintf('Training ETRs: %s', paste(sprintf('%.2f%%', surrogate$training_etrs * 100), collapse = ', ')))
message(sprintf('ETR range: %.2f%% to %.2f%%', etr_range[1] * 100, etr_range[2] * 100))
message(sprintf('Quarters: %d', n_quarters))
message('Done!')
