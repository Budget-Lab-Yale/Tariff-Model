# =============================================================================
# Command line interface for the Yale Budget Lab Tariff Model
#
# Usage:
#   Rscript run.R <scenario_name> [--manual-maus]
#
# Options:
#   --manual-maus   Use manual MAUS workflow instead of surrogate model
#
# Example:
#   Rscript run.R 11-17
#   Rscript run.R 11-17 --manual-maus
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat('Usage: Rscript run.R <scenario_name> [--manual-maus]\n')
  cat('\nOptions:\n')
  cat('  --manual-maus   Use manual MAUS workflow instead of surrogate model\n')
  cat('\nAvailable scenarios:\n')
  scenarios <- list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)
  for (s in scenarios) {
    cat(sprintf('  %s\n', s))
  }
  quit(status = 1)
}

scenario <- args[1]
use_maus_surrogate <- !('--manual-maus' %in% args)

source('src/run_model.R')
run_scenario(scenario, use_maus_surrogate = use_maus_surrogate)
