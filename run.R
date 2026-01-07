# =============================================================================
# Command line interface for the Yale Budget Lab Tariff Model
#
# Usage:
#   Rscript run.R <scenario_name>
#
# Example:
#   Rscript run.R 11-17
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  cat('Usage: Rscript run.R <scenario_name>\n')
  cat('\nAvailable scenarios:\n')
  scenarios <- list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)
  for (s in scenarios) {
    cat(sprintf('  %s\n', s))
  }
  quit(status = 1)
}

scenario <- args[1]

source('src/run_model.R')
run_scenario(scenario)
