# =============================================================================
# Command line interface for the Yale Budget Lab Tariff Model
#
# Usage:
#   Rscript run.R <scenario_name> [options]
#
# Options:
#   --markup <type>            Markup assumption: 'constant_percentage' (default,
#                              upper bound) or 'constant_dollar' (lower bound)
#
# Example:
#   Rscript run.R 2-21_perm
#   Rscript run.R 2-21_perm --markup constant_dollar
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat('Usage: Rscript run.R <scenario_name> [options]\n')
  cat('\nOptions:\n')
  cat('  --markup <type>            Markup assumption: constant_percentage (default) or constant_dollar\n')
  cat('\nAvailable scenarios:\n')
  scenarios <- list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)
  for (s in scenarios) {
    cat(sprintf('  %s\n', s))
  }
  quit(status = 1)
}

scenario <- args[1]

# Parse --markup argument
markup_assumption <- 'constant_percentage'
if ('--markup' %in% args) {
  idx <- which(args == '--markup')
  if (idx < length(args)) {
    markup_assumption <- args[idx + 1]
  }
}

# Run model
source('src/run_model.R')
run_scenario(scenario, markup_assumption = markup_assumption)
