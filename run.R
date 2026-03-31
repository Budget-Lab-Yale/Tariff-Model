# =============================================================================
# Command line interface for the Yale Budget Lab Tariff Model
#
# Usage:
#   Rscript run.R <scenario_name> [options]
#
# Options:
#   --markup <type>            Markup assumption: 'constant_dollar' (default,
#                              lower bound), 'constant_percentage' (upper bound),
#                              or 'average' (mean of upper and lower bounds)
#   --bea-io-level <level>     BEA I-O table level: 'detail' (default, ~400
#                              commodities) or 'summary' (73 commodities)
#
# Example:
#   Rscript run.R 2-21_perm
#   Rscript run.R 2-21_perm --markup constant_dollar
#   Rscript run.R 2-21_perm --bea-io-level detail
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat('Usage: Rscript run.R <scenario_name> [options]\n')
  cat('\nOptions:\n')
  cat('  --markup <type>            Markup assumption: constant_dollar (default), constant_percentage, or average\n')
  cat('  --bea-io-level <level>     BEA I-O table level: detail (default) or summary\n')
  cat('\nAvailable scenarios:\n')
  scenarios <- list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)
  for (s in scenarios) {
    cat(sprintf('  %s\n', s))
  }
  quit(status = 1)
}

scenario <- args[1]

# Parse --markup argument
markup_assumption <- 'constant_dollar'
if ('--markup' %in% args) {
  idx <- which(args == '--markup')
  if (idx < length(args)) {
    markup_assumption <- args[idx + 1]
  }
}

# Parse --bea-io-level argument
bea_io_level <- NULL
if ('--bea-io-level' %in% args) {
  idx <- which(args == '--bea-io-level')
  if (idx < length(args)) {
    bea_io_level <- args[idx + 1]
  }
}

# Run model
source('src/run_model.R')
run_scenario(scenario, markup_assumption = markup_assumption,
             bea_io_level = bea_io_level)
