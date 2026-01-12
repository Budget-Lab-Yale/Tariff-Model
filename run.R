# =============================================================================
# Command line interface for the Yale Budget Lab Tariff Model
#
# Usage:
#   Rscript run.R <scenario_name> [options]
#
# Options:
#   --manual-maus              Use manual MAUS workflow instead of surrogate model
#   --report                   Generate State of Tariffs report after model run
#   --report-date YYYY-MM-DD   Date for report (required if --report is used)
#   --policy-changes "text"    Policy changes description for report (optional)
#
# Example:
#   Rscript run.R 11-17
#   Rscript run.R 11-17 --report --report-date 2025-11-17
#   Rscript run.R 11-17 --report --report-date 2025-11-17 --policy-changes "Expanded exemptions"
#   Rscript run.R 11-17 --manual-maus
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat('Usage: Rscript run.R <scenario_name> [options]\n')
  cat('\nOptions:\n')
  cat('  --manual-maus              Use manual MAUS workflow instead of surrogate model\n')
  cat('  --report                   Generate State of Tariffs report after model run\n')
  cat('  --report-date YYYY-MM-DD   Date for report (required if --report is used)\n')
  cat('  --policy-changes "text"    Policy changes description for report (optional)\n')
  cat('\nAvailable scenarios:\n')
  scenarios <- list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)
  for (s in scenarios) {
    cat(sprintf('  %s\n', s))
  }
  quit(status = 1)
}

scenario <- args[1]
use_maus_surrogate <- !('--manual-maus' %in% args)
generate_report_flag <- '--report' %in% args

# Parse --report-date argument
report_date_raw <- NULL
if ('--report-date' %in% args) {
  idx <- which(args == '--report-date')
  if (idx < length(args)) {
    report_date_raw <- args[idx + 1]
  }
}

# Parse --policy-changes argument
policy_changes <- ''
if ('--policy-changes' %in% args) {
  idx <- which(args == '--policy-changes')
  if (idx < length(args)) {
    policy_changes <- args[idx + 1]
  }
}

# Validate report options
if (generate_report_flag && is.null(report_date_raw)) {
  cat('Error: --report-date is required when using --report\n')
  cat('Example: Rscript run.R 11-17 --report --report-date 2025-11-17\n')
  quit(status = 1)
}

# Convert YYYY-MM-DD to "Month DD, YYYY" format
format_report_date <- function(date_str) {
  date_obj <- as.Date(date_str, format = '%Y-%m-%d')
  if (is.na(date_obj)) {
    stop('Invalid date format. Use YYYY-MM-DD (e.g., 2025-11-17)')
  }
  format(date_obj, '%B %d, %Y')
}

# Run model
source('src/run_model.R')
run_scenario(scenario, use_maus_surrogate = use_maus_surrogate)

# Generate figure mockups after model run
source('reports/figure_mockups.R')
save_mockups(scenario = scenario)

# Generate report if requested
if (generate_report_flag) {
  report_date <- format_report_date(report_date_raw)
  cat(sprintf('\nGenerating report for %s...\n', report_date))
  generate_report(
    baseline_scenario = scenario,
    report_date = report_date,
    policy_changes = policy_changes
  )
}
