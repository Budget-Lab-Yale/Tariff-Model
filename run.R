# =============================================================================
# Command line interface for the Yale Budget Lab Tariff Model
#
# Usage:
#   Rscript run.R <scenario> [<scenario> ...] [options]
#
# One or more scenarios can be run in a single invocation; they all publish
# under the SAME output vintage, so a downstream dashboard build can find them
# together. Scenarios run sequentially in this process.
#
# Options:
#   --markup <type>            Markup assumption: 'constant_dollar' (default,
#                              lower bound), 'constant_percentage' (upper bound),
#                              or 'average' (mean of upper and lower bounds)
#   --bea-io-level <level>     BEA I-O table level: 'detail' (default, ~400
#                              commodities) or 'summary' (73 commodities)
#   --vintage <id>             Output interface vintage (default: run timestamp).
#                              Shared across every scenario in this invocation.
#   --write-local              Publish under the local scratch root instead of
#                              shared production
#   --dashboard                After the runs, build the dashboard data CSVs for
#                              this vintage (config/dashboard.yaml scenario set).
#                              With no scenarios listed, runs exactly the
#                              dashboard.yaml scenarios, then builds it.
#
# Examples:
#   Rscript run.R tracker_actual_2026-07-01
#   Rscript run.R tracker_actual_2026-07-01 tracker_new_301_2026-07-01 --vintage 2026070114 --dashboard
#   Rscript run.R --dashboard --vintage 2026070114        # run the dashboard.yaml set + build
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

# --- parse: collect positional scenarios + flags ------------------------------
markup_assumption <- 'constant_dollar'
bea_io_level <- NULL
vintage <- NULL
write_local <- FALSE
build_dashboard <- FALSE
scenarios <- character(0)

i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a == '--markup' && i < length(args)) {
    markup_assumption <- args[i + 1]; i <- i + 2
  } else if (a == '--bea-io-level' && i < length(args)) {
    bea_io_level <- args[i + 1]; i <- i + 2
  } else if (a == '--vintage' && i < length(args)) {
    vintage <- args[i + 1]; i <- i + 2
  } else if (a == '--write-local') {
    write_local <- TRUE; i <- i + 1
  } else if (a == '--dashboard') {
    build_dashboard <- TRUE; i <- i + 1
  } else if (startsWith(a, '--')) {
    stop('Unknown or malformed option: ', a)
  } else {
    scenarios <- c(scenarios, a); i <- i + 1
  }
}

# With --dashboard and no scenarios named, default to the dashboard.yaml set so a
# single command runs the release scenarios and builds their dashboard. Source
# only the (light) dashboard module here — the heavy pipeline (which needs HARr /
# GTAP) is sourced below, once we know we are actually running something.
if (build_dashboard && length(scenarios) == 0) {
  source('src/13_export_dashboard.R')
  scenarios <- read_dashboard_config()$scenarios$id
  message(sprintf('--dashboard with no scenarios: running the dashboard.yaml set (%s)',
                  paste(scenarios, collapse = ', ')))
}

if (length(scenarios) == 0) {
  cat('Usage: Rscript run.R <scenario> [<scenario> ...] [options]\n')
  cat('\nOptions:\n')
  cat('  --markup <type>            constant_dollar (default), constant_percentage, or average\n')
  cat('  --bea-io-level <level>     detail (default) or summary\n')
  cat('  --vintage <id>             output vintage, shared across scenarios (default: timestamp)\n')
  cat('  --write-local              publish under the local scratch root\n')
  cat('  --dashboard                build the dashboard for this vintage after the runs\n')
  cat('\nAvailable scenarios:\n')
  for (s in list.dirs('config/scenarios', full.names = FALSE, recursive = FALSE)) {
    cat(sprintf('  %s\n', s))
  }
  quit(status = 1)
}

# Heavy pipeline (GTAP / HARr) — sourced only now that we have work to do.
source('src/run_model.R')

# Resolve ONE vintage up front so every scenario (and the dashboard) share it.
run_vintage <- vintage %||% make_vintage()
message(sprintf('Run vintage: %s | scenarios: %s%s\n',
                run_vintage, paste(scenarios, collapse = ', '),
                if (build_dashboard) ' | dashboard: yes' else ''))

# --- run each scenario --------------------------------------------------------
for (sc in scenarios) {
  run_scenario(sc, markup_assumption = markup_assumption,
               bea_io_level = bea_io_level, vintage = run_vintage,
               write_local = write_local)
}

# --- optional dashboard build -------------------------------------------------
# export_dashboard reads config/dashboard.yaml and fails loudly if any of its
# scenarios were not published at this vintage, so a partial run can't silently
# produce a stale dashboard.
if (build_dashboard) {
  message('\n==========================================================')
  message('Building dashboard data CSVs...')
  message('==========================================================')
  if (!exists('export_dashboard')) source('src/13_export_dashboard.R')
  export_dashboard(interface_vintage = run_vintage, write_local = write_local)
}
