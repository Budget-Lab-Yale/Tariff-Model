#!/usr/bin/env Rscript
# =============================================================================
# update_dashboard.R - Build the State of Tariffs dashboard data CSVs
# =============================================================================
# Thin CLI over src/13_export_dashboard.R. Reads the published model scenarios
# (config/dashboard.yaml) at one interface vintage and writes per-figure
# long-format data.csv files + manifest.json + dependencies.csv under
#   <root>/model_data/Tariff-Model/v<version>/<vintage>/dashboard/
# for hand-copy into the budget-lab-interactives repo via PR.
#
# Usage:
#   Rscript update_dashboard.R --interface-vintage <V> [--write-local] \
#                              [--config config/dashboard.yaml]
#
#   --interface-vintage <V>  (required) vintage the scenarios were published under
#                            (the value passed to run.R --vintage)
#   --write-local            read model outputs from / write the dashboard under
#                            the local scratch root instead of production
#                            (tracker daily files are always read from production)
#   --config <path>          dashboard config (default: config/dashboard.yaml)
#
# Example:
#   Rscript update_dashboard.R --interface-vintage 2026070114
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

interface_vintage <- NA_character_
write_local <- FALSE
config_path <- 'config/dashboard.yaml'

i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a == '--interface-vintage' && i < length(args)) {
    interface_vintage <- args[i + 1]; i <- i + 2
  } else if (a == '--config' && i < length(args)) {
    config_path <- args[i + 1]; i <- i + 2
  } else if (a == '--write-local') {
    write_local <- TRUE; i <- i + 1
  } else {
    stop('Unknown or malformed argument: ', a,
         '\n  Usage: Rscript update_dashboard.R --interface-vintage <V> [--write-local] [--config <path>]')
  }
}

if (is.na(interface_vintage)) {
  stop('--interface-vintage <V> is required (the vintage the scenarios were published under).')
}

source('src/13_export_dashboard.R')
export_dashboard(interface_vintage = interface_vintage,
                 write_local = write_local,
                 config_path = config_path)
