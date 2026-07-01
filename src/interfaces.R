# =============================================================================
# interfaces.R - Budget Lab interface convention for the Tariff-Model
#
# Implements the shared-model_data interface system used across Budget Lab models
# (cf. Tax-Simulator / Tax-Data):
#   config/interfaces/interface_versions.yaml : this model's version + upstream
#       dependency versions and default vintages
#   config/interfaces/output_roots.yaml       : local + production filesystem roots
#
# Two jobs:
#   1. Resolve the tariff-rate-tracker dependency (root / vintage / scenario) from
#      the interface config, with a per-scenario override and an env-var escape
#      hatch — so a committed scenario runs unchanged on laptop and HPC.
#   2. Publish a run's results to the shared, versioned model_data tree and stamp a
#      dependencies.csv (+ manifest.json) recording which tracker vintage/scenario
#      produced them, so every output is traceable to its inputs.
# =============================================================================

INTERFACE_VERSIONS_FILE <- 'config/interfaces/interface_versions.yaml'
OUTPUT_ROOTS_FILE       <- 'config/interfaces/output_roots.yaml'

#' Read the interface config (versions + roots).
#'
#' Fields are NULL when a file is absent, so the model still runs from a scenario's
#' own rate_panel.root on a machine without the shared tree configured.
read_interface_config <- function() {
  list(
    versions = if (file.exists(INTERFACE_VERSIONS_FILE)) yaml::read_yaml(INTERFACE_VERSIONS_FILE) else NULL,
    roots    = if (file.exists(OUTPUT_ROOTS_FILE)) yaml::read_yaml(OUTPUT_ROOTS_FILE) else NULL
  )
}

#' Resolve the tariff-rate-tracker dependency into a complete rate_panel block.
#'
#' Precedence:
#'   root            : env TARIFF_RATE_TRACKER_ROOT
#'                       > <production>/model_data/Tariff-Rate-Tracker
#'                       > rate_panel$root (scenario, back-compat)
#'   vintage         : rate_panel$vintage > interface default_vintage
#'   tracker_scenario: rate_panel$tracker_scenario > interface default_id
#' Scenario-semantic fields (baseline_date, interval_end) pass through untouched.
#'
#' @param rate_panel The scenario's `rate_panel:` block (may omit root/vintage)
#' @param cfg Interface config from read_interface_config()
#' @return rate_panel with root, vintage, tracker_scenario filled
resolve_rate_panel <- function(rate_panel, cfg = read_interface_config()) {
  dep <- cfg$versions[['Tariff-Rate-Tracker']]

  env_root <- Sys.getenv('TARIFF_RATE_TRACKER_ROOT', unset = NA_character_)
  prod     <- cfg$roots$production
  rate_panel$root <- if (!is.na(env_root) && nzchar(env_root)) {
    env_root
  } else if (!is.null(prod)) {
    file.path(prod, 'model_data', 'Tariff-Rate-Tracker')
  } else {
    rate_panel$root
  }

  rate_panel$vintage          <- rate_panel$vintage %||% dep$default_vintage
  rate_panel$tracker_scenario <- rate_panel$tracker_scenario %||% dep$default_id
  rate_panel
}

#' This model's interface version (integer), default 1.
tariff_model_version <- function(cfg = read_interface_config()) {
  cfg$versions[['Tariff-Model']]$version %||% 1L
}

#' A run vintage id (timestamp). Pass an explicit one for reproducible re-runs.
make_vintage <- function() format(Sys.time(), '%Y%m%d%H%M')

#' Root dir for this model's published outputs:
#'   {production|local}/model_data/Tariff-Model/v{version}/{vintage}
#' Returns NULL when no output root is configured (publishing is then skipped).
tariff_output_root <- function(vintage, write_local = FALSE, cfg = read_interface_config()) {
  root <- if (write_local) cfg$roots$local else cfg$roots$production
  if (is.null(root)) return(NULL)
  file.path(root, 'model_data', 'Tariff-Model',
            paste0('v', tariff_model_version(cfg)), vintage)
}

#' Best-effort git commit of the model repo, for the run manifest.
git_commit <- function() {
  out <- tryCatch(
    suppressWarnings(system2('git', c('rev-parse', 'HEAD'), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (length(out) == 0) NA_character_ else out[1]
}

#' Publish a completed run to the shared versioned tree and stamp its lineage.
#'
#' Copies the scenario's results/ into {vintage_root}/{scenario}/ and writes
#' dependencies.csv (Budget Lab schema: ID, interface, version, vintage, scenario)
#' + manifest.json INTO that scenario dir (not the vintage root) so the lineage
#' stamp travels with the scenario output and concurrent one-scenario-per-job runs
#' can't clobber a shared root file. No-op (with a message) when no output root is
#' configured.
#'
#' @param scenario Scenario name
#' @param vintage This run's vintage id
#' @param resolved_rate_panel rate_panel after resolve_rate_panel() (carries the
#'   tracker vintage + scenario actually consumed)
#' @param results_dir Local dir holding the run's result CSVs (output/<scenario>/results)
#' @param markup_assumption, bea_io_level Run options (recorded in the manifest)
#' @param write_local Publish to the local root instead of production
#' @return Invisibly, the vintage root (or NULL if skipped)
publish_run <- function(scenario, vintage, resolved_rate_panel,
                        markup_assumption = NA, bea_io_level = NA,
                        write_local = FALSE, cfg = read_interface_config()) {

  vintage_root <- tariff_output_root(vintage, write_local = write_local, cfg = cfg)
  if (is.null(vintage_root)) {
    message('  No output root configured (config/interfaces/output_roots.yaml); skipping publish.')
    return(invisible(NULL))
  }

  # Step 10 already wrote the result CSVs straight into scenario_dir (it is the
  # run output root), so there is nothing to copy — only the lineage to stamp.
  scenario_dir <- file.path(vintage_root, scenario)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)

  dep <- cfg$versions[['Tariff-Rate-Tracker']]
  deps <- tibble(
    ID        = scenario,
    interface = 'Tariff-Rate-Tracker',
    version   = dep$version %||% NA,
    vintage   = resolved_rate_panel$vintage,
    scenario  = resolved_rate_panel$tracker_scenario
  )
  write_csv(deps, file.path(scenario_dir, 'dependencies.csv'))

  manifest <- list(
    model             = 'Tariff-Model',
    version           = tariff_model_version(cfg),
    vintage           = vintage,
    scenario          = scenario,
    published_at      = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z'),
    git_commit        = git_commit(),
    markup_assumption = markup_assumption,
    bea_io_level      = bea_io_level,
    dependencies      = deps
  )
  jsonlite::write_json(manifest, file.path(scenario_dir, 'manifest.json'),
                       auto_unbox = TRUE, pretty = TRUE)

  message(sprintf('  Published interface output -> %s', scenario_dir))
  message(sprintf('    dependencies.csv: Tariff-Rate-Tracker v%s / %s / %s',
                  deps$version, deps$vintage, deps$scenario))
  invisible(vintage_root)
}
