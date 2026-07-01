# =============================================================================
# On-disk output locations
# =============================================================================
# Every model artifact lives under the shared model_data interface tree
# (config/interfaces/output_roots.yaml), never inside the repo checkout — the
# same convention as Tax-Simulator / Tax-Data. Two off-repo locations:
#
#   run artifacts (per vintage):
#       <root>/model_data/Tariff-Model/v<ver>/<vintage>/<scenario>/
#       (rate_inputs/, baseline/, gtap/ subdirs + result CSVs + manifest/deps)
#   calibration (eta/alpha; produced by calibrate.R, consumed by runs; NOT
#   vintage-scoped):
#       <root>/model_data/Tariff-Model/calibration/<scenario>/
#
# run_scenario() fixes the run root once via set_run_output_root(); pipeline
# stages read it through run_scenario_dir(). Calibration paths resolve straight
# from the interface config, so calibrate.R needs no run vintage.

source('src/interfaces.R')

.paths_env <- new.env(parent = emptyenv())

#' Fix the output root for the current run (called once by run_scenario()).
set_run_output_root <- function(root) assign('run_output_root', root, envir = .paths_env)

#' The current run's output root (errors if unset — the model must publish to
#' the interface tree, so a run without a configured root is a hard error).
run_output_root <- function() {
  root <- get0('run_output_root', envir = .paths_env, ifnotfound = NULL)
  if (is.null(root)) {
    stop('Run output root not set. run_scenario() must call set_run_output_root(); ',
         'ensure config/interfaces/output_roots.yaml defines a production (or local) root.')
  }
  root
}

#' A run's per-scenario artifact dir under the vintage root.
run_scenario_dir <- function(scenario) file.path(run_output_root(), scenario)

#' <root>/model_data/Tariff-Model (production, or local when write_local).
model_data_root <- function(write_local = FALSE, cfg = read_interface_config()) {
  root <- if (write_local) cfg$roots$local else cfg$roots$production
  if (is.null(root)) stop('No output root configured in config/interfaces/output_roots.yaml')
  file.path(root, 'model_data', 'Tariff-Model')
}

#' Persistent, non-vintage calibration root (eta/alpha + IMDB cache). Overridable
#' via TARIFF_MODEL_CALIBRATION_ROOT.
calibration_root <- function(cfg = read_interface_config()) {
  env <- Sys.getenv('TARIFF_MODEL_CALIBRATION_ROOT', unset = NA_character_)
  if (!is.na(env) && nzchar(env)) return(env)
  file.path(model_data_root(cfg = cfg), 'calibration')
}
calibration_dir <- function(scenario) file.path(calibration_root(), scenario)
imdb_cache_dir  <- function() file.path(calibration_root(), '_imdb')

#' Resolve a config-supplied eta/alpha path against the calibration root.
#' Absolute paths pass through; a legacy 'output/calibration/' prefix is stripped
#' so old configs keep working.
resolve_calibration_path <- function(p) {
  if (is.null(p)) return(p)
  if (startsWith(p, '/')) return(p)
  p <- sub('^output/calibration/', '', p)
  file.path(calibration_root(), p)
}
