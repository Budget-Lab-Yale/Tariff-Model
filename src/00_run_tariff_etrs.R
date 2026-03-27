# =============================================================================
# 00_run_tariff_etrs.R - Run Tariff-ETRs model via CLI
# =============================================================================

#' Run Tariff-ETRs for a scenario
#'
#' Calls the Tariff-ETRs model via Rscript, using ETRs's own config directory.
#' ETRs owns all tariff policy configs; we just pass the scenario name.
#'
#' @param scenario Name of this model's scenario (controls output directory)
#' @param etrs_scenario Name of the scenario in Tariff-ETRs/config/
#' @param tariff_etrs_path Path to Tariff-ETRs repo
#'
#' @return Invisibly returns the exit code
run_tariff_etrs <- function(scenario, etrs_scenario, tariff_etrs_path) {

  # Paths
  output_dir <- file.path('output', scenario)
  script_path <- file.path(tariff_etrs_path, 'src', 'main.R')

  # Verify Tariff-ETRs script exists
  if (!file.exists(script_path)) {
    stop(sprintf('Tariff-ETRs script not found: %s',
                 normalizePath(script_path, mustWork = FALSE)))
  }

  # Verify ETRs scenario config exists
  etrs_config_dir <- file.path(tariff_etrs_path, 'config', etrs_scenario)
  if (!dir.exists(etrs_config_dir)) {
    stop(sprintf('Tariff-ETRs scenario config not found: %s', etrs_config_dir))
  }

  message(sprintf('  ETRs scenario: %s', etrs_scenario))
  message(sprintf('  ETRs path: %s', tariff_etrs_path))
  message(sprintf('  Output: %s', output_dir))

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Find Rscript executable
  rscript <- if (.Platform$OS.type == 'windows') {
    r_home <- R.home()
    file.path(r_home, 'bin', 'Rscript.exe')
  } else {
    'Rscript'
  }

  # Run Tariff-ETRs using its own config directory
  message('  Running Tariff-ETRs...')

  old_wd <- getwd()
  setwd(tariff_etrs_path)
  on.exit(setwd(old_wd), add = TRUE)

  result <- system2(
    rscript,
    args = c(
      'src/main.R',
      '--scenario', etrs_scenario,
      '--output-dir', normalizePath(file.path(old_wd, output_dir), mustWork = FALSE)
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  # Check for errors
  exit_code <- attr(result, 'status')
  if (!is.null(exit_code) && exit_code != 0) {
    message('Tariff-ETRs output:')
    message(paste(result, collapse = '\n'))
    stop(sprintf('Tariff-ETRs failed with exit code %d', exit_code))
  }

  # Rename ETRs output directory for downstream compatibility
  # ETRs creates: {output_dir}/{etrs_scenario}/
  # Downstream expects: {output_dir}/tariff_etrs/
  etrs_output <- file.path(old_wd, output_dir, etrs_scenario)
  target_output <- file.path(old_wd, output_dir, 'tariff_etrs')

  if (normalizePath(etrs_output, mustWork = FALSE) !=
      normalizePath(target_output, mustWork = FALSE)) {
    if (dir.exists(target_output)) {
      unlink(target_output, recursive = TRUE)
    }
    if (!dir.exists(etrs_output)) {
      stop(sprintf('Expected ETRs output not found: %s', etrs_output))
    }
    file.rename(etrs_output, target_output)
    message(sprintf('  Renamed %s/ -> tariff_etrs/', etrs_scenario))
  }

  message('  Tariff-ETRs complete')

  invisible(exit_code)
}
