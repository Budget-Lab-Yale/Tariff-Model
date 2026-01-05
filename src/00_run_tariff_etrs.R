# =============================================================================
# 00_run_tariff_etrs.R - Run Tariff-ETRs model via CLI
# =============================================================================

#' Run Tariff-ETRs for a scenario
#'
#' Calls the Tariff-ETRs model via Rscript, passing this model's config
#' and receiving ETR outputs.
#'
#' @param scenario Name of the scenario
#' @param tariff_etrs_path Path to Tariff-ETRs repo (default: ../Tariff-ETRs)
#'
#' @return Invisibly returns the exit code
run_tariff_etrs <- function(scenario, tariff_etrs_path = '../Tariff-ETRs') {

  # Paths
  # Tariff-ETRs expects: config_dir/scenario/232.yaml
  # Our structure: config/scenarios/{scenario}/tariff_etrs/232.yaml
  # So we pass config_dir = config/scenarios/{scenario} and scenario = tariff_etrs
  scenario_dir <- file.path('config', 'scenarios', scenario)
  output_dir <- file.path('output', scenario)
  script_path <- file.path(tariff_etrs_path, 'src', 'main.R')

  # Verify paths exist
  if (!file.exists(script_path)) {
    stop(sprintf('Tariff-ETRs script not found: %s', normalizePath(script_path, mustWork = FALSE)))
  }

  tariff_etrs_config <- file.path(scenario_dir, 'tariff_etrs')
  if (!dir.exists(tariff_etrs_config)) {
    stop(sprintf('Tariff-ETRs config not found: %s', tariff_etrs_config))
  }

  message(sprintf('  Config: %s', tariff_etrs_config))
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

  # Run Tariff-ETRs
  message('  Running Tariff-ETRs...')

  # Change to Tariff-ETRs directory so relative paths in that script work
  old_wd <- getwd()
  setwd(tariff_etrs_path)

  result <- tryCatch({
    system2(
      rscript,
      args = c(
        'src/main.R',
        '--scenario', 'tariff_etrs',
        '--config-dir', normalizePath(file.path(old_wd, scenario_dir), mustWork = FALSE),
        '--output-dir', normalizePath(file.path(old_wd, output_dir), mustWork = FALSE)
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  }, finally = {
    setwd(old_wd)
  })

  # Check for errors
  exit_code <- attr(result, 'status')
  if (!is.null(exit_code) && exit_code != 0) {
    message('Tariff-ETRs output:')
    message(paste(result, collapse = '\n'))
    stop(sprintf('Tariff-ETRs failed with exit code %d', exit_code))
  }

  # Print summary from output
  message('  Tariff-ETRs complete')

  invisible(exit_code)
}
