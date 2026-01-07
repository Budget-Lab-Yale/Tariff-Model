# ============================================================================
# 00b_run_gtap.R
#
# Run GTAP model for a scenario
# ============================================================================

#' Run GTAP model for a scenario
#'
#' Generates a CMF file from template + shocks and executes gtapv7.exe
#'
#' @param scenario Name of the scenario
#' @param include_retaliation Include retaliation shocks (default: TRUE)
#' @return List with paths to solution files and exit code
run_gtap <- function(scenario, include_retaliation = TRUE) {

  # Paths
  scenario_dir <- file.path('config', 'scenarios', scenario)
  output_dir <- file.path('output', scenario, 'gtap')
  shocks_file <- file.path('output', scenario, 'tariff_etrs', 'shocks.txt')
  template_file <- 'resources/gtap/cmf_template.cmf'
  retaliation_file <- 'config/retaliation/shocks.txt'

  # Load model params for GTAP config
  params <- yaml::read_yaml(file.path(scenario_dir, 'model_params.yaml'))
  gtap_exe <- params$gtap$executable %||% 'C:/Users/jar335/Documents/GTAP/gtapv7.exe'
  gtap_work <- params$gtap$work_dir %||% 'C:/Users/jar335/Documents/GTAP/work'

  # Verify GTAP executable exists
  if (!file.exists(gtap_exe)) {
    stop('GTAP executable not found: ', gtap_exe)
  }

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Build CMF file
  # Use forward slashes for GEMPACK compatibility
  solution_path <- normalizePath(file.path(output_dir, scenario), mustWork = FALSE)
  solution_path <- str_replace_all(solution_path, '\\\\', '/')
  cmf <- read_file(template_file) %>%
    str_replace(fixed('{{SOLUTION_FILE}}'), solution_path) %>%
    str_replace(fixed('{{SCENARIO_NAME}}'), scenario)

  # Read and transform shocks (lowercase for GEMPACK)
  tariff_shocks <- read_lines(shocks_file) %>%
    str_subset('^\\s*$', negate = TRUE) %>%
    str_to_lower()

  # Optionally add retaliation
  if (include_retaliation && file.exists(retaliation_file)) {
    retaliation <- read_lines(retaliation_file) %>%
      str_subset('^[^#]') %>%
      str_subset('^\\s*$', negate = TRUE) %>%
      str_to_lower()
    tariff_shocks <- c(tariff_shocks, retaliation)
    message('  Including retaliation shocks')
  }

  # Write CMF (use absolute path for GTAP)
  cmf_path <- file.path(output_dir, paste0(scenario, '.cmf'))
  write_lines(c(cmf, tariff_shocks), cmf_path)
  cmf_path <- normalizePath(cmf_path)
  message('  CMF written: ', cmf_path)

  # Run GTAP (from work directory for relative paths in CMF)
  message('  Running GTAP...')
  old_wd <- getwd()
  setwd(gtap_work)

  result <- tryCatch({
    system2(
      gtap_exe,
      args = c('-cmf', cmf_path),
      stdout = TRUE,
      stderr = TRUE
    )
  }, finally = {
    setwd(old_wd)
  })

  # Check exit code
  exit_code <- attr(result, 'status') %||% 0
  if (exit_code != 0) {
    message('GTAP output:')
    message(paste(result, collapse = '\n'))
    stop('GTAP failed with exit code ', exit_code)
  }

  # Verify .sl4 file created
  sl4_file <- paste0(solution_path, '.sl4')
  slc_file <- paste0(solution_path, '.slc')
  sol_file <- paste0(solution_path, '.sol')

  if (!file.exists(sl4_file)) {
    stop('GTAP did not create solution file: ', sl4_file)
  }

  message('  GTAP simulation complete')

  # Post-process: Extract solution variables to HAR file using sltoht
  message('  Extracting solution variables (sltoht)...')
  sltoht_exe <- params$gtap$sltoht %||% 'C:/GP/sltoht.exe'
  map_file <- params$gtap$map_file %||% 'resources/gtap/tariff_model.map'

  if (!file.exists(sltoht_exe)) {
    stop('sltoht executable not found: ', sltoht_exe)
  }

  # Run sltoht from output directory
  # sltoht reads .sl4 and writes .sol (HAR format)
  # Input format: sl4_file, then output_file, then map_file via stdin
  # Use absolute path for map file since we change directories
  map_file_abs <- normalizePath(map_file, mustWork = TRUE)
  sltoht_input <- paste0(basename(sol_file), '\n', map_file_abs, '\n\n')
  old_wd2 <- getwd()
  setwd(output_dir)

  sltoht_result <- tryCatch({
    system2(
      sltoht_exe,
      args = basename(sl4_file),
      input = sltoht_input,
      stdout = TRUE,
      stderr = TRUE
    )
  }, finally = {
    setwd(old_wd2)
  })

  sltoht_exit <- attr(sltoht_result, 'status') %||% 0
  if (sltoht_exit != 0 || !file.exists(sol_file)) {
    message('sltoht output:')
    message(paste(sltoht_result, collapse = '\n'))
    stop('sltoht failed to create .sol file')
  }

  message('  GTAP complete')

  return(list(
    cmf = cmf_path,
    sol = sol_file,
    sl4 = sl4_file,
    slc = slc_file,
    exit_code = exit_code
  ))
}
