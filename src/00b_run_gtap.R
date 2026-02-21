# ============================================================================
# 00b_run_gtap.R
#
# Run GTAP model for a scenario
# ============================================================================

#' Load global GTAP configuration
#'
#' Loads GTAP paths from config/gtap_config.yaml
#'
#' @return List with GTAP configuration
load_gtap_config <- function() {
  config_file <- 'config/gtap_config.yaml'

  if (!file.exists(config_file)) {
    stop(
      'GTAP configuration file not found: ', config_file, '\n',
      'Please create this file with your GTAP installation paths.\n',
      'See README.md for setup instructions.'
    )
  }

  config <- yaml::read_yaml(config_file)

  # Normalize paths (convert backslashes to forward slashes)
  config$gtap_executable <- str_replace_all(config$gtap_executable, '\\\\', '/')

config$gtap_work_dir <- str_replace_all(config$gtap_work_dir, '\\\\', '/')
  config$gtap_data_dir <- str_replace_all(config$gtap_data_dir, '\\\\', '/')
  config$gtap_aux_dir <- str_replace_all(config$gtap_aux_dir, '\\\\', '/')
  config$sltoht_executable <- str_replace_all(config$sltoht_executable, '\\\\', '/')

  return(config)
}


#' Check GTAP availability and prompt user if not found
#'
#' Validates that all required GTAP components are installed and accessible.
#' If components are missing, provides helpful error messages with setup instructions.
#'
#' @param gtap_config GTAP configuration from load_gtap_config()
#' @return TRUE if all checks pass, otherwise stops with error
check_gtap_availability <- function(gtap_config = NULL) {

  if (is.null(gtap_config)) {
    gtap_config <- tryCatch(
      load_gtap_config(),
      error = function(e) {
        message('\n', strrep('=', 60))
        message('GTAP CONFIGURATION REQUIRED')
        message(strrep('=', 60))
        message('\nGTAP is not configured. Please follow these steps:\n')
        message('1. Copy config/gtap_config.yaml.example to config/gtap_config.yaml')
        message('2. Edit config/gtap_config.yaml with your GTAP installation paths')
        message('3. Re-run the model\n')
        message('Required GTAP components:')
        message('  - RunGTAP (gtapv7.exe) from https://www.gtap.agecon.purdue.edu/')
        message('  - GEMPACK (sltoht.exe) from https://www.copsmodels.com/gempack.htm')
        message('  - GTAP database files (sets.har, basedata.har, default.prm)\n')
        message(strrep('=', 60))
        stop('GTAP configuration missing. See above for setup instructions.')
      }
    )
  }

  issues <- list()

  # Check GTAP executable
if (!file.exists(gtap_config$gtap_executable)) {
    issues <- c(issues, paste0(
      'GTAP executable not found: ', gtap_config$gtap_executable, '\n',
      '  -> Install RunGTAP from https://www.gtap.agecon.purdue.edu/'
    ))
  }

  # Check GTAP work directory
  if (!dir.exists(gtap_config$gtap_work_dir)) {
    issues <- c(issues, paste0(
      'GTAP work directory not found: ', gtap_config$gtap_work_dir, '\n',
      '  -> This should be created during RunGTAP installation'
    ))
  }

  # Check GTAP data directory and required files
  if (!dir.exists(gtap_config$gtap_data_dir)) {
    issues <- c(issues, paste0(
      'GTAP data directory not found: ', gtap_config$gtap_data_dir, '\n',
      '  -> This should contain sets.har, basedata.har, default.prm'
    ))
  } else {
    required_files <- c('sets.har', 'basedata.har', 'default.prm')
    for (f in required_files) {
      fpath <- file.path(gtap_config$gtap_data_dir, f)
      if (!file.exists(fpath)) {
        issues <- c(issues, paste0('Required GTAP file not found: ', fpath))
      }
    }
  }

  # Check sltoht executable
  if (!file.exists(gtap_config$sltoht_executable)) {
    issues <- c(issues, paste0(
      'sltoht executable not found: ', gtap_config$sltoht_executable, '\n',
      '  -> Install GEMPACK from https://www.copsmodels.com/gempack.htm'
    ))
  }

  # Report issues
  if (length(issues) > 0) {
    message('\n', strrep('=', 60))
    message('GTAP SETUP ISSUES DETECTED')
    message(strrep('=', 60), '\n')

    for (issue in issues) {
      message('- ', issue, '\n')
    }

    message('\nPlease update config/gtap_config.yaml with correct paths.')
    message('See README.md section "GTAP Setup" for detailed instructions.\n')
    message(strrep('=', 60))

    stop('GTAP setup incomplete. See above for details.')
  }

  message('  GTAP configuration validated')
  return(TRUE)
}


#' Run GTAP model for a scenario
#'
#' Generates a CMF file from template + shocks and executes gtapv7.exe
#'
#' @param scenario Name of the scenario
#' @param include_retaliation Include retaliation shocks (default: TRUE)
#' @return List with paths to solution files and exit code
run_gtap <- function(scenario, include_retaliation = TRUE) {

  # Load global GTAP configuration
  gtap_config <- load_gtap_config()

  # Validate GTAP installation
  check_gtap_availability(gtap_config)

  # Paths
  scenario_dir <- file.path('config', 'scenarios', scenario)
  output_dir <- file.path('output', scenario, 'gtap')
  template_file <- 'resources/gtap/cmf_template.cmf'
  retaliation_file <- file.path(scenario_dir, 'retaliation', 'shocks.txt')

  # Select shocks file: flat file for static, date subdirectory for time-varying
  flat_shocks <- file.path('output', scenario, 'tariff_etrs', 'shocks.txt')
  if (file.exists(flat_shocks)) {
    shocks_file <- flat_shocks
  } else {
    # Time-varying: look for date subdirectories
    tariff_etrs_dir <- file.path('output', scenario, 'tariff_etrs')
    date_dirs <- sort(list.dirs(tariff_etrs_dir, recursive = FALSE, full.names = FALSE))
    if (length(date_dirs) == 0) {
      stop('No shocks.txt found (flat or date-subdirectory) in: ', tariff_etrs_dir)
    }
    # Use gtap_reference_date from model_params (default: first date)
    params_check <- yaml::read_yaml(file.path(scenario_dir, 'model_params.yaml'))
    ref_date <- params_check$gtap_reference_date %||% date_dirs[1]
    ref_date <- as.character(ref_date)
    if (!ref_date %in% date_dirs) {
      stop('gtap_reference_date "', ref_date, '" not found in: ',
           paste(date_dirs, collapse = ', '))
    }
    shocks_file <- file.path(tariff_etrs_dir, ref_date, 'shocks.txt')
    if (!file.exists(shocks_file)) {
      stop('Shocks file not found for reference date: ', shocks_file)
    }
    message(sprintf('  Using time-varying shocks for reference date: %s', ref_date))
  }

  # Load scenario-specific params (for retaliation flag override)
  params <- yaml::read_yaml(file.path(scenario_dir, 'model_params.yaml'))

  # Use global config for GTAP paths
  gtap_exe <- gtap_config$gtap_executable
  gtap_work <- gtap_config$gtap_work_dir
  gtap_data <- gtap_config$gtap_data_dir
  gtap_aux <- gtap_config$gtap_aux_dir
  sltoht_exe <- gtap_config$sltoht_executable

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Build CMF file
  # Use forward slashes for GEMPACK compatibility
  solution_path <- normalizePath(file.path(output_dir, scenario), mustWork = FALSE)
  solution_path <- str_replace_all(solution_path, '\\\\', '/')

  # Replace all placeholders in CMF template
  cmf <- read_file(template_file) %>%
    str_replace(fixed('{{SOLUTION_FILE}}'), solution_path) %>%
    str_replace(fixed('{{SCENARIO_NAME}}'), scenario) %>%
    str_replace(fixed('{{GTAP_AUX_DIR}}'), gtap_aux) %>%
    str_replace_all(fixed('{{GTAP_DATA_DIR}}'), gtap_data)

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

  # Resolve paths that are relative to repo root before changing directories
  map_file <- params$gtap$map_file %||% 'resources/gtap/tariff_model.map'
  map_file_abs <- normalizePath(map_file, mustWork = TRUE)
  output_dir_abs <- normalizePath(output_dir, mustWork = TRUE)

  # Run GTAP (from work directory for relative paths in CMF)
  message('  Running GTAP...')
  old_wd <- getwd()
  setwd(gtap_work)
  on.exit(setwd(old_wd), add = TRUE)

  result <- system2(
    gtap_exe,
    args = c('-cmf', cmf_path),
    stdout = TRUE,
    stderr = TRUE
  )

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

  # Run sltoht from output directory (scoped so on.exit restores immediately)
  # sltoht reads .sl4 and writes .sol (HAR format)
  # Input format: sl4_file, then output_file, then map_file via stdin
  sltoht_input <- paste0(basename(sol_file), '\n', map_file_abs, '\n\n')
  sltoht_result <- local({
    old_wd2 <- getwd()
    setwd(output_dir_abs)
    on.exit(setwd(old_wd2))

    system2(
      sltoht_exe,
      args = basename(sl4_file),
      input = sltoht_input,
      stdout = TRUE,
      stderr = TRUE
    )
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
