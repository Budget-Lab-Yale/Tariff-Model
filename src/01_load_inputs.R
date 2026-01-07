# =============================================================================
# 01_load_inputs.R - Load all inputs for the tariff model
# =============================================================================

# Source GTAP file reader
source('src/read_gtap.R')

#' Load global assumptions (applies to all scenarios)
#'
#' @return List containing global assumption parameters
load_global_assumptions <- function() {

  assumptions_file <- 'config/global_assumptions.yaml'

  if (!file.exists(assumptions_file)) {
    stop('Global assumptions file not found: ', assumptions_file)
  }

  assumptions <- read_yaml(assumptions_file)
  message('  Loaded global assumptions')

  return(assumptions)
}

#' Load scenario-specific model parameters
#'
#' @param scenario_dir Path to scenario directory
#' @return List containing model parameters (or NULL if not found)
load_model_params <- function(scenario_dir) {

  params_file <- file.path(scenario_dir, 'model_params.yaml')

  if (!file.exists(params_file)) {
    return(NULL)
  }

  params <- read_yaml(params_file)
  message('  Loaded model parameters')

  return(params)
}


#' Load baseline data (applies to all scenarios)
#'
#' @return List containing baseline data
load_baselines <- function() {

  baselines <- list()

  # CBO baseline projections
  cbo_file <- 'resources/baselines/cbo.csv'
  if (file.exists(cbo_file)) {
    baselines$cbo <- read_csv(cbo_file, show_col_types = FALSE)
    message(sprintf('  Loaded CBO baselines: FY%d-FY%d',
                    min(baselines$cbo$fiscal_year),
                    max(baselines$cbo$fiscal_year)))
  } else {
    stop('CBO baseline file not found: ', cbo_file)
  }

  # GTAP baseline (no policy change)
  gtap_file <- 'resources/baselines/gtap.csv'
  if (file.exists(gtap_file)) {
    baselines$gtap <- read_csv(gtap_file, show_col_types = FALSE)
    message('  Loaded GTAP baseline')
  } else {
    stop('GTAP baseline file not found: ', gtap_file)
  }

  # VIWS baseline (baseline imports matrix for pre-substitution calculations)
  viws_baseline_file <- 'resources/gtap_baseline/viws_baseline.csv'
  if (file.exists(viws_baseline_file)) {
    viws_data <- read_csv(viws_baseline_file, show_col_types = FALSE)
    # Convert to matrix format matching GTAP output
    baselines$viws_baseline <- as.matrix(viws_data[, -1])  # Remove commodity column
    rownames(baselines$viws_baseline) <- viws_data$commodity
    message('  Loaded VIWS baseline')
  }

  # Import baseline in dollars (for Excel-compatible ETR weighting)
  import_baseline_file <- 'resources/gtap_baseline/import_baseline_dollars.csv'
  if (file.exists(import_baseline_file)) {
    import_data <- read_csv(import_baseline_file, show_col_types = FALSE)
    # Convert to matrix format
    baselines$import_baseline_dollars <- as.matrix(import_data[, -1])
    rownames(baselines$import_baseline_dollars) <- import_data$gtap_code
    message('  Loaded import baseline (dollars)')
  }

  return(baselines)
}


#' Load all inputs for a scenario
#'
#' @param scenario Name of the scenario
#'
#' @return List containing all input data
load_inputs <- function(scenario) {

  scenario_dir <- file.path('config', 'scenarios', scenario)

  if (!dir.exists(scenario_dir)) {
    stop('Scenario directory not found: ', scenario_dir)
  }

  inputs <- list()

  # ============================
  # Global assumptions
  # ============================

  inputs$assumptions <- load_global_assumptions()

  # ============================
  # Baseline data
  # ============================

  inputs$baselines <- load_baselines()

  # ============================
  # Model parameters (scenario-specific)
  # ============================

  inputs$model_params <- load_model_params(scenario_dir)

  # ============================
  # Tariff-ETRs outputs
  # ============================

  tariff_etrs_dir <- file.path('output', scenario, 'tariff_etrs')

  etr_file <- file.path(tariff_etrs_dir, 'etrs_by_sector_country.csv')
  if (file.exists(etr_file)) {
    inputs$etr_matrix <- read_csv(etr_file, show_col_types = FALSE)
    message(sprintf('  Loaded ETR matrix: %d sectors', nrow(inputs$etr_matrix)))
  } else {
    message('  No ETR matrix found (optional)')
  }

  # ============================
  # Mappings (load early for GTAP processing)
  # ============================

  gtap_mapping_file <- 'resources/mappings/gtap_sectors.csv'
  if (file.exists(gtap_mapping_file)) {
    inputs$gtap_sector_mapping <- read_csv(gtap_mapping_file, show_col_types = FALSE)
    message('  Loaded GTAP sector mappings')
  }

  # Product parameters (coefficients and weights for aggregation)
  product_params_file <- 'resources/products/product_params.csv'
  if (file.exists(product_params_file)) {
    inputs$product_params <- read_csv(product_params_file, show_col_types = FALSE)
    message('  Loaded product parameters')
  }

  # ============================
  # GTAP outputs (from solution files)
  # ============================

  gtap_solution_dir <- inputs$model_params$gtap$solution_dir
  gtap_file_prefix <- inputs$model_params$gtap$file_prefix

  if (is.null(gtap_solution_dir) || !dir.exists(gtap_solution_dir)) {
    stop('GTAP solution directory not found. Check model_params.yaml gtap.solution_dir: ', gtap_solution_dir)
  }

  message('  Loading GTAP from solution files...')

  gtap_data <- load_gtap_from_files(
    solution_dir = gtap_solution_dir,
    file_prefix = gtap_file_prefix,
    sector_mapping = inputs$gtap_sector_mapping,
    product_params = inputs$product_params,
    etr_matrix = inputs$etr_matrix
    # Note: overall price effects will be calculated after ETR aggregation
  )

  # Transfer GTAP data to inputs
  inputs$foreign_gdp <- gtap_data$foreign_gdp
  inputs$qmwreg <- gtap_data$qmwreg
  inputs$sector_outputs <- gtap_data$sector_outputs
  inputs$imports_by_country <- gtap_data$imports_by_country
  inputs$viws <- gtap_data$viws
  inputs$vgdp <- gtap_data$vgdp
  inputs$ppm <- gtap_data$ppm
  inputs$ppa <- gtap_data$ppa

  # ETR increase from GTAP mtax (for revenue calculations)
  if (!is.null(gtap_data$etr_increase)) {
    inputs$etr_increase <- gtap_data$etr_increase
    inputs$mtax_data <- gtap_data$mtax_data
  }

  # Product prices will be calculated in 02_calculate_etr.R after ETR aggregation

  # ============================
  # MAUS outputs (scenario-specific, optional)
  # ============================

  maus_outputs_dir <- file.path(scenario_dir, 'maus_outputs')

  maus_file <- file.path(maus_outputs_dir, 'quarterly.csv')
  if (file.exists(maus_file)) {
    inputs$maus <- read_csv(maus_file, show_col_types = FALSE)
    message(sprintf('  Loaded MAUS results: %d rows', nrow(inputs$maus)))
  } else {
    message('  No MAUS results found (optional)')
  }

  # ============================
  # CBO dynamic scoring parameters
  # ============================

  # Load CBO revenue sensitivity (derived from CBO rules of thumb workbook)
  # Formula: sensitivity = cbo_revenue_change / cbo_gdp_change
  cbo_file <- 'resources/cbo_rules/revenue_sensitivity.csv'
  if (file.exists(cbo_file)) {
    inputs$cbo_sensitivity <- read_csv(cbo_file, show_col_types = FALSE)
    message('  Loaded CBO revenue sensitivity parameters')
  } else {
    message('  No CBO revenue sensitivity parameters found (optional)')
  }

  # ============================
  # Distribution parameters
  # ============================

  dist_file <- 'resources/distribution/decile_parameters.csv'
  if (file.exists(dist_file)) {
    inputs$decile_parameters <- read_csv(dist_file, show_col_types = FALSE)
    message('  Loaded distribution parameters (10 deciles)')
  } else {
    message('  No distribution parameters found (optional)')
  }

  return(inputs)
}
