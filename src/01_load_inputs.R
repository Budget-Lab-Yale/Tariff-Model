# =============================================================================
# 01_load_inputs.R - Load all inputs for the tariff model
# =============================================================================

# Source GTAP file reader
source('src/read_gtap.R')

#' Load and merge MAUS scenario data with baseline
#'
#' Reads scenario-specific MAUS output (tariff values) and joins with baseline.
#' MAUS uses column names: GDP, LEB (employment), LURC (unemployment rate)
#' These are mapped to internal names: gdp_tariff, employment_tariff, urate_tariff
#'
#' @param scenario_file Path to scenario MAUS output file
#' @param baseline_data Baseline MAUS data (from load_baselines())
#' @return Tibble with both baseline and tariff columns
load_maus_scenario <- function(scenario_file, baseline_data) {

  if (!file.exists(scenario_file)) {
    stop('MAUS scenario file not found: ', scenario_file)
  }

  # Read scenario data with MAUS column names
  scenario_raw <- read_csv(scenario_file, show_col_types = FALSE)

  # Validate required columns (MAUS native names)
  required_cols <- c('year', 'quarter', 'GDP', 'LEB', 'LURC')
  missing_cols <- setdiff(required_cols, names(scenario_raw))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in MAUS output: ', paste(missing_cols, collapse = ', '),
         '\n  Expected columns: year, quarter, GDP, LEB, LURC')
  }

  # Map MAUS column names to internal names for tariff values
  scenario_data <- scenario_raw %>%
    rename(
      gdp_tariff = GDP,
      employment_tariff = LEB,
      urate_tariff = LURC
    )

  # Join with baseline data
  maus_combined <- baseline_data %>%
    left_join(
      scenario_data %>% select(year, quarter, gdp_tariff, employment_tariff, urate_tariff),
      by = c('year', 'quarter')
    )

  # Validate join worked
  if (any(is.na(maus_combined$gdp_tariff))) {
    missing_quarters <- maus_combined %>%
      filter(is.na(gdp_tariff)) %>%
      mutate(yq = paste0(year, 'Q', quarter)) %>%
      pull(yq)
    stop('Missing tariff data for quarters: ', paste(missing_quarters, collapse = ', '))
  }

  return(maus_combined)
}


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
#' @return List containing model parameters
load_model_params <- function(scenario_dir) {

  params_file <- file.path(scenario_dir, 'model_params.yaml')

  if (!file.exists(params_file)) {
    stop('Model parameters file not found: ', params_file)
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
  } else {
    stop('VIWS baseline file not found: ', viws_baseline_file)
  }

  # Import baseline in dollars (for Excel-compatible ETR weighting)
  import_baseline_file <- 'resources/gtap_baseline/import_baseline_dollars.csv'
  if (file.exists(import_baseline_file)) {
    import_data <- read_csv(import_baseline_file, show_col_types = FALSE)
    # Convert to matrix format
    baselines$import_baseline_dollars <- as.matrix(import_data[, -1])
    rownames(baselines$import_baseline_dollars) <- import_data$gtap_code
    message('  Loaded import baseline (dollars)')
  } else {
    stop('Import baseline (dollars) file not found: ', import_baseline_file)
  }

  # MAUS baseline (GDP, employment, unemployment rate)
  maus_baseline_file <- 'resources/baselines/maus_baseline.csv'
  if (file.exists(maus_baseline_file)) {
    maus_raw <- read_csv(maus_baseline_file, show_col_types = FALSE)
    # Map MAUS column names to internal names
    # MAUS uses: GDP, LEB (employment), LURC (unemployment rate)
    baselines$maus <- maus_raw %>%
      rename(
        gdp_baseline = GDP,
        employment_baseline = LEB,
        urate_baseline = LURC
      )
    message(sprintf('  Loaded MAUS baseline: %d quarters', nrow(baselines$maus)))
  } else {
    stop('MAUS baseline file not found: ', maus_baseline_file)
  }

  return(baselines)
}


#' Load all inputs for a scenario
#'
#' @param scenario Name of the scenario
#' @param skip_maus If TRUE, skip loading MAUS data (will be loaded later)
#'
#' @return List containing all input data
load_inputs <- function(scenario, skip_maus = FALSE) {

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
  if (!file.exists(etr_file)) {
    stop('ETR matrix not found: ', etr_file)
  }
  inputs$etr_matrix <- read_csv(etr_file, show_col_types = FALSE)
  message(sprintf('  Loaded ETR matrix: %d sectors', nrow(inputs$etr_matrix)))

  # ============================
  # Mappings (load early for GTAP processing)
  # ============================

  gtap_mapping_file <- 'resources/mappings/gtap_sectors.csv'
  if (!file.exists(gtap_mapping_file)) {
    stop('GTAP sector mappings not found: ', gtap_mapping_file)
  }
  inputs$gtap_sector_mapping <- read_csv(gtap_mapping_file, show_col_types = FALSE)
  message('  Loaded GTAP sector mappings')

  # Product parameters (coefficients and weights for aggregation)
  product_params_file <- 'resources/products/product_params.csv'
  if (!file.exists(product_params_file)) {
    stop('Product parameters not found: ', product_params_file)
  }
  inputs$product_params <- read_csv(product_params_file, show_col_types = FALSE)
  message('  Loaded product parameters')

  # Import shares (baseline import share of consumption by sector)
  import_shares_file <- 'resources/products/import_shares.csv'
  if (!file.exists(import_shares_file)) {
    stop('Import shares not found: ', import_shares_file)
  }
  inputs$import_shares <- read_csv(import_shares_file, show_col_types = FALSE)
  message('  Loaded import shares')

  # Import weights by country (for product-level weighted ETR calculation)
  import_weights_file <- 'resources/products/import_weights.csv'
  if (!file.exists(import_weights_file)) {
    stop('Import weights not found: ', import_weights_file)
  }
  inputs$import_weights <- read_csv(import_weights_file, show_col_types = FALSE)
  message('  Loaded import weights')

  # ============================
  # GTAP outputs (from solution files)
  # ============================

  # GTAP outputs are in output/{scenario}/gtap/ (generated by run_gtap)
  gtap_solution_dir <- file.path('output', scenario, 'gtap')
  gtap_file_prefix <- scenario

  if (!dir.exists(gtap_solution_dir)) {
    stop('GTAP solution directory not found: ', gtap_solution_dir,
         '\n  Run run_gtap() first or use skip_gtap=FALSE')
  }

  message('  Loading GTAP from: ', gtap_solution_dir)

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
  inputs$qxwreg <- gtap_data$qxwreg
  inputs$sector_outputs <- gtap_data$sector_outputs
  inputs$imports_by_country <- gtap_data$imports_by_country
  inputs$viws <- gtap_data$viws
  inputs$vgdp <- gtap_data$vgdp
  inputs$ppm <- gtap_data$ppm
  inputs$ppd <- gtap_data$ppd
  inputs$ppa <- gtap_data$ppa
  inputs$qgdp <- gtap_data$qgdp

  # ETR increase from GTAP mtax (for revenue calculations)
  if (is.null(gtap_data$etr_increase)) {
    stop('GTAP mtax-based etr_increase not found in GTAP data')
  }
  inputs$etr_increase <- gtap_data$etr_increase
  inputs$mtax_data <- gtap_data$mtax_data

  # NVPP adjustment factors (for post-substitution price calculation)
  if (is.null(gtap_data$nvpp_adjustment)) {
    stop('GTAP nvpp_adjustment not found in GTAP data')
  }
  inputs$nvpp_adjustment <- gtap_data$nvpp_adjustment

  # Product prices will be calculated in 02_calculate_etr.R after ETR aggregation

  # ============================
  # MAUS outputs (scenario-specific)
  # ============================

  if (!skip_maus) {
    maus_outputs_dir <- file.path(scenario_dir, 'maus_outputs')
    maus_file <- file.path(maus_outputs_dir, 'quarterly.csv')
    if (!file.exists(maus_file)) {
      stop('MAUS results not found: ', maus_file)
    }
    inputs$maus <- load_maus_scenario(maus_file, inputs$baselines$maus)
    message(sprintf('  Loaded MAUS results: %d rows', nrow(inputs$maus)))
  } else {
    message('  Skipping MAUS load (will be loaded after shock generation)')
    inputs$maus <- NULL
  }

  # ============================
  # CBO dynamic scoring parameters
  # ============================

  # Load CBO revenue sensitivity (derived from CBO rules of thumb workbook)
  # Formula: sensitivity = cbo_revenue_change / cbo_gdp_change
  cbo_file <- 'resources/cbo_rules/revenue_sensitivity.csv'
  if (!file.exists(cbo_file)) {
    stop('CBO revenue sensitivity parameters not found: ', cbo_file)
  }
  inputs$cbo_sensitivity <- read_csv(cbo_file, show_col_types = FALSE)
  message('  Loaded CBO revenue sensitivity parameters')

  # ============================
  # Distribution parameters
  # ============================

  dist_file <- 'resources/distribution/decile_parameters.csv'
  if (!file.exists(dist_file)) {
    stop('Distribution parameters not found: ', dist_file)
  }
  inputs$decile_parameters <- read_csv(dist_file, show_col_types = FALSE)
  message('  Loaded distribution parameters (10 deciles)')

  return(inputs)
}
