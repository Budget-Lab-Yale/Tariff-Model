# =============================================================================
# 01_load_inputs.R - Load all inputs for the tariff model
# =============================================================================

#' Load all inputs for a scenario
#'
#' @param scenario Name of the scenario
#'
#' @return List containing all input data
load_inputs <- function(scenario) {

  scenario_dir <- file.path('config', 'scenarios', scenario)

  inputs <- list()

  #---------------------------
  # Model parameters
  #---------------------------

  params_file <- file.path(scenario_dir, 'model_params.yaml')
  if (file.exists(params_file)) {
    inputs$params <- read_yaml(params_file)
    message(sprintf('  Loaded model parameters from %s', params_file))
  } else {
    message(sprintf('  No model_params.yaml found, using defaults'))
    inputs$params <- get_default_params()
  }

  #---------------------------
  # Tariff-ETRs outputs
  #---------------------------

  # Tariff-ETRs outputs go to output/{scenario}/tariff_etrs/
  tariff_etrs_dir <- file.path('output', scenario, 'tariff_etrs')

  etr_file <- file.path(tariff_etrs_dir, 'etrs_by_sector_country.csv')
  if (file.exists(etr_file)) {
    inputs$etr_matrix <- read_csv(etr_file, show_col_types = FALSE)
    message(sprintf('  Loaded ETR matrix: %d sectors', nrow(inputs$etr_matrix)))
  } else {
    warning(sprintf('ETR matrix not found: %s', etr_file))
  }

  #---------------------------
  # Other model outputs (GTAP, MAUS)
  #---------------------------

  other_models_dir <- file.path(scenario_dir, 'other_models')

  # GTAP results
  gtap_file <- file.path(other_models_dir, 'gtap_results.csv')
  if (file.exists(gtap_file)) {
    inputs$gtap <- read_csv(gtap_file, show_col_types = FALSE)
    message(sprintf('  Loaded GTAP results: %d rows', nrow(inputs$gtap)))
  } else {
    message('  No GTAP results found (optional for Phase 1)')
  }

  # MAUS results
  maus_file <- file.path(other_models_dir, 'maus_results.csv')
  if (file.exists(maus_file)) {
    inputs$maus <- read_csv(maus_file, show_col_types = FALSE)
    message(sprintf('  Loaded MAUS results: %d rows', nrow(inputs$maus)))
  } else {
    message('  No MAUS results found (optional for Phase 1)')
  }

  #---------------------------
  # CBO baselines
  #---------------------------

  cbo_imports_file <- 'resources/cbo_baselines/imports.csv'
  if (file.exists(cbo_imports_file)) {
    inputs$cbo_imports <- read_csv(cbo_imports_file, show_col_types = FALSE)
    message(sprintf('  Loaded CBO import baselines'))
  }

  cbo_duties_file <- 'resources/cbo_baselines/duties.csv'
  if (file.exists(cbo_duties_file)) {
    inputs$cbo_duties <- read_csv(cbo_duties_file, show_col_types = FALSE)
    message(sprintf('  Loaded CBO duty baselines'))
  }

  #---------------------------
  # Mappings
  #---------------------------

  gtap_sectors_file <- 'resources/mappings/gtap_sectors.csv'
  if (file.exists(gtap_sectors_file)) {
    inputs$gtap_sectors <- read_csv(gtap_sectors_file, show_col_types = FALSE)
    message(sprintf('  Loaded GTAP sector mappings'))
  }

  inputs
}


#' Get default model parameters
#'
#' @return List of default parameters
get_default_params <- function() {
  list(
    # Price effect parameters (from Excel model)
    usd_offset = 0.17,           # = 0.3 * 0.58
    price_passthrough = 0.50,    # Domestic price passthrough
    goods_share_pce = 0.31,      # Goods share of PCE
    import_share = 0.21,         # Import share of goods PCE

    # Revenue parameters
    compliance_effect = 0.10,    # -10% of gross revenue
    income_effect = 0.23,        # -23% of gross revenue

    # Baseline ETR
    baseline_etr = 0.02418       # 2.418% (2024 baseline)
  )
}
