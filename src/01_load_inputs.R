# =============================================================================
# 01_load_inputs.R - Load all inputs for the tariff model
# =============================================================================

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
  # GTAP outputs (scenario-specific)
  # ============================

  gtap_outputs_dir <- file.path(scenario_dir, 'gtap_outputs')

  # GTAP aggregates (postsim country-level data)
  gtap_agg_file <- file.path(gtap_outputs_dir, 'aggregates.csv')
  if (file.exists(gtap_agg_file)) {
    inputs$gtap_postsim <- read_csv(gtap_agg_file, show_col_types = FALSE)
    message('  Loaded GTAP postsim aggregates')
  } else {
    stop('GTAP aggregates not found: ', gtap_agg_file)
  }

  # GTAP qmwreg (percent change in imports)
  qmwreg_file <- file.path(gtap_outputs_dir, 'qmwreg.csv')
  if (file.exists(qmwreg_file)) {
    qmwreg_data <- read_csv(qmwreg_file, show_col_types = FALSE)
    inputs$qmwreg <- qmwreg_data$qmwreg[1]
    message(sprintf('  Loaded GTAP qmwreg: %.2f%%', inputs$qmwreg))
  } else {
    stop('GTAP qmwreg not found: ', qmwreg_file)
  }

  # GTAP etr_increase (tariff rate increase from policy)
  etr_increase_file <- file.path(gtap_outputs_dir, 'etr_increase.csv')
  if (file.exists(etr_increase_file)) {
    etr_data <- read_csv(etr_increase_file, show_col_types = FALSE)
    inputs$etr_increase <- etr_data$etr_increase[1]
    message(sprintf('  Loaded GTAP etr_increase: %.2f%%', inputs$etr_increase * 100))
  } else {
    stop('GTAP etr_increase not found: ', etr_increase_file)
  }

  # GTAP sector outputs (for sector effect calculations)
  sector_outputs_file <- file.path(gtap_outputs_dir, 'sector_outputs.csv')
  if (file.exists(sector_outputs_file)) {
    inputs$sector_outputs <- read_csv(sector_outputs_file, show_col_types = FALSE)
    message(sprintf('  Loaded GTAP sector outputs: %d sectors', nrow(inputs$sector_outputs)))
  } else {
    message('  No sector outputs found (optional)')
  }

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
  # Legacy: other_models directory (for backward compatibility)
  # ============================

  other_models_dir <- file.path(scenario_dir, 'other_models')

  # GTAP sector-level results (optional)
  gtap_sectors_file <- file.path(other_models_dir, 'gtap_results.csv')
  if (file.exists(gtap_sectors_file)) {
    inputs$gtap_sectors <- read_csv(gtap_sectors_file, show_col_types = FALSE)
    message(sprintf('  Loaded GTAP sector results: %d rows', nrow(inputs$gtap_sectors)))
  }

  # Legacy gtap_aggregates.csv (if new structure not yet in place)
  if (is.null(inputs$gtap_postsim)) {
    legacy_agg_file <- file.path(other_models_dir, 'gtap_aggregates.csv')
    if (file.exists(legacy_agg_file)) {
      legacy_data <- read_csv(legacy_agg_file, show_col_types = FALSE)
      inputs$gtap_postsim <- legacy_data %>% filter(scenario == 'postsim')
      message('  Loaded GTAP aggregates (legacy format)')
    }
  }

  # ============================
  # Mappings
  # ============================

  gtap_mapping_file <- 'resources/mappings/gtap_sectors.csv'
  if (file.exists(gtap_mapping_file)) {
    inputs$gtap_sector_mapping <- read_csv(gtap_mapping_file, show_col_types = FALSE)
    message('  Loaded GTAP sector mappings')
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

  return(inputs)
}
