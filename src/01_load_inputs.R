# =============================================================================
# 01_load_inputs.R - Load all inputs for the tariff model
# =============================================================================

# Source GTAP file reader
source('src/read_gtap.R')

# Source I-O price model (Boston Fed)
source('src/io_price_model.R')


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
  if (!file.exists(cbo_file)) {
    stop('CBO baseline file not found: ', cbo_file)
  }
  baselines$cbo <- read_csv(cbo_file, show_col_types = FALSE)
  assert_has_columns(baselines$cbo, c('fiscal_year', 'imports_bn', 'duties_bn', 'baseline_etr'),
                     'CBO baselines')
  message(sprintf('  Loaded CBO baselines: FY%d-FY%d',
                  min(baselines$cbo$fiscal_year),
                  max(baselines$cbo$fiscal_year)))

  # GTAP baseline (no policy change)
  gtap_file <- 'resources/baselines/gtap.csv'
  if (!file.exists(gtap_file)) {
    stop('GTAP baseline file not found: ', gtap_file)
  }
  baselines$gtap <- read_csv(gtap_file, show_col_types = FALSE)
  message('  Loaded GTAP baseline')

  # VIWS baseline (baseline imports matrix for pre-substitution calculations)
  viws_baseline_file <- 'resources/gtap_baseline/viws_baseline.csv'
  if (!file.exists(viws_baseline_file)) {
    stop('VIWS baseline file not found: ', viws_baseline_file)
  }
  viws_data <- read_csv(viws_baseline_file, show_col_types = FALSE)
  assert_has_columns(viws_data, 'commodity', 'VIWS baseline')
  # Convert to matrix format matching GTAP output
  baselines$viws_baseline <- as.matrix(viws_data[, -1])  # Remove commodity column
  rownames(baselines$viws_baseline) <- viws_data$commodity
  message('  Loaded VIWS baseline')

  # Import baseline in dollars (for Excel-compatible ETR weighting)
  import_baseline_file <- 'resources/gtap_baseline/import_baseline_dollars.csv'
  if (!file.exists(import_baseline_file)) {
    stop('Import baseline (dollars) file not found: ', import_baseline_file)
  }
  import_data <- read_csv(import_baseline_file, show_col_types = FALSE)
  assert_has_columns(import_data, 'gtap_code', 'import baseline dollars')
  # Convert to matrix format
  baselines$import_baseline_dollars <- as.matrix(import_data[, -1])
  rownames(baselines$import_baseline_dollars) <- import_data$gtap_code
  message('  Loaded import baseline (dollars)')

  return(baselines)
}


#' Load all inputs for a scenario
#'
#' @param scenario Name of the scenario
#' @param bea_io_level_override Override for BEA I-O table level
#' @param markup_assumption 'constant_percentage' or 'constant_dollar'; affects
#'   B-matrix normalization per Boston Fed appendix
#'
#' @return List containing all input data
load_inputs <- function(scenario, bea_io_level_override = NULL,
                        markup_assumption = 'constant_percentage') {

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
  # Boston Fed I-O data (always loaded)
  # ============================

  bea_io_level <- bea_io_level_override %||% inputs$assumptions$bea_io_level %||% 'summary'
  inputs$bea_io_level <- bea_io_level
  io_data_dir <- resolve_io_data_dir(bea_io_level)
  inputs$io_data_dir <- io_data_dir
  message(sprintf('  BEA I-O level: %s (%s)', bea_io_level, io_data_dir))

  inputs$bea_use_import <- load_bea_use_data('import', io_data_dir)
  inputs$bea_use_domestic <- load_bea_use_data('domestic', io_data_dir)
  inputs$bea_pce_weights <- load_bea_pce_weights(io_data_dir)
  inputs$bea_industry_output <- load_bea_industry_output(io_data_dir)
  inputs$bea_leontief_domestic <- load_bea_requirements('domestic', io_data_dir)

  # Full commodity use totals (intermediate + final demand) for omega_M
  inputs$bea_commodity_use_totals <- load_bea_commodity_use_totals(io_data_dir)
  # Industry variable cost (for constant-percentage B normalization)
  inputs$bea_industry_variable_cost <- load_bea_industry_variable_cost(io_data_dir)

  # Pre-compute Boston Fed matrices (B_MD, omega_M, omega_D)
  inputs$boston_fed_matrices <- build_boston_fed_matrices(
    use_import = inputs$bea_use_import,
    use_domestic = inputs$bea_use_domestic,
    industry_output = inputs$bea_industry_output,
    markup_assumption = markup_assumption,
    commodity_use_totals = inputs$bea_commodity_use_totals,
    industry_variable_cost = inputs$bea_industry_variable_cost
  )
  message(sprintf('  Built Boston Fed matrices: %d commodities x %d industries',
                  nrow(inputs$boston_fed_matrices$B_MD),
                  ncol(inputs$boston_fed_matrices$B_MD)))

  # GTAP-BEA crosswalk (for post-substitution adjustments)
  inputs$gtap_bea_crosswalk <- load_gtap_bea_crosswalk(io_data_dir)

  # PCE bridge for consumer category reporting
  inputs$pce_bridge <- load_pce_bridge(io_data_dir)

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

  # ---- Deltas (tariff increases from baseline) ----
  etr_file <- file.path(tariff_etrs_dir, 'gtap_deltas_by_sector_country.csv')
  if (!file.exists(etr_file)) {
    stop('ETR delta matrix not found: ', etr_file)
  }
  etr_raw <- read_csv(etr_file, show_col_types = FALSE)
  assert_has_columns(etr_raw, 'gtap_code', 'ETR delta matrix')

  # Detect time-varying ETRs (stacked CSV with date column)
  if ('date' %in% names(etr_raw)) {
    inputs$is_time_varying <- TRUE
    inputs$etr_matrix_by_date <- etr_raw %>%
      mutate(date = as.Date(date))
    inputs$etr_dates <- sort(unique(inputs$etr_matrix_by_date$date))

    # Reference date for GTAP (from model_params, default: first date)
    gtap_ref <- inputs$model_params$gtap_reference_date
    if (!is.null(gtap_ref)) {
      inputs$gtap_reference_date <- as.Date(gtap_ref)
    } else {
      inputs$gtap_reference_date <- inputs$etr_dates[1]
    }

    # Extract reference date rows as flat etr_matrix (no date column)
    inputs$etr_matrix <- inputs$etr_matrix_by_date %>%
      filter(date == inputs$gtap_reference_date) %>%
      select(-date)

    message(sprintf('  Loaded time-varying ETR deltas: %d dates, %d sectors each',
                    length(inputs$etr_dates), nrow(inputs$etr_matrix)))
    message(sprintf('  GTAP reference date: %s', inputs$gtap_reference_date))
  } else {
    inputs$is_time_varying <- FALSE
    inputs$etr_matrix <- etr_raw
    message(sprintf('  Loaded ETR delta matrix: %d sectors', nrow(inputs$etr_matrix)))
  }

  # ---- Scenario levels (absolute tariff rates) ----
  levels_file <- file.path(tariff_etrs_dir, 'gtap_levels_by_sector_country.csv')
  if (!file.exists(levels_file)) {
    stop('ETR levels matrix not found: ', levels_file)
  }
  levels_raw <- read_csv(levels_file, show_col_types = FALSE)
  assert_has_columns(levels_raw, 'gtap_code', 'ETR levels matrix')

  if ('date' %in% names(levels_raw)) {
    inputs$levels_matrix_by_date <- levels_raw %>%
      mutate(date = as.Date(date))
    inputs$levels_matrix <- inputs$levels_matrix_by_date %>%
      filter(date == inputs$gtap_reference_date) %>%
      select(-date)
    message(sprintf('  Loaded time-varying ETR levels: %d dates', length(inputs$etr_dates)))
  } else {
    inputs$levels_matrix <- levels_raw
    inputs$levels_matrix_by_date <- NULL
    message(sprintf('  Loaded ETR levels matrix: %d sectors', nrow(inputs$levels_matrix)))
  }

  # ---- Baseline levels (pre-existing tariff rates, always static) ----
  baseline_levels_file <- file.path('output', scenario, 'baseline',
                                     'gtap_levels_by_sector_country.csv')
  if (!file.exists(baseline_levels_file)) {
    stop('Baseline levels matrix not found: ', baseline_levels_file)
  }
  baseline_levels_raw <- read_csv(baseline_levels_file, show_col_types = FALSE)
  assert_has_columns(baseline_levels_raw, 'gtap_code', 'baseline levels matrix')
  if ('date' %in% names(baseline_levels_raw)) {
    stop('Baseline levels must be static (no date column), found date column in: ',
         baseline_levels_file)
  }
  inputs$baseline_levels_matrix <- baseline_levels_raw
  message(sprintf('  Loaded baseline levels matrix: %d sectors',
                  nrow(inputs$baseline_levels_matrix)))

  # ---- Census-country files (optional passthrough data) ----
  census_deltas_file <- file.path(tariff_etrs_dir, 'deltas_by_census_country.csv')
  if (file.exists(census_deltas_file)) {
    inputs$census_country_deltas <- read_csv(census_deltas_file, show_col_types = FALSE)
    message(sprintf('  Loaded census-country deltas: %d rows',
                    nrow(inputs$census_country_deltas)))
  } else {
    inputs$census_country_deltas <- NULL
  }

  census_levels_file <- file.path(tariff_etrs_dir, 'levels_by_census_country.csv')
  if (file.exists(census_levels_file)) {
    inputs$census_country_levels <- read_csv(census_levels_file, show_col_types = FALSE)
    message(sprintf('  Loaded census-country levels: %d rows',
                    nrow(inputs$census_country_levels)))
  } else {
    inputs$census_country_levels <- NULL
  }

  baseline_census_file <- file.path('output', scenario, 'baseline',
                                     'levels_by_census_country.csv')
  if (file.exists(baseline_census_file)) {
    inputs$baseline_census_country_levels <- read_csv(baseline_census_file,
                                                       show_col_types = FALSE)
    message(sprintf('  Loaded baseline census-country levels: %d rows',
                    nrow(inputs$baseline_census_country_levels)))
  } else {
    inputs$baseline_census_country_levels <- NULL
  }

  # ---- BEA-level ETR deltas (for Boston Fed I-O price model) ----
  bea_deltas_file <- file.path(tariff_etrs_dir, 'bea_deltas_by_commodity.csv')
  if (!file.exists(bea_deltas_file)) {
    stop('BEA deltas file not found: ', bea_deltas_file,
         '\n  Re-run Tariff-ETRs to generate BEA-level output')
  }
  bea_deltas_raw <- read_csv(bea_deltas_file, show_col_types = FALSE)

  if ('date' %in% names(bea_deltas_raw)) {
    inputs$bea_deltas_by_date <- bea_deltas_raw %>%
      mutate(date = as.Date(date))
    inputs$bea_deltas <- bea_deltas_raw %>%
      filter(date == as.character(inputs$gtap_reference_date)) %>%
      select(-date)
    stopifnot(nrow(inputs$bea_deltas) > 0)
    message(sprintf('  Loaded time-varying BEA deltas: %d dates',
                    length(unique(inputs$bea_deltas_by_date$date))))
  } else {
    inputs$bea_deltas <- bea_deltas_raw
    inputs$bea_deltas_by_date <- NULL
    message(sprintf('  Loaded BEA deltas: %d commodities', nrow(inputs$bea_deltas)))
  }

  # Build tau_M vector (named by bea_code)
  inputs$tau_M <- setNames(inputs$bea_deltas$etr_delta, inputs$bea_deltas$bea_code)
  message(sprintf('  tau_M vector: %d BEA commodities, mean=%.4f%%',
                  length(inputs$tau_M), mean(inputs$tau_M) * 100))

  # Disaggregate tau_M to detail BEA codes if using detail-level tables
  if (bea_io_level == 'detail') {
    inputs$tau_M <- disaggregate_tau_M(inputs$tau_M, io_data_dir)
    # Cache the mapping for reaggregation in run_model.R
    inputs$summary_to_detail <- read_csv(
      file.path(io_data_dir, 'bea_summary_to_detail.csv'),
      show_col_types = FALSE
    )
  }

  # ============================
  # Mappings
  # ============================

  gtap_mapping_file <- 'resources/mappings/gtap_sectors.csv'
  if (!file.exists(gtap_mapping_file)) {
    stop('GTAP sector mappings not found: ', gtap_mapping_file)
  }
  inputs$gtap_sector_mapping <- read_csv(gtap_mapping_file, show_col_types = FALSE)
  assert_has_columns(inputs$gtap_sector_mapping,
                     c('gtap_code', 'aggregate_sector', 'is_manufacturing', 'is_durable',
                       'is_nondurable', 'is_advanced'),
                     'GTAP sector mappings')
  message('  Loaded GTAP sector mappings')

  # ============================
  # GTAP outputs (from solution files)
  # ============================

  gtap_solution_dir <- file.path('output', scenario, 'gtap')
  gtap_file_prefix <- scenario

  if (!dir.exists(gtap_solution_dir)) {
    stop('GTAP solution directory not found: ', gtap_solution_dir,
         '\n  Run run_gtap() first')
  }

  message('  Loading GTAP from: ', gtap_solution_dir)

  gtap_data <- load_gtap_from_files(
    solution_dir = gtap_solution_dir,
    file_prefix = gtap_file_prefix,
    sector_mapping = inputs$gtap_sector_mapping
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
  inputs$ppriv <- gtap_data$ppriv
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

  # Per-commodity NVPP import share ratios (for post-sub omega_M)
  inputs$nvpp_commodity_ratio <- gtap_data$nvpp_commodity_ratio

  # ============================
  # USMM IRFs (impulse response functions)
  # ============================

  inputs$usmm_irfs <- load_usmm_irfs()
  message('  Loaded USMM IRFs and baseline')

  # ============================
  # CBO dynamic scoring parameters
  # ============================

  cbo_file <- 'resources/cbo_rules/revenue_sensitivity.csv'
  if (!file.exists(cbo_file)) {
    stop('CBO revenue sensitivity parameters not found: ', cbo_file)
  }
  inputs$cbo_sensitivity <- read_csv(cbo_file, show_col_types = FALSE)
  assert_has_columns(inputs$cbo_sensitivity, 'fiscal_year', 'CBO sensitivity')
  message('  Loaded CBO revenue sensitivity parameters')

  # ============================
  # Distribution parameters
  # ============================

  dist_file <- 'resources/distribution/decile_parameters.csv'
  if (!file.exists(dist_file)) {
    stop('Distribution parameters not found: ', dist_file)
  }
  inputs$decile_parameters <- read_csv(dist_file, show_col_types = FALSE)
  assert_has_columns(inputs$decile_parameters,
                     c('decile', 'income', 'scaling_factor', 'pce_variation'),
                     'distribution parameters')
  message('  Loaded distribution parameters (10 deciles)')

  # BLS distributional PCE shares by decile (optional)
  dist_pce_file <- 'resources/distribution/distributional_pce_2023.csv'
  if (file.exists(dist_pce_file)) {
    inputs$distributional_pce <- read_csv(dist_pce_file, show_col_types = FALSE)
    assert_has_columns(inputs$distributional_pce,
                       c('pce_mp', 'total_billions', 'd1', 'd10'),
                       'distributional PCE')
    message(sprintf('  Loaded BLS distributional PCE: %d categories',
                    nrow(inputs$distributional_pce)))
  } else {
    inputs$distributional_pce <- NULL
    message('  BLS distributional PCE not found (will use aggregate pce_variation fallback)')
  }

  # NIPA-to-distributional-bucket mapping (optional, required if distributional_pce loaded)
  bucket_file <- 'resources/distribution/nipa_pce_to_distributional_bucket.csv'
  if (file.exists(bucket_file)) {
    inputs$nipa_to_bucket <- read_csv(bucket_file, show_col_types = FALSE)
    assert_has_columns(inputs$nipa_to_bucket, c('nipa_line', 'pce_mp'), 'NIPA-to-bucket mapping')
    message(sprintf('  Loaded NIPA-to-bucket mapping: %d categories -> %d buckets',
                    nrow(inputs$nipa_to_bucket),
                    length(unique(inputs$nipa_to_bucket$pce_mp))))
  } else {
    inputs$nipa_to_bucket <- NULL
  }

  return(inputs)
}
