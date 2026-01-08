# =============================================================================
# run_model.R - Main orchestrator for the Yale Budget Lab Tariff Model
# =============================================================================
#
# This script runs the complete tariff impact analysis pipeline for a scenario.
#
# Usage:
#   source('src/run_model.R')
#   run_scenario('my_scenario')
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
})

# Source helper modules
source('src/00_run_tariff_etrs.R')
source('src/00b_run_gtap.R')
source('src/01_load_inputs.R')
source('src/02_calculate_etr.R')
source('src/03_calculate_prices.R')
source('src/04a_generate_maus_inputs.R')
source('src/04_calculate_revenue.R')
source('src/05_calculate_macro.R')
source('src/06_calculate_sectors.R')
source('src/07_calculate_dynamic_revenue.R')
source('src/08_calculate_foreign_gdp.R')
source('src/09_calculate_distribution.R')
source('src/10_calculate_products.R')
source('src/11_write_outputs.R')


#' Load MAUS output levels after user runs MAUS
#' @param scenario_dir Path to scenario directory
#' @param maus_baseline Baseline MAUS data (from inputs$baselines$maus)
#' @return Tibble with MAUS quarterly data (baseline + tariff)
load_maus_levels <- function(scenario_dir, maus_baseline) {
  maus_file <- file.path(scenario_dir, 'maus_outputs', 'quarterly.csv')
  if (!file.exists(maus_file)) {
    stop('MAUS output file not found: ', maus_file)
  }
  # Use shared loader that merges scenario data with baseline
  maus <- load_maus_scenario(maus_file, maus_baseline)
  message(sprintf('  Loaded MAUS output: %d quarters', nrow(maus)))
  return(maus)
}


#' Pause and wait for user to run MAUS
#' @param maus_inputs Results from generate_maus_inputs()
#' @param scenario_dir Path to scenario directory
wait_for_maus <- function(maus_inputs, scenario_dir) {
  maus_output_file <- file.path(scenario_dir, 'maus_outputs', 'quarterly.csv')

  # If MAUS output already exists, continue without pausing
  if (file.exists(maus_output_file)) {
    message('  MAUS output file found, continuing...')
    return()
  }

  # Get date range from shock data
  shocks <- maus_inputs$shocks
  start_year <- min(shocks$year)
  start_qtr <- shocks$quarter[shocks$year == start_year][1]
  end_year <- max(shocks$year)
  end_qtr <- shocks$quarter[shocks$year == end_year]
  end_qtr <- end_qtr[length(end_qtr)]

  # Build example rows from first few quarters
  example_rows <- shocks %>%
    head(4) %>%
    mutate(row = sprintf('  %d,%d,<GDP>,<LEB>,<LURC>', year, quarter)) %>%
    pull(row) %>%
    paste(collapse = '\n')

  msg <- sprintf('
===========================================================
MAUS INPUT REQUIRED
===========================================================

The model has generated MAUS input shocks.

INPUT FILE (shocks for MAUS):
  %s

NEXT STEPS:
  1. Open MAUS and load the shock series from the file above
  2. Run MAUS to generate quarterly GDP/employment projections
  3. Save MAUS output to:
     %s

REQUIRED OUTPUT FORMAT (CSV with columns):
  year, quarter, GDP, LEB, LURC

  Column mapping:
    GDP  = Real GDP (billions)
    LEB  = Employment (millions)
    LURC = Unemployment rate (%%)

  Example (first rows should look like):
  year,quarter,GDP,LEB,LURC
%s
  ...
  (%d quarters total, through %d Q%d)

  NOTE: Only include tariff scenario values.
        Baseline values are loaded from resources/baselines/maus_baseline.csv

===========================================================

', maus_inputs$output_file, maus_output_file, example_rows,
   maus_inputs$n_quarters, end_year, end_qtr)

  cat(msg)

  # Wait for user to run MAUS and save output
  cat('Press [Enter] when MAUS output file is ready...')
  readLines(con = 'stdin', n = 1)

  # Verify the file now exists
  if (!file.exists(maus_output_file)) {
    stop('MAUS output file still not found: ', maus_output_file)
  }

  message('  MAUS output file found, continuing...')
}


#' Run the complete tariff model for a scenario
#'
#' @param scenario Name of the scenario (must exist in config/scenarios/)
#'
#' @return List containing all model outputs
run_scenario <- function(scenario) {

  message(sprintf('\n=========================================================='))
  message(sprintf('Running Tariff Model: %s', scenario))
  message(sprintf('==========================================================\n'))

  scenario_dir <- file.path('config', 'scenarios', scenario)

  if (!dir.exists(scenario_dir)) {
    stop(sprintf('Scenario directory not found: %s', scenario_dir))
  }

  #---------------------------
  # Step 0: Run Tariff-ETRs
  #---------------------------

  message('Step 0: Running Tariff-ETRs...')
  run_tariff_etrs(scenario)

  #---------------------------
  # Step 0b: Run GTAP
  #---------------------------

  message('\nStep 0b: Running GTAP...')
  run_gtap(scenario)

  #---------------------------
  # Step 1: Load all inputs
  #---------------------------

  message('\nStep 1: Loading inputs...')
  inputs <- load_inputs(scenario, skip_maus = TRUE)

  #---------------------------
  # Step 2: Calculate ETRs
  #---------------------------

  message('\nStep 2: Calculating effective tariff rates...')
  etr_results <- calculate_etr(inputs)

  # Update inputs with calculated values from ETR step
  if (!is.null(etr_results$etr_increase)) {
    inputs$etr_increase <- etr_results$etr_increase
  }
  if (!is.null(etr_results$gtap_postsim)) {
    inputs$gtap_postsim <- etr_results$gtap_postsim
  }

  #---------------------------
  # Step 3: Calculate price effects
  #---------------------------

  message('\nStep 3: Calculating price effects...')
  price_results <- calculate_prices(etr_results, inputs)

  #---------------------------
  # Step 3b: Calculate product price effects (if GTAP data available)
  #---------------------------

  if (is.null(inputs$etr_matrix) || is.null(inputs$ppa) || is.null(inputs$import_weights)) {
    stop('Product price effects require etr_matrix, ppa, and import_weights inputs')
  }

  message('\nStep 3b: Calculating product price effects from GTAP...')

  # SR = import_share * weighted_ETR, normalized by consumption weights
  # LR = ppa (purchaser's price), normalized by consumption weights
  inputs$product_prices <- get_price_effects(
    gtap_data = list(ppa = inputs$ppa),
    etr_matrix = inputs$etr_matrix,
    product_params = inputs$product_params,
    import_shares = inputs$import_shares,
    import_weights = inputs$import_weights,
    overall_sr_effect = price_results$pre_sub_price_increase,
    overall_lr_effect = price_results$post_sub_price_increase,
    target_region = 'usa'
  )

  message(sprintf('  Calculated price effects for %d products', nrow(inputs$product_prices)))

  #---------------------------
  #---------------------------
  # Step 4a: Generate MAUS inputs and pause
  #---------------------------

  message('
Step 4a: Generating MAUS input shocks...')
  maus_inputs <- generate_maus_inputs(etr_results, inputs, scenario)
  print_shock_summary(maus_inputs)

  wait_for_maus(maus_inputs, scenario_dir)

  # Load MAUS output levels
  message('
Loading MAUS output levels...')
  inputs$maus <- load_maus_levels(scenario_dir, inputs$baselines$maus)

  # Step 4: Calculate revenue
  #---------------------------

  message('\nStep 4: Calculating revenue estimates...')
  revenue_results <- calculate_revenue(inputs, etr_results)

  #---------------------------
  # Step 5: Calculate macro effects
  #---------------------------

  message('\nStep 5: Calculating macro effects...')
  if (is.null(inputs$maus)) {
    stop('MAUS data is required for macro calculations')
  }
  macro_results <- calculate_macro(inputs)

  #---------------------------
  # Step 6: Calculate sector effects
  #---------------------------

  message('\nStep 6: Calculating sector effects...')
  if (is.null(inputs$sector_outputs)) {
    stop('sector_outputs data is required for sector calculations')
  }
  sector_results <- calculate_sectors(inputs)

  #---------------------------
  # Step 7: Calculate dynamic revenue
  #---------------------------

  message('\nStep 7: Calculating dynamic revenue...')
  if (is.null(inputs$cbo_sensitivity)) {
    stop('CBO sensitivity data is required for dynamic revenue')
  }
  dynamic_results <- calculate_dynamic_revenue(inputs, revenue_results)

  #---------------------------
  # Step 8: Calculate foreign GDP effects
  #---------------------------

  message('\nStep 8: Calculating foreign GDP effects...')
  if (is.null(inputs$foreign_gdp)) {
    stop('foreign_gdp data is required for foreign GDP calculations')
  }
  foreign_gdp_results <- calculate_foreign_gdp(inputs)

  #---------------------------
  # Step 9: Calculate distribution
  #---------------------------

  message('\nStep 9: Calculating distribution by income decile...')
  if (is.null(inputs$decile_parameters)) {
    stop('decile_parameters data is required for distribution calculations')
  }
  distribution_results <- calculate_distribution(price_results, inputs)

  #---------------------------
  # Step 10: Calculate product price effects
  #---------------------------

  message('\nStep 10: Calculating product price effects...')
  if (is.null(inputs$product_prices)) {
    stop('product_prices data is required for product calculations')
  }
  product_results <- calculate_products(inputs)

  #---------------------------
  # Compile results
  #---------------------------

  message('\nCompiling results...')

  results <- list(
    scenario = scenario,
    inputs = inputs,
    etr = etr_results,
    prices = price_results,
    revenue = revenue_results,
    macro = macro_results,
    sectors = sector_results,
    dynamic = dynamic_results,
    foreign_gdp = foreign_gdp_results,
    distribution = distribution_results,
    products = product_results
  )

  #---------------------------
  # Step 11: Write outputs to disk
  #---------------------------

  message('\nStep 11: Writing outputs to disk...')
  write_outputs(results, scenario)

  # Print key results summary
  message('\n----------------------------------------------------------')
  message('KEY RESULTS')
  message('----------------------------------------------------------')
  message(sprintf('Pre-substitution ETR increase:  %.2f%%', etr_results$pre_sub_increase))
  message(sprintf('Post-substitution ETR increase: %.2f%%', etr_results$post_sub_increase))
  message(sprintf('Pre-sub price increase:         %.3f%%', price_results$pre_sub_price_increase))
  message(sprintf('Post-sub price increase:        %.3f%%', price_results$post_sub_price_increase))

  # Per-HH costs come from distribution calculation (matches Excel methodology)
  if (!is.null(distribution_results)) {
    message(sprintf('Pre-sub per-HH cost:            $%.0f', abs(distribution_results$pre_sub_per_hh_cost)))
    message(sprintf('Post-sub per-HH cost:           $%.0f', abs(distribution_results$post_sub_per_hh_cost)))
  }
  message(sprintf('10-yr conventional revenue:     $%.0fB', revenue_results$conventional_10yr))
  if (!is.null(dynamic_results)) {
    message(sprintf('10-yr dynamic effect:           $%.0fB', dynamic_results$dynamic_effect_10yr))
    message(sprintf('10-yr dynamic revenue:          $%.0fB', dynamic_results$dynamic_10yr))
  }

  if (!is.null(macro_results)) {
    message('----------------------------------------------------------')
    message('MACROECONOMIC EFFECTS')
    message('----------------------------------------------------------')
    message(sprintf('GDP 2025 Q4-Q4:                 %.2f%%', macro_results$gdp_2025))
    message(sprintf('GDP 2026 Q4-Q4:                 %.2f%%', macro_results$gdp_2026))
    message(sprintf('U-rate 2025 Q4:                 %+.2f pp', macro_results$urate_2025))
    message(sprintf('U-rate 2026 Q4:                 %+.2f pp', macro_results$urate_2026))
    message(sprintf('Payroll 2025 Q4:                %.0f thousand', macro_results$payroll_2025))
    message(sprintf('Payroll 2026 Q4:                %.0f thousand', macro_results$payroll_2026))
  }

  if (!is.null(sector_results)) {
    message('----------------------------------------------------------')
    message('SECTOR OUTPUT EFFECTS')
    message('----------------------------------------------------------')
    message(sprintf('Agriculture:                    %.2f%%', sector_results$agriculture))
    message(sprintf('Mining & Extraction:            %.2f%%', sector_results$mining))
    message(sprintf('Total Manufacturing:            %.2f%%', sector_results$manufacturing))
    message(sprintf('  Durable:                      %.2f%%', sector_results$durable))
    message(sprintf('  Advanced:                     %.2f%%', sector_results$advanced))
    message(sprintf('  Nondurable:                   %.2f%%', sector_results$nondurable))
    message(sprintf('Utilities:                      %.2f%%', sector_results$utilities))
    message(sprintf('Construction:                   %.2f%%', sector_results$construction))
    message(sprintf('Services:                       %.2f%%', sector_results$services))
  }

  if (!is.null(foreign_gdp_results)) {
    message('----------------------------------------------------------')
    message('FOREIGN GDP EFFECTS (Long-Run)')
    message('----------------------------------------------------------')
    message(sprintf('USA:                            %+.2f%%', foreign_gdp_results$usa))
    message(sprintf('China:                          %+.2f%%', foreign_gdp_results$china))
    message(sprintf('Canada:                         %+.2f%%', foreign_gdp_results$canada))
    message(sprintf('Mexico:                         %+.2f%%', foreign_gdp_results$mexico))
    message(sprintf('EU:                             %+.2f%%', foreign_gdp_results$eu))
    message(sprintf('UK:                             %+.2f%%', foreign_gdp_results$uk))
    message(sprintf('Japan:                          %+.2f%%', foreign_gdp_results$japan))
    message(sprintf('ROW:                            %+.2f%%', foreign_gdp_results$row))
  }

  if (!is.null(product_results)) {
    message('----------------------------------------------------------')
    message('PRODUCT PRICE EFFECTS')
    message('----------------------------------------------------------')
    message(sprintf('Food (Short-Run):               %.4f%%', product_results$food_sr))
    message(sprintf('Food (Long-Run):                %.4f%%', product_results$food_lr))
    message(sprintf('Total products:                 %d', product_results$n_products))
    message(sprintf('Food products:                  %d', product_results$n_food))
  }

  if (!is.null(distribution_results)) {
    message('----------------------------------------------------------')
    message('DISTRIBUTION BY INCOME DECILE (Pre-Substitution)')
    message('----------------------------------------------------------')
    dist <- if (!is.null(distribution_results$pre_sub)) {
      distribution_results$pre_sub$by_decile
    } else {
      distribution_results$by_decile
    }
    for (i in 1:nrow(dist)) {
      message(sprintf('Decile %2d ($%s income):  $%s (%.2f%% of income)',
                      dist$decile[i],
                      format(dist$income[i], big.mark = ',', scientific = FALSE),
                      format(round(abs(dist$cost_per_hh[i])), big.mark = ',', scientific = FALSE),
                      abs(dist$pct_of_income[i])))
    }
    avg_cost <- if (!is.null(distribution_results$pre_sub)) {
      distribution_results$pre_sub$avg_per_hh_cost
    } else {
      distribution_results$avg_per_hh_cost
    }
    message(sprintf('Average per-HH cost:            $%s',
                    format(round(abs(avg_cost)), big.mark = ',', scientific = FALSE)))
  }
  message('----------------------------------------------------------')

  message(sprintf('\nScenario %s complete!\n', scenario))

  invisible(results)
}
