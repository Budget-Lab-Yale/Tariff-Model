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

library(tidyverse)
library(yaml)

# Source helper modules
source('src/00_run_tariff_etrs.R')
source('src/01_load_inputs.R')
# source('src/02_calculate_etr.R')
# source('src/03_calculate_prices.R')
# source('src/04_calculate_revenue.R')
# source('src/05_calculate_macro.R')
# source('src/06_calculate_sectors.R')


#' Run the complete tariff model for a scenario
#'
#' @param scenario Name of the scenario (must exist in config/scenarios/)
#' @param skip_tariff_etrs If TRUE, skip running Tariff-ETRs (use existing outputs)
#'
#' @return List containing all model outputs
run_scenario <- function(scenario, skip_tariff_etrs = FALSE) {

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

  if (!skip_tariff_etrs) {
    message('Step 0: Running Tariff-ETRs...')
    run_tariff_etrs(scenario)
  } else {
    message('Step 0: Skipping Tariff-ETRs (using existing outputs)')
  }

  #---------------------------
  # Step 1: Load all inputs
  #---------------------------

  message('\nStep 1: Loading inputs...')
  inputs <- load_inputs(scenario)

  #---------------------------
  # Step 2: Calculate ETRs
  #---------------------------

  message('\nStep 2: Calculating effective tariff rates...')
  # etr_results <- calculate_etr(inputs)

  #---------------------------
  # Step 3: Calculate price effects
  #---------------------------

  message('\nStep 3: Calculating price effects...')
  # price_results <- calculate_prices(inputs, etr_results)

  #---------------------------
  # Step 4: Calculate revenue
  #---------------------------

  message('\nStep 4: Calculating revenue estimates...')
  # revenue_results <- calculate_revenue(inputs, etr_results)

  #---------------------------
  # Step 5: Calculate macro effects
  #---------------------------

  message('\nStep 5: Calculating macro effects...')
  # macro_results <- calculate_macro(inputs)

  #---------------------------
  # Step 6: Calculate sector effects
  #---------------------------

  message('\nStep 6: Calculating sector effects...')
  # sector_results <- calculate_sectors(inputs)

  #---------------------------
  # Compile results
  #---------------------------

  message('\nCompiling results...')

  results <- list(
    scenario = scenario,
    inputs = inputs
    # etr = etr_results,
    # prices = price_results,
    # revenue = revenue_results,
    # macro = macro_results,
    # sectors = sector_results
  )

  message(sprintf('\nScenario %s complete!\n', scenario))

  invisible(results)
}
