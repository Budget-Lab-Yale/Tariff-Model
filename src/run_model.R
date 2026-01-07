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
source('src/02_calculate_etr.R')
source('src/03_calculate_prices.R')
source('src/04_calculate_revenue.R')
source('src/05_calculate_macro.R')
source('src/06_calculate_sectors.R')
source('src/07_calculate_dynamic_revenue.R')
source('src/08_calculate_foreign_gdp.R')
source('src/09_calculate_distribution.R')
source('src/10_calculate_products.R')
source('src/11_write_outputs.R')


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

  if (!is.null(inputs$etr_matrix) && !is.null(inputs$viws) && !is.null(inputs$ppm)) {
    message('\nStep 3b: Calculating product price effects from GTAP...')

    # Use post-substitution price effect as overall SR effect
    overall_sr_effect <- price_results$post_sub_price_increase / 100

    # Calculate overall LR effect as weighted average of ppm
    ppm_usa <- inputs$ppm[, 'usa']
    viws_total <- rowSums(inputs$viws)
    overall_lr_effect <- sum(ppm_usa * viws_total) / sum(viws_total)

    inputs$product_prices <- get_price_effects(
      gtap_data = list(viws = inputs$viws, ppm = inputs$ppm),
      etr_matrix = inputs$etr_matrix,
      product_params = inputs$product_params,
      overall_sr_effect = overall_sr_effect,
      overall_lr_effect = overall_lr_effect,
      target_region = 'usa'
    )

    message(sprintf('  Calculated price effects for %d products', nrow(inputs$product_prices)))
  }

  #---------------------------
  # Step 4: Calculate revenue
  #---------------------------

  message('\nStep 4: Calculating revenue estimates...')
  revenue_results <- calculate_revenue(inputs, etr_results)

  #---------------------------
  # Step 5: Calculate macro effects
  #---------------------------

  message('\nStep 5: Calculating macro effects...')
  macro_results <- NULL
  if (!is.null(inputs$maus)) {
    macro_results <- calculate_macro(inputs)
  } else {
    message('  Skipping macro calculations (no MAUS data)')
  }

  #---------------------------
  # Step 6: Calculate sector effects
  #---------------------------

  message('\nStep 6: Calculating sector effects...')
  sector_results <- NULL
  if (!is.null(inputs$sector_outputs)) {
    sector_results <- calculate_sectors(inputs)
  } else {
    message('  Skipping sector calculations (no sector_outputs data)')
  }

  #---------------------------
  # Step 7: Calculate dynamic revenue
  #---------------------------

  message('\nStep 7: Calculating dynamic revenue...')
  dynamic_results <- NULL
  if (!is.null(inputs$maus) && !is.null(inputs$cbo_sensitivity)) {
    dynamic_results <- calculate_dynamic_revenue(inputs, revenue_results)
  } else {
    message('  Skipping dynamic revenue (requires MAUS + CBO data)')
  }

  #---------------------------
  # Step 8: Calculate foreign GDP effects
  #---------------------------

  message('\nStep 8: Calculating foreign GDP effects...')
  foreign_gdp_results <- NULL
  if (!is.null(inputs$foreign_gdp)) {
    foreign_gdp_results <- calculate_foreign_gdp(inputs)
  } else {
    message('  Skipping foreign GDP (no foreign_gdp data)')
  }

  #---------------------------
  # Step 9: Calculate distribution
  #---------------------------

  message('\nStep 9: Calculating distribution by income decile...')
  distribution_results <- NULL
  if (!is.null(inputs$decile_parameters)) {
    distribution_results <- calculate_distribution(price_results, inputs)
  } else {
    message('  Skipping distribution (no decile_parameters data)')
  }

  #---------------------------
  # Step 10: Calculate product price effects
  #---------------------------

  message('\nStep 10: Calculating product price effects...')
  product_results <- NULL
  if (!is.null(inputs$product_prices)) {
    product_results <- calculate_products(inputs)
  } else {
    message('  Skipping product prices (no product_prices data)')
  }

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

  # Use distribution results for per-HH cost if available (more accurate)
  if (!is.null(distribution_results)) {
    message(sprintf('Pre-sub per-HH cost:            $%.0f', abs(distribution_results$pre_sub$avg_per_hh_cost)))
    message(sprintf('Post-sub per-HH cost:           $%.0f', abs(distribution_results$post_sub$avg_per_hh_cost)))
  } else {
    message(sprintf('Pre-sub per-HH cost:            $%.0f (approx)', price_results$pre_sub_per_hh_cost))
    message(sprintf('Post-sub per-HH cost:           $%.0f (approx)', price_results$post_sub_per_hh_cost))
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
    dist <- distribution_results$pre_sub$by_decile
    for (i in 1:nrow(dist)) {
      message(sprintf('Decile %2d ($%s income):  $%s (%.2f%% of income)',
                      dist$decile[i],
                      format(dist$income[i], big.mark = ',', scientific = FALSE),
                      format(round(abs(dist$cost_per_hh[i])), big.mark = ',', scientific = FALSE),
                      abs(dist$pct_of_income[i])))
    }
    message(sprintf('Average per-HH cost:            $%s',
                    format(round(abs(distribution_results$pre_sub$avg_per_hh_cost)), big.mark = ',', scientific = FALSE)))
    message(sprintf('Regressivity ratio (D1/D10):    %.2fx',
                    distribution_results$regressivity$burden_ratio))
  }
  message('----------------------------------------------------------')

  message(sprintf('\nScenario %s complete!\n', scenario))

  invisible(results)
}
