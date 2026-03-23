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
source('src/helpers.R')
source('src/00_run_tariff_etrs.R')
source('src/00b_run_gtap.R')
source('src/01_load_inputs.R')
source('src/02_calculate_etr.R')
source('src/04_calculate_revenue.R')
source('src/05a_usmm_surrogate.R')
source('src/05_calculate_macro.R')
source('src/06_calculate_sectors.R')
source('src/07_calculate_dynamic_revenue.R')
source('src/08_calculate_foreign_gdp.R')
source('src/09_calculate_distribution.R')
source('src/11_write_outputs.R')
source('src/12_export_excel.R')


#' Run the complete tariff model for a scenario
#'
#' @param scenario Name of the scenario (must exist in config/scenarios/)
#' @param markup_assumption Markup response to cost changes:
#'   'constant_percentage' (default, upper bound) or 'constant_dollar' (lower bound)
#' @param bea_io_level BEA I-O table level: 'summary' (73 commodities, 2024) or
#'   'detail' (~400 commodities, 2017). NULL uses value from global_assumptions.yaml.
#'
#' @return List containing all model outputs
run_scenario <- function(scenario, markup_assumption = 'constant_percentage',
                         bea_io_level = NULL) {

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
  inputs <- load_inputs(scenario, bea_io_level_override = bea_io_level,
                        markup_assumption = markup_assumption)

  if (isTRUE(inputs$is_time_varying)) {
    message(sprintf('  Detected time-varying ETRs (%d dates)', length(inputs$etr_dates)))
  }

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
  # Step 3: Calculate consumer price effects (Boston Fed)
  #---------------------------

  message('\nStep 3: Calculating consumer price effects...')

  # Pre-substitution
  message('  Pre-substitution (Boston Fed):')
  presub_results <- compute_boston_fed_prices(
    tau_M = inputs$tau_M,
    B_MD = inputs$boston_fed_matrices$B_MD,
    leontief_domestic = inputs$bea_leontief_domestic,
    omega_M = inputs$boston_fed_matrices$omega_M,
    omega_D = inputs$boston_fed_matrices$omega_D,
    pce_bridge = inputs$pce_bridge,
    usd_offset = inputs$assumptions$usd_offset,
    markup_assumption = markup_assumption
  )

  # PE post-substitution (adjust tau_M and omega_M using GTAP)
  message('  PE post-substitution (GTAP-adjusted):')
  tau_M_post <- compute_postsub_tau_M(
    inputs$tau_M, inputs$gtap_bea_crosswalk,
    inputs$etr_matrix, inputs$viws, inputs$baselines$viws_baseline
  )
  omega_M_post <- compute_postsub_omega_M(
    inputs$boston_fed_matrices$omega_M, inputs$gtap_bea_crosswalk,
    inputs$nvpp_commodity_ratio, inputs$baselines$viws_baseline
  )
  # omega_M + omega_D = 1 by BEA use table construction (import + domestic = total)
  # Preserve zero-use commodities: pre-sub has omega_M=0, omega_D=0 for these,
  # so post-sub must keep both at 0 rather than letting 1 - 0 = 1 create a fake share
  zero_use <- inputs$boston_fed_matrices$omega_M == 0 & inputs$boston_fed_matrices$omega_D == 0
  omega_D_post <- 1 - omega_M_post
  omega_D_post[zero_use] <- 0

  pe_postsub_results <- compute_boston_fed_prices(
    tau_M = tau_M_post,
    B_MD = inputs$boston_fed_matrices$B_MD,
    leontief_domestic = inputs$bea_leontief_domestic,
    omega_M = omega_M_post,
    omega_D = omega_D_post,
    pce_bridge = inputs$pce_bridge,
    usd_offset = inputs$assumptions$usd_offset,
    markup_assumption = markup_assumption
  )

  # GE prices (full general equilibrium from GTAP ppa)
  message('  GE prices (GTAP ppa):')
  ppa_usa <- inputs$ppa[, 'usa']
  ge_results <- compute_ge_prices(
    ppa_usa = ppa_usa,
    gtap_bea_crosswalk = inputs$gtap_bea_crosswalk,
    viws_baseline = inputs$baselines$viws_baseline,
    pce_bridge = inputs$pce_bridge,
    markup_assumption = markup_assumption
  )

  price_results <- list(
    pre_sub_price_increase = presub_results$aggregate * 100,
    pe_postsub_price_increase = pe_postsub_results$aggregate * 100,
    ge_price_increase = ge_results$aggregate * 100,
    presub = presub_results,
    pe_postsub = pe_postsub_results,
    ge = ge_results
  )

  # When using detail-level tables, reaggregate commodity prices to summary-level
  # NIPA categories for the distribution pipeline (which needs 2024 NIPA line numbers)
  if (inputs$bea_io_level == 'detail') {
    message('  Reaggregating detail prices to summary NIPA categories for distribution:')
    summary_bridge <- load_pce_bridge('resources/io')

    message('  Pre-substitution:')
    price_results$presub$pce_category_prices <- reaggregate_to_summary_pce(
      presub_results$bea_commodity_prices,
      inputs$bea_use_import, inputs$bea_use_domestic,
      inputs$summary_to_detail, summary_bridge, markup_assumption
    )
    message('  PE post-substitution:')
    price_results$pe_postsub$pce_category_prices <- reaggregate_to_summary_pce(
      pe_postsub_results$bea_commodity_prices,
      inputs$bea_use_import, inputs$bea_use_domestic,
      inputs$summary_to_detail, summary_bridge, markup_assumption
    )
    message('  GE:')
    price_results$ge$pce_category_prices <- reaggregate_to_summary_pce(
      ge_results$bea_commodity_prices,
      inputs$bea_use_import, inputs$bea_use_domestic,
      inputs$summary_to_detail, summary_bridge, markup_assumption
    )
  }

  # Store PCE category prices and BEA commodity prices for outputs
  inputs$pce_category_prices <- price_results$presub$pce_category_prices

  # Build BEA commodity detail table
  bea_prices_vec <- presub_results$bea_commodity_prices
  desc_file <- file.path(inputs$io_data_dir, 'bea_commodity_descriptions.csv')
  if (file.exists(desc_file)) {
    bea_desc <- read_csv(desc_file, show_col_types = FALSE) %>%
      rename(bea_code = `Commodity Code`, description = Description)
  } else {
    bea_desc <- tibble(bea_code = character(), description = character())
  }

  pce <- inputs$bea_pce_weights
  bea_commodities <- names(bea_prices_vec)
  inputs$bea_commodity_prices <- tibble(
    bea_code = bea_commodities,
    sr_price_effect = as.numeric(bea_prices_vec) * 100,
    pce_weight = as.numeric(pce[bea_commodities])
  )

  # Flag commodities with no PCE weight before filling (pure intermediates are
  # expected; goods with consumer expenditure missing would be a data issue)
  na_pce_codes <- inputs$bea_commodity_prices$bea_code[is.na(inputs$bea_commodity_prices$pce_weight)]
  if (length(na_pce_codes) > 0) {
    message(sprintf('  Note: %d BEA commodities have no PCE weight (intermediates, set to 0): %s',
                    length(na_pce_codes),
                    paste(head(na_pce_codes, 10), collapse = ', ')))
  }

  inputs$bea_commodity_prices <- inputs$bea_commodity_prices %>%
    mutate(pce_weight = if_else(is.na(pce_weight), 0, pce_weight)) %>%
    left_join(bea_desc, by = 'bea_code') %>%
    mutate(description = coalesce(description, bea_code)) %>%
    arrange(desc(sr_price_effect))

  message(sprintf('  Pre-sub aggregate:     %.3f%%', price_results$pre_sub_price_increase))
  message(sprintf('  PE post-sub aggregate: %.3f%%', price_results$pe_postsub_price_increase))
  message(sprintf('  GE aggregate:          %.3f%%', price_results$ge_price_increase))

  #---------------------------
  # Step 3a: Run USMM surrogate
  #---------------------------

  message('\nStep 3a: Running USMM surrogate...')
  usmm_results <- run_usmm_surrogate(etr_results, inputs)
  inputs$macro_quarterly <- usmm_results$macro_quarterly
  inputs$macro_quarterly_allperm <- usmm_results$macro_quarterly_allperm

  #---------------------------
  # Step 4: Calculate revenue
  #---------------------------

  message('\nStep 4: Calculating revenue estimates...')
  revenue_results <- calculate_revenue(inputs, etr_results)

  #---------------------------
  # Step 5: Calculate macro effects
  #---------------------------

  message('\nStep 5: Calculating macro effects...')
  if (is.null(inputs$macro_quarterly)) {
    stop('USMM macro data is required for macro calculations')
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
    distribution = distribution_results
  )

  #---------------------------
  # Step 10: Write outputs to disk
  #---------------------------

  message('\nStep 10: Writing outputs to disk...')
  write_outputs(results, scenario)

  # Print key results summary
  message('\n----------------------------------------------------------')
  message('KEY RESULTS')
  message('----------------------------------------------------------')
  message(sprintf('Baseline ETR (from levels):     %.3f%%', etr_results$baseline_etr))
  message(sprintf('Pre-substitution ETR increase:  %.2f%%', etr_results$pre_sub_increase))
  message(sprintf('Pre-sub all-in ETR:             %.3f%%', etr_results$pre_sub_all_in))
  message(sprintf('PE post-sub ETR increase:       %.2f%%', etr_results$pe_postsub_increase))
  message(sprintf('PE post-sub all-in ETR:         %.3f%%', etr_results$pe_postsub_all_in))
  message(sprintf('Pre-sub price increase:         %.3f%%', price_results$pre_sub_price_increase))
  message(sprintf('PE post-sub price increase:     %.3f%%', price_results$pe_postsub_price_increase))
  message(sprintf('GE price increase:              %.3f%%', price_results$ge_price_increase))

  # Per-HH costs come from distribution calculation (matches Excel methodology)
  if (!is.null(distribution_results)) {
    message(sprintf('Pre-sub per-HH cost:            $%.0f', abs(distribution_results$pre_sub_per_hh_cost)))
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
    message(sprintf('PCE 2025 Q4:                    %+.2f%%', macro_results$pce_2025))
    message(sprintf('PCE 2026 Q4:                    %+.2f%%', macro_results$pce_2026))
    message(sprintf('Fed funds 2025 Q4:              %+.2f pp', macro_results$fed_funds_2025))
    message(sprintf('Fed funds 2026 Q4:              %+.2f pp', macro_results$fed_funds_2026))
    # Export change from GTAP
    if (!is.null(inputs$qxwreg)) {
      message(sprintf('Export change (GTAP):           %.2f%%', inputs$qxwreg))
    }
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

  message('----------------------------------------------------------')
  message('BEA COMMODITY PRICE EFFECTS (Boston Fed)')
  message('----------------------------------------------------------')
  message(sprintf('Aggregate SR (PCE-weighted):     %.4f%%', presub_results$aggregate * 100))
  message(sprintf('  Direct import channel:         %.4f%%', presub_results$direct_aggregate * 100))
  message(sprintf('  Supply chain channel:          %.4f%%', presub_results$supply_chain_aggregate * 100))

  bea_detail <- inputs$bea_commodity_prices
  top_bea <- bea_detail %>% filter(sr_price_effect > 0.01) %>% head(15)
  message(sprintf('\nTop BEA commodities by SR price effect (%d with nonzero):',
                  sum(bea_detail$sr_price_effect > 0.001)))
  message(sprintf('  %-8s %-42s %8s %10s', 'Code', 'Description', 'SR (%)', 'PCE ($M)'))
  for (i in 1:nrow(top_bea)) {
    message(sprintf('  %-8s %-42s %8.3f %10s',
                    top_bea$bea_code[i],
                    substr(top_bea$description[i], 1, 42),
                    top_bea$sr_price_effect[i],
                    format(round(top_bea$pce_weight[i]), big.mark = ',')))
  }

  if (!is.null(inputs$pce_category_prices)) {
    pce_cats <- inputs$pce_category_prices
    pce_agg <- presub_results$aggregate * 100

    message('')
    message('----------------------------------------------------------')
    message('CONSUMER PRICE EFFECTS BY PCE CATEGORY (Boston Fed)')
    message('----------------------------------------------------------')
    message(sprintf('Aggregate (PCE-weighted):        %.4f%%', pce_agg))
    message('')
    message(sprintf('  %-4s %-44s %7s %12s %6s',
                    'Line', 'PCE Category', 'SR (%)', 'Value ($M)', '% PCE'))
    message(paste(rep('-', 80), collapse = ''))

    for (i in 1:nrow(pce_cats)) {
      cat_name <- pce_cats$pce_category[i]
      if (nchar(cat_name) > 43) cat_name <- substr(cat_name, 1, 43)
      message(sprintf('  %4d %-44s %7.3f %12s %5.1f%%',
                      pce_cats$nipa_line[i],
                      cat_name,
                      pce_cats$sr_price_effect[i],
                      format(round(pce_cats$purchasers_value[i]), big.mark = ','),
                      pce_cats$pce_share[i]))
    }
    message(paste(rep('-', 80), collapse = ''))
    message(sprintf('  %4s %-44s %7.3f %12s %5.1f%%',
                    '', 'TOTAL (PCE-weighted average)',
                    pce_agg,
                    format(round(sum(pce_cats$purchasers_value)), big.mark = ','),
                    100.0))
  }

  if (!is.null(foreign_gdp_results)) {
    message('----------------------------------------------------------')
    message('FOREIGN GDP EFFECTS (Long-Run)')
    message('----------------------------------------------------------')
    message(sprintf('USA:                            %+.2f%%', foreign_gdp_results$usa))
    lr_gdp_dollar_loss <- foreign_gdp_results$usa / 100 * 30441
    message(sprintf('USA (2025$):                    $%.0fB', lr_gdp_dollar_loss))
    message(sprintf('China:                          %+.2f%%', foreign_gdp_results$china))
    message(sprintf('Canada:                         %+.2f%%', foreign_gdp_results$canada))
    message(sprintf('Mexico:                         %+.2f%%', foreign_gdp_results$mexico))
    message(sprintf('EU:                             %+.2f%%', foreign_gdp_results$eu))
    message(sprintf('UK:                             %+.2f%%', foreign_gdp_results$uk))
    message(sprintf('Japan:                          %+.2f%%', foreign_gdp_results$japan))
    message(sprintf('ROW:                            %+.2f%%', foreign_gdp_results$row))
  }

  if (!is.null(distribution_results)) {
    message('----------------------------------------------------------')
    message('DISTRIBUTION BY INCOME DECILE (Pre-Substitution)')
    message('----------------------------------------------------------')
    dist <- distribution_results$by_decile
    for (i in 1:nrow(dist)) {
      message(sprintf('Decile %2d ($%s income):  $%s (%.2f%% of income)',
                      dist$decile[i],
                      format(dist$income[i], big.mark = ',', scientific = FALSE),
                      format(round(abs(dist$cost_per_hh[i])), big.mark = ',', scientific = FALSE),
                      abs(dist$pct_of_income[i])))
    }
    avg_cost <- distribution_results$avg_per_hh_cost
    message(sprintf('Average per-HH cost:            $%s',
                    format(round(abs(avg_cost)), big.mark = ',', scientific = FALSE)))
  }
  message('----------------------------------------------------------')

  message(sprintf('\nScenario %s complete!\n', scenario))

  invisible(results)
}
