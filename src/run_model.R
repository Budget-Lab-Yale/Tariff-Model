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


#' Average two sets of price results (element-wise)
#'
#' @param a Price results list from compute_io_prices() or compute_ge_prices()
#' @param b Price results list from same function with different markup_assumption
#' @return Averaged price results list
average_price_results <- function(a, b) {
  avg <- list(
    aggregate = (a$aggregate + b$aggregate) / 2,
    bea_commodity_prices = (a$bea_commodity_prices + b$bea_commodity_prices) / 2,
    markup_assumption = 'average'
  )

  # Average per-PCE-category prices (join by nipa_line to be safe)
  avg$pce_category_prices <- a$pce_category_prices %>%
    inner_join(
      b$pce_category_prices %>% select(nipa_line, sr_price_effect_b = sr_price_effect),
      by = 'nipa_line'
    ) %>%
    mutate(
      sr_price_effect = (sr_price_effect + sr_price_effect_b) / 2
    ) %>%
    select(-sr_price_effect_b)

  # Average decomposition if present (presub Boston Fed only)
  if (!is.null(a$direct_aggregate) && !is.null(b$direct_aggregate)) {
    avg$direct_aggregate <- (a$direct_aggregate + b$direct_aggregate) / 2
    avg$supply_chain_aggregate <- (a$supply_chain_aggregate + b$supply_chain_aggregate) / 2
  }

  return(avg)
}


#' Average GE decomposition results across markup assumptions
#'
#' @param a GE decomposition result from decompose_ge_prices()
#' @param b GE decomposition result from decompose_ge_prices()
#' @return Averaged decomposition result
average_ge_decomposition_results <- function(a, b) {
  avg_numeric_join <- function(x, y, keys) {
    numeric_cols <- setdiff(intersect(
      names(x)[vapply(x, is.numeric, logical(1))],
      names(y)[vapply(y, is.numeric, logical(1))]
    ), keys)
    keep_cols <- union(keys, setdiff(names(x), numeric_cols))

    joined <- x %>%
      inner_join(
        y %>% select(all_of(c(keys, numeric_cols))),
        by = keys,
        suffix = c('_a', '_b')
      )

    for (col in numeric_cols) {
      joined[[col]] <- (joined[[paste0(col, '_a')]] + joined[[paste0(col, '_b')]]) / 2
    }

    joined %>%
      select(all_of(keep_cols), all_of(numeric_cols))
  }

  summarize_pce_decomp <- function(pce_category) {
    summary <- tibble(
      metric = c(
        'import_price_component',
        'domestic_price_component',
        'share_shift_component',
        'residual_component',
        'ge_price_increase'
      ),
      value = c(
        sum(pce_category$import_price_component * pce_category$purchasers_value) /
          sum(pce_category$purchasers_value),
        sum(pce_category$domestic_price_component * pce_category$purchasers_value) /
          sum(pce_category$purchasers_value),
        sum(pce_category$share_shift_component * pce_category$purchasers_value) /
          sum(pce_category$purchasers_value),
        sum(pce_category$residual_component * pce_category$purchasers_value) /
          sum(pce_category$purchasers_value),
        sum(pce_category$ge * pce_category$purchasers_value) /
          sum(pce_category$purchasers_value)
      ),
      unit = 'pct'
    )

    summary
  }

  pce_category <- avg_numeric_join(a$pce_category, b$pce_category, 'nipa_line')

  list(
    gtap_commodity = a$gtap_commodity,
    bea_commodity = avg_numeric_join(a$bea_commodity, b$bea_commodity, 'bea_code'),
    pce_category = pce_category,
    summary = summarize_pce_decomp(pce_category),
    markup_assumption = 'average'
  )
}


#' Reaggregate GE decomposition PCE outputs from detail to summary categories
#'
#' @param decomp GE decomposition result from decompose_ge_prices()
#' @param use_import Detail import use matrix
#' @param use_domestic Detail domestic use matrix
#' @param summary_to_detail Detail-to-summary commodity mapping
#' @param summary_bridge Summary-level PCE bridge
#' @param markup_assumption Either constant_percentage or constant_dollar
#' @param presub_pce_category_prices Summary-level presub category prices
#' @return Decomposition result with summary-level PCE categories and summary metrics
reaggregate_ge_decomp_to_summary <- function(decomp, use_import, use_domestic,
                                             summary_to_detail, summary_bridge,
                                             markup_assumption,
                                             presub_pce_category_prices = NULL) {
  reagg_term <- function(column_name) {
    prices <- setNames(decomp$bea_commodity[[column_name]] / 100, decomp$bea_commodity$bea_code)
    reaggregate_to_summary_pce(
      prices,
      use_import = use_import,
      use_domestic = use_domestic,
      summary_to_detail = summary_to_detail,
      summary_bridge = summary_bridge,
      markup_assumption = markup_assumption
    ) %>%
      select(nipa_line, pce_category, purchasers_value, pce_share, value = sr_price_effect)
  }

  pce_import <- reagg_term('import_price_component') %>%
    rename(import_price_component = value)
  pce_domestic <- reagg_term('domestic_price_component') %>%
    select(nipa_line, domestic_price_component = value)
  pce_share_shift <- reagg_term('share_shift_component') %>%
    select(nipa_line, share_shift_component = value)
  pce_residual <- reagg_term('residual_component') %>%
    select(nipa_line, residual_component = value)
  pce_ge <- reagg_term('ppa') %>%
    select(nipa_line, ge = value)

  pce_category <- pce_import %>%
    left_join(pce_domestic, by = 'nipa_line') %>%
    left_join(pce_share_shift, by = 'nipa_line') %>%
    left_join(pce_residual, by = 'nipa_line') %>%
    left_join(pce_ge, by = 'nipa_line')

  if (!is.null(presub_pce_category_prices)) {
    pce_category <- pce_category %>%
      left_join(
        presub_pce_category_prices %>% select(nipa_line, pre_sub = sr_price_effect),
        by = 'nipa_line'
      ) %>%
      mutate(ge_minus_pre_sub = ge - pre_sub)
  }

  summary <- tibble(
    metric = c(
      'import_price_component',
      'domestic_price_component',
      'share_shift_component',
      'residual_component',
      'ge_price_increase'
    ),
    value = c(
      sum(pce_category$import_price_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$domestic_price_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$share_shift_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$residual_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$ge * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value)
    ),
    unit = 'pct'
  )

  decomp$pce_category <- pce_category
  decomp$summary <- summary
  decomp
}


#' Run the complete tariff model for a scenario
#'
#' @param scenario Name of the scenario (must exist in config/scenarios/)
#' @param markup_assumption Markup response to cost changes:
#'   'average' (default, mean of upper and lower bounds),
#'   'constant_percentage' (upper bound), or 'constant_dollar' (lower bound)
#' @param bea_io_level BEA I-O table level: 'summary' (73 commodities, 2024) or
#'   'detail' (~400 commodities, 2017). NULL uses value from global_assumptions.yaml.
#'
#' @return List containing all model outputs
run_scenario <- function(scenario, markup_assumption = 'average',
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
  # Step 3: Calculate consumer price effects (I-O model)
  #---------------------------

  message('\nStep 3: Calculating consumer price effects...')
  message(sprintf('  Markup assumption: %s', markup_assumption))

  # Helper: compute all three price concepts for a given markup assumption
  compute_all_prices <- function(ma, matrices) {
    ppa_usa <- inputs$ppa[, 'usa']

    # Pre-substitution
    presub <- compute_io_prices(
      tau_M = inputs$tau_M,
      B_MD = matrices$B_MD,
      leontief_domestic = inputs$bea_leontief_domestic,
      omega_M = matrices$omega_M,
      omega_D = matrices$omega_D,
      pce_bridge = inputs$pce_bridge,
      usd_offset = inputs$assumptions$usd_offset,
      markup_assumption = ma
    )

    # PE post-substitution
    tau_M_post <- compute_postsub_tau_M(
      inputs$tau_M, inputs$gtap_bea_crosswalk,
      inputs$etr_matrix, inputs$viws, inputs$baselines$viws_baseline
    )
    omega_M_post <- compute_postsub_omega_M(
      matrices$omega_M, inputs$gtap_bea_crosswalk,
      inputs$nvpp_commodity_ratio, inputs$baselines$viws_baseline
    )
    zero_use <- matrices$omega_M == 0 & matrices$omega_D == 0
    omega_D_post <- 1 - omega_M_post
    omega_D_post[zero_use] <- 0

    pe_postsub <- compute_io_prices(
      tau_M = tau_M_post,
      B_MD = matrices$B_MD,
      leontief_domestic = inputs$bea_leontief_domestic,
      omega_M = omega_M_post,
      omega_D = omega_D_post,
      pce_bridge = inputs$pce_bridge,
      usd_offset = inputs$assumptions$usd_offset,
      markup_assumption = ma
    )

    # GE prices
    ge <- compute_ge_prices(
      ppa_usa = ppa_usa,
      gtap_bea_crosswalk = inputs$gtap_bea_crosswalk,
      viws_baseline = inputs$baselines$viws_baseline,
      pce_bridge = inputs$pce_bridge,
      markup_assumption = ma
    )

    ge_decomp <- decompose_ge_prices(
      ppa_usa = ppa_usa,
      ppm_usa = inputs$ppm[, 'usa'],
      ppd_usa = inputs$ppd[, 'usa'],
      gtap_bea_crosswalk = inputs$gtap_bea_crosswalk,
      viws_baseline = inputs$baselines$viws_baseline,
      pce_bridge = inputs$pce_bridge,
      nvpp_adjustment = inputs$nvpp_adjustment,
      markup_assumption = ma,
      presub_pce_category_prices = presub$pce_category_prices
    )

    # Detail-level reaggregation if needed
    if (inputs$bea_io_level == 'detail') {
      summary_bridge <- load_pce_bridge('resources/io')
      presub$pce_category_prices <- reaggregate_to_summary_pce(
        presub$bea_commodity_prices,
        inputs$bea_use_import, inputs$bea_use_domestic,
        inputs$summary_to_detail, summary_bridge, ma
      )
      pe_postsub$pce_category_prices <- reaggregate_to_summary_pce(
        pe_postsub$bea_commodity_prices,
        inputs$bea_use_import, inputs$bea_use_domestic,
        inputs$summary_to_detail, summary_bridge, ma
      )
      ge$pce_category_prices <- reaggregate_to_summary_pce(
        ge$bea_commodity_prices,
        inputs$bea_use_import, inputs$bea_use_domestic,
        inputs$summary_to_detail, summary_bridge, ma
      )
      ge_decomp <- reaggregate_ge_decomp_to_summary(
        ge_decomp,
        use_import = inputs$bea_use_import,
        use_domestic = inputs$bea_use_domestic,
        summary_to_detail = inputs$summary_to_detail,
        summary_bridge = summary_bridge,
        markup_assumption = ma,
        presub_pce_category_prices = presub$pce_category_prices
      )
    }

    return(list(presub = presub, pe_postsub = pe_postsub, ge = ge, ge_decomp = ge_decomp))
  }

  if (markup_assumption == 'average') {
    message('  Running constant_percentage (upper bound):')
    cp_prices <- compute_all_prices('constant_percentage', inputs$io_matrices_cp)
    message('  Running constant_dollar (lower bound):')
    cd_prices <- compute_all_prices('constant_dollar', inputs$io_matrices_cd)

    message('  Averaging upper and lower bounds...')
    presub_results <- average_price_results(cp_prices$presub, cd_prices$presub)
    pe_postsub_results <- average_price_results(cp_prices$pe_postsub, cd_prices$pe_postsub)
    ge_results <- average_price_results(cp_prices$ge, cd_prices$ge)
    ge_decomp_results <- average_ge_decomposition_results(cp_prices$ge_decomp, cd_prices$ge_decomp)
  } else {
    prices <- compute_all_prices(markup_assumption, inputs$io_matrices)
    presub_results <- prices$presub
    pe_postsub_results <- prices$pe_postsub
    ge_results <- prices$ge
    ge_decomp_results <- prices$ge_decomp
  }

  # GTAP ppriv: aggregate private consumption price (CDE utility-weighted)
  if (!is.null(inputs$ppriv)) {
    gtap_ppriv <- inputs$ppriv['usa']
    message(sprintf('  GTAP ppriv (aggregate consumption price):  %.4f%%', gtap_ppriv))
    message(sprintf('  GE via BEA/PCE bridge:                     %.4f%%',
                    ge_results$aggregate * 100))
  }

  price_results <- list(
    pre_sub_price_increase = presub_results$aggregate * 100,
    pe_postsub_price_increase = pe_postsub_results$aggregate * 100,
    ge_price_increase = ge_results$aggregate * 100,
    presub = presub_results,
    pe_postsub = pe_postsub_results,
    ge = ge_results
  )

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
    ge_decomp = ge_decomp_results,
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
  message('BEA COMMODITY PRICE EFFECTS (I-O model)')
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
    message('CONSUMER PRICE EFFECTS BY PCE CATEGORY (I-O model)')
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
    avg_cost <- distribution_results$pre_sub_per_hh_cost
    message(sprintf('Average per-HH cost:            $%s',
                    format(round(abs(avg_cost)), big.mark = ',', scientific = FALSE)))
  }
  message('----------------------------------------------------------')

  message(sprintf('\nScenario %s complete!\n', scenario))

  invisible(results)
}
