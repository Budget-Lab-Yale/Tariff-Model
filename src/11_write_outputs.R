# =============================================================================
# 11_write_outputs.R - Write all model outputs to disk
# =============================================================================

#' Write all model results to output directory
#'
#' @param results List containing all model outputs from run_scenario()
#' @param scenario Name of the scenario
#'
#' @return Invisibly returns the output directory path
write_outputs <- function(results, scenario) {

  output_dir <- file.path('output', scenario, 'results')

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  message(sprintf('  Writing outputs to %s/', output_dir))

  required_sections <- c('etr', 'prices', 'revenue', 'dynamic', 'macro',
                         'distribution', 'sectors', 'foreign_gdp')
  for (section in required_sections) {
    if (is.null(results[[section]])) {
      stop('Missing required results section: ', section)
    }
  }

  # ============================
  # Key Results Summary
  # ============================

  key_results <- tibble(
    metric = c(
      # ETR
      'pre_sub_etr_increase', 'pe_postsub_etr_increase',
      'baseline_etr', 'pre_sub_all_in_etr', 'pe_postsub_all_in_etr',
      # Prices
      'pre_sub_price_increase', 'pe_postsub_price_increase',
      'ge_price_increase', 'pre_sub_per_hh_cost', 'post_sub_per_hh_cost',
      # Price decomposition (Boston Fed)
      'direct_aggregate', 'supply_chain_aggregate',
      # Revenue
      'gross_revenue_10yr', 'conventional_revenue_10yr',
      # Dynamic revenue
      'dynamic_effect_10yr', 'dynamic_revenue_10yr',
      # Macro
      'gdp_2025_q4q4', 'gdp_2026_q4q4',
      'urate_2025_q4', 'urate_2026_q4',
      'payroll_2025_q4', 'payroll_2026_q4',
      'pce_2025_q4', 'pce_2026_q4',
      'fed_funds_2025_q4', 'fed_funds_2026_q4'
    ),
    value = c(
      results$etr$pre_sub_increase, results$etr$pe_postsub_increase,
      results$etr$baseline_etr, results$etr$pre_sub_all_in, results$etr$pe_postsub_all_in,
      results$prices$pre_sub_price_increase, results$prices$pe_postsub_price_increase,
      results$prices$ge_price_increase,
      abs(results$distribution$pre_sub_per_hh_cost),
      abs(results$distribution$post_sub_per_hh_cost),
      results$prices$presub$direct_aggregate * 100,
      results$prices$presub$supply_chain_aggregate * 100,
      results$revenue$gross_10yr, results$revenue$conventional_10yr,
      results$dynamic$dynamic_effect_10yr, results$dynamic$dynamic_10yr,
      results$macro$gdp_2025, results$macro$gdp_2026,
      results$macro$urate_2025, results$macro$urate_2026,
      results$macro$payroll_2025, results$macro$payroll_2026,
      results$macro$pce_2025, results$macro$pce_2026,
      results$macro$fed_funds_2025, results$macro$fed_funds_2026
    ),
    unit = c(
      'pct', 'pct',
      'pct', 'pct', 'pct',
      'pct', 'pct', 'pct', 'dollars', 'dollars',
      'pct', 'pct',
      'billions', 'billions',
      'billions', 'billions',
      'pct', 'pct', 'pp', 'pp', 'thousands', 'thousands',
      'pct', 'pct', 'pp', 'pp'
    )
  )

  write_csv(key_results, file.path(output_dir, 'key_results.csv'))
  message(sprintf('    key_results.csv (%d metrics)', nrow(key_results)))

  # ============================
  # Revenue by Year
  # ============================

  if (is.null(results$revenue$by_year)) {
    stop('revenue$by_year not found in results')
  }
  write_csv(results$revenue$by_year, file.path(output_dir, 'revenue_by_year.csv'))
  message(sprintf('    revenue_by_year.csv (%d years)', nrow(results$revenue$by_year)))

  # ============================
  # Dynamic Revenue by Year
  # ============================

  if (is.null(results$dynamic$by_year)) {
    stop('dynamic$by_year not found in results')
  }
  write_csv(results$dynamic$by_year, file.path(output_dir, 'dynamic_revenue_by_year.csv'))
  message(sprintf('    dynamic_revenue_by_year.csv (%d years)', nrow(results$dynamic$by_year)))

  # ============================
  # Macro Effects (Quarterly)
  # ============================

  if (is.null(results$macro$quarterly)) {
    stop('macro$quarterly not found in results')
  }
  write_csv(results$macro$quarterly, file.path(output_dir, 'macro_quarterly.csv'))
  message(sprintf('    macro_quarterly.csv (%d quarters)', nrow(results$macro$quarterly)))

  # ============================
  # Sector Effects
  # ============================

  sector_results <- tibble(
    sector = c('agriculture', 'mining', 'manufacturing', 'durable', 'nondurable',
               'advanced', 'utilities', 'construction', 'services', 'overall_gdp'),
    output_change_pct = c(
      results$sectors$agriculture,
      results$sectors$mining,
      results$sectors$manufacturing,
      results$sectors$durable,
      results$sectors$nondurable,
      results$sectors$advanced,
      results$sectors$utilities,
      results$sectors$construction,
      results$sectors$services,
      results$sectors$overall_gdp
    )
  )
  write_csv(sector_results, file.path(output_dir, 'sector_effects.csv'))
  message('    sector_effects.csv (10 sectors)')

  # ============================
  # Foreign GDP Effects
  # ============================

  foreign_gdp <- tibble(
    region = c('usa', 'china', 'canada', 'mexico', 'eu', 'uk', 'japan', 'fta', 'row',
               'world', 'world_ex_usa'),
    gdp_change_pct = c(
      results$foreign_gdp$usa,
      results$foreign_gdp$china,
      results$foreign_gdp$canada,
      results$foreign_gdp$mexico,
      results$foreign_gdp$eu,
      results$foreign_gdp$uk,
      results$foreign_gdp$japan,
      results$foreign_gdp$fta,
      results$foreign_gdp$row,
      results$foreign_gdp$world,
      results$foreign_gdp$world_ex_usa
    )
  )
  write_csv(foreign_gdp, file.path(output_dir, 'foreign_gdp.csv'))
  message('    foreign_gdp.csv (11 regions)')

  # ============================
  # Distribution by Decile
  # ============================

  write_csv(results$distribution$by_decile,
            file.path(output_dir, 'distribution.csv'))
  message('    distribution.csv (10 deciles)')

  if (!is.null(results$distribution$by_decile_post_sub)) {
    write_csv(results$distribution$by_decile_post_sub,
              file.path(output_dir, 'distribution_postsub.csv'))
    message('    distribution_postsub.csv (10 deciles)')
  }

  # ============================
  # PCE Category Prices (I-O model)
  # ============================

  if (is.null(results$prices$presub$pce_category_prices) ||
      is.null(results$prices$pe_postsub$pce_category_prices) ||
      is.null(results$prices$ge$pce_category_prices)) {
    stop('pce_category_prices not found for one or more price concepts in results$prices')
  }

  pce_category_prices <- results$prices$presub$pce_category_prices %>%
    select(
      nipa_line,
      pce_category,
      purchasers_value,
      pce_share,
      pre_sub = sr_price_effect
    ) %>%
    left_join(
      results$prices$pe_postsub$pce_category_prices %>%
        select(nipa_line, pe_postsub = sr_price_effect),
      by = 'nipa_line'
    ) %>%
    left_join(
      results$prices$ge$pce_category_prices %>%
        select(nipa_line, ge = sr_price_effect),
      by = 'nipa_line'
    ) %>%
    arrange(desc(pre_sub))

  write_csv(pce_category_prices,
            file.path(output_dir, 'pce_category_prices.csv'))
  message(sprintf('    pce_category_prices.csv (%d consumer categories)',
                  nrow(pce_category_prices)))

  # ============================
  # PCE Major Category Prices (BEA Table 2.4.5 hierarchy)
  # ============================

  major_cat_file <- 'resources/mappings/nipa_pce_major_category.csv'
  if (file.exists(major_cat_file)) {
    major_cat_map <- read_csv(major_cat_file, show_col_types = FALSE)

    pce_major_prices <- pce_category_prices %>%
      inner_join(major_cat_map, by = 'nipa_line') %>%
      group_by(top_level, major_category) %>%
      summarise(
        pre_sub = sum(pre_sub * purchasers_value) / sum(purchasers_value),
        pe_postsub = sum(pe_postsub * purchasers_value) / sum(purchasers_value),
        ge = sum(ge * purchasers_value) / sum(purchasers_value),
        purchasers_value = sum(purchasers_value),
        n_categories = n(),
        .groups = 'drop'
      ) %>%
      mutate(pce_share = purchasers_value / sum(purchasers_value) * 100) %>%
      arrange(desc(pre_sub))

    write_csv(pce_major_prices,
              file.path(output_dir, 'pce_major_category_prices.csv'))
    message(sprintf('    pce_major_category_prices.csv (%d major categories)',
                    nrow(pce_major_prices)))
  }

  # ============================
  # BEA Commodity Prices (I-O model)
  # ============================

  if (!is.null(results$inputs$bea_commodity_prices)) {
    write_csv(results$inputs$bea_commodity_prices,
              file.path(output_dir, 'bea_commodity_prices.csv'))
    message(sprintf('    bea_commodity_prices.csv (%d BEA commodities)',
                    nrow(results$inputs$bea_commodity_prices)))
  }

  # ============================
  # GTAP GE Decomposition
  # ============================

  if (!is.null(results$ge_decomp)) {
    if (!is.null(results$ge_decomp$summary)) {
      write_csv(results$ge_decomp$summary,
                file.path(output_dir, 'gtap_ge_decomp_summary.csv'))
      message(sprintf('    gtap_ge_decomp_summary.csv (%d metrics)',
                      nrow(results$ge_decomp$summary)))
    }

    if (!is.null(results$ge_decomp$gtap_commodity)) {
      write_csv(results$ge_decomp$gtap_commodity,
                file.path(output_dir, 'gtap_ge_decomp_by_commodity.csv'))
      message(sprintf('    gtap_ge_decomp_by_commodity.csv (%d GTAP commodities)',
                      nrow(results$ge_decomp$gtap_commodity)))
    }

    if (!is.null(results$ge_decomp$bea_commodity)) {
      write_csv(results$ge_decomp$bea_commodity,
                file.path(output_dir, 'gtap_ge_decomp_by_bea_commodity.csv'))
      message(sprintf('    gtap_ge_decomp_by_bea_commodity.csv (%d BEA commodities)',
                      nrow(results$ge_decomp$bea_commodity)))
    }

    if (!is.null(results$ge_decomp$pce_category)) {
      write_csv(results$ge_decomp$pce_category,
                file.path(output_dir, 'gtap_ge_decomp_by_pce_category.csv'))
      message(sprintf('    gtap_ge_decomp_by_pce_category.csv (%d PCE categories)',
                      nrow(results$ge_decomp$pce_category)))
    }
  }

  # ============================
  # Goods-Weighted ETRs by Country
  # ============================

  if (is.null(results$etr$postsim_country)) {
    stop('etr$postsim_country not found in results')
  }
  if (is.null(results$etr$presim_country)) {
    stop('etr$presim_country not found in results')
  }

  country_codes <- c('chn', 'ca', 'mx', 'uk', 'jp', 'eu', 'row', 'fta')
  country_names <- c('China', 'Canada', 'Mexico', 'UK', 'Japan', 'EU', 'ROW', 'FTA')

  required_import_cols <- paste0('imports_', country_codes)
  required_etr_cols <- paste0('etr_', country_codes)
  missing_post <- setdiff(c(required_import_cols, required_etr_cols), names(results$etr$postsim_country))
  if (length(missing_post) > 0) {
    stop('Missing required columns in etr$postsim_country: ', paste(missing_post, collapse = ', '))
  }
  missing_pre <- setdiff(c(required_import_cols, required_etr_cols), names(results$etr$presim_country))
  if (length(missing_pre) > 0) {
    stop('Missing required columns in etr$presim_country: ', paste(missing_pre, collapse = ', '))
  }

  has_levels <- !is.null(results$etr$baseline_country_levels) &&
                !is.null(results$etr$presim_country_levels) &&
                !is.null(results$etr$postsim_country_levels)

  goods_etrs <- tibble(
    country = country_names,
    country_code = country_codes,
    pe_postsub_imports = sapply(country_codes, function(code)
      results$etr$postsim_country[[paste0('imports_', code)]]),
    pe_postsub_etr = sapply(country_codes, function(code)
      results$etr$postsim_country[[paste0('etr_', code)]]),
    presub_imports = sapply(country_codes, function(code)
      results$etr$presim_country[[paste0('imports_', code)]]),
    presub_etr = sapply(country_codes, function(code)
      results$etr$presim_country[[paste0('etr_', code)]])
  )

  if (has_levels) {
    goods_etrs <- goods_etrs %>%
      mutate(
        baseline_level = sapply(country_codes, function(code)
          results$etr$baseline_country_levels[[paste0('etr_', code)]]),
        presub_level = sapply(country_codes, function(code)
          results$etr$presim_country_levels[[paste0('etr_', code)]]),
        pe_postsub_level = sapply(country_codes, function(code)
          results$etr$postsim_country_levels[[paste0('etr_', code)]])
      )
  }

  total_row <- tibble(
    country = 'TOTAL',
    country_code = 'all',
    pe_postsub_imports = sum(goods_etrs$pe_postsub_imports),
    pe_postsub_etr = results$etr$pe_postsub_increase,
    presub_imports = sum(goods_etrs$presub_imports),
    presub_etr = results$etr$pre_sub_increase
  )

  if (has_levels) {
    total_row <- total_row %>%
      mutate(
        baseline_level = results$etr$baseline_etr,
        presub_level = results$etr$pre_sub_all_in,
        pe_postsub_level = results$etr$pe_postsub_all_in
      )
  }

  goods_etrs <- bind_rows(goods_etrs, total_row)

  write_csv(goods_etrs, file.path(output_dir, 'goods_weighted_etrs.csv'))
  message('    goods_weighted_etrs.csv (8 countries + total)')

  # ============================
  # Census-Country Passthrough (optional)
  # ============================

  if (!is.null(results$inputs$census_country_deltas)) {
    write_csv(results$inputs$census_country_deltas,
              file.path(output_dir, 'census_country_deltas.csv'))
    message(sprintf('    census_country_deltas.csv (%d rows)',
                    nrow(results$inputs$census_country_deltas)))
  }

  if (!is.null(results$inputs$census_country_levels)) {
    write_csv(results$inputs$census_country_levels,
              file.path(output_dir, 'census_country_levels.csv'))
    message(sprintf('    census_country_levels.csv (%d rows)',
                    nrow(results$inputs$census_country_levels)))
  }

  if (!is.null(results$inputs$baseline_census_country_levels)) {
    write_csv(results$inputs$baseline_census_country_levels,
              file.path(output_dir, 'baseline_census_country_levels.csv'))
    message(sprintf('    baseline_census_country_levels.csv (%d rows)',
                    nrow(results$inputs$baseline_census_country_levels)))
  }

  # ============================
  # Sector-Country Matrices (raw Tariff-ETRs output)
  # ============================

  if (!is.null(results$inputs$levels_matrix)) {
    write_csv(results$inputs$levels_matrix,
              file.path(output_dir, 'sector_country_levels.csv'))
    message(sprintf('    sector_country_levels.csv (%d sectors)',
                    nrow(results$inputs$levels_matrix)))
  }

  if (!is.null(results$inputs$etr_matrix)) {
    write_csv(results$inputs$etr_matrix,
              file.path(output_dir, 'sector_country_deltas.csv'))
    message(sprintf('    sector_country_deltas.csv (%d sectors)',
                    nrow(results$inputs$etr_matrix)))
  }

  message(sprintf('  Done. All outputs written to %s/', output_dir))

  invisible(output_dir)
}
