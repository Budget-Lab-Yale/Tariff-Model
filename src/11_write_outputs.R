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
                         'distribution', 'products', 'sectors', 'foreign_gdp')
  for (section in required_sections) {
    if (is.null(results[[section]])) {
      stop('Missing required results section: ', section)
    }
  }

  # ============================
  # Key Results Summary
  # ============================

  key_results <- tibble(
    metric = character(),
    value = numeric(),
    unit = character()
  )

  # ETR
  key_results <- key_results %>%
    add_row(metric = 'pre_sub_etr_increase', value = results$etr$pre_sub_increase, unit = 'pct') %>%
    add_row(metric = 'post_sub_etr_increase', value = results$etr$post_sub_increase, unit = 'pct')

  # Prices
  # Per-HH costs come from distribution calculation (matches Excel: Key Results B13/B15
  # pull from F6 Distribution via ricco_price_effects_and_etr!I24/I25)
  key_results <- key_results %>%
    add_row(metric = 'pre_sub_price_increase', value = results$prices$pre_sub_price_increase, unit = 'pct') %>%
    add_row(metric = 'post_sub_price_increase', value = results$prices$post_sub_price_increase, unit = 'pct') %>%
    add_row(metric = 'pre_sub_per_hh_cost', value = abs(results$distribution$pre_sub_per_hh_cost), unit = 'dollars') %>%
    add_row(metric = 'post_sub_per_hh_cost', value = abs(results$distribution$post_sub_per_hh_cost), unit = 'dollars')

  # Revenue
  key_results <- key_results %>%
    add_row(metric = 'gross_revenue_10yr', value = results$revenue$gross_10yr, unit = 'billions') %>%
    add_row(metric = 'conventional_revenue_10yr', value = results$revenue$conventional_10yr, unit = 'billions')

  # Dynamic revenue
  key_results <- key_results %>%
    add_row(metric = 'dynamic_effect_10yr', value = results$dynamic$dynamic_effect_10yr, unit = 'billions') %>%
    add_row(metric = 'dynamic_revenue_10yr', value = results$dynamic$dynamic_10yr, unit = 'billions')

  # Macro
  key_results <- key_results %>%
    add_row(metric = 'gdp_2025_q4q4', value = results$macro$gdp_2025, unit = 'pct') %>%
    add_row(metric = 'gdp_2026_q4q4', value = results$macro$gdp_2026, unit = 'pct') %>%
    add_row(metric = 'urate_2025_q4', value = results$macro$urate_2025, unit = 'pp') %>%
    add_row(metric = 'urate_2026_q4', value = results$macro$urate_2026, unit = 'pp') %>%
    add_row(metric = 'payroll_2025_q4', value = results$macro$payroll_2025, unit = 'thousands') %>%
    add_row(metric = 'payroll_2026_q4', value = results$macro$payroll_2026, unit = 'thousands')

  # Distribution (avg_per_hh_cost removed - now reported as pre_sub_per_hh_cost above)

  # Products
  key_results <- key_results %>%
    add_row(metric = 'food_price_sr', value = results$products$food_sr, unit = 'pct') %>%
    add_row(metric = 'food_price_lr', value = results$products$food_lr, unit = 'pct')

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

  # ============================
  # Product Prices
  # ============================

  if (is.null(results$products$products)) {
    stop('products$products not found in results')
  }
  write_csv(results$products$products, file.path(output_dir, 'product_prices.csv'))
  message(sprintf('    product_prices.csv (%d products)', nrow(results$products$products)))

  # ============================
  # Goods-Weighted ETRs by Country
  # ============================

  if (is.null(results$etr$postsim_country)) {
    stop('etr$postsim_country not found in results')
  }
  if (is.null(results$etr$presim_country)) {
    stop('etr$presim_country not found in results')
  }

  # Create output with pre-sub and post-sub ETRs by country
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

  # Actually, create a simpler format - one row per country with columns for pre/post
  goods_etrs <- tibble(
    country = country_names,
    country_code = country_codes,
    postsub_imports = sapply(country_codes, function(c)
      results$etr$postsim_country[[paste0('imports_', c)]]),
    postsub_etr = sapply(country_codes, function(c)
      results$etr$postsim_country[[paste0('etr_', c)]]),
    presub_imports = sapply(country_codes, function(c)
      results$etr$presim_country[[paste0('imports_', c)]]),
    presub_etr = sapply(country_codes, function(c)
      results$etr$presim_country[[paste0('etr_', c)]])
  )

  # Add totals row
  total_row <- tibble(
    country = 'TOTAL',
    country_code = 'all',
    postsub_imports = sum(goods_etrs$postsub_imports),
    postsub_etr = results$etr$post_sub_increase,
    presub_imports = sum(goods_etrs$presub_imports),
    presub_etr = results$etr$pre_sub_increase
  )

  goods_etrs <- bind_rows(goods_etrs, total_row)

  write_csv(goods_etrs, file.path(output_dir, 'goods_weighted_etrs.csv'))
  message('    goods_weighted_etrs.csv (8 countries + total)')

  message(sprintf('  Done. All outputs written to %s/', output_dir))

  invisible(output_dir)
}
