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

  # Per-HH costs come from distribution calculation (matches Excel: Key Results B13/B15
  # pull from F6 Distribution via ricco_price_effects_and_etr!I24/I25)
  key_results <- tibble(
    metric = c(
      # ETR
      'pre_sub_etr_increase', 'post_sub_etr_increase',
      'baseline_etr', 'pre_sub_all_in_etr', 'post_sub_all_in_etr',
      # Prices
      'pre_sub_price_increase', 'post_sub_price_increase',
      'pre_sub_per_hh_cost', 'post_sub_per_hh_cost',
      # Revenue
      'gross_revenue_10yr', 'conventional_revenue_10yr',
      # Dynamic revenue
      'dynamic_effect_10yr', 'dynamic_revenue_10yr',
      # Macro
      'gdp_2025_q4q4', 'gdp_2026_q4q4',
      'urate_2025_q4', 'urate_2026_q4',
      'payroll_2025_q4', 'payroll_2026_q4',
      # Products
      'food_price_sr', 'food_price_lr'
    ),
    value = c(
      results$etr$pre_sub_increase, results$etr$post_sub_increase,
      results$etr$baseline_etr, results$etr$pre_sub_all_in, results$etr$post_sub_all_in,
      results$prices$pre_sub_price_increase, results$prices$post_sub_price_increase,
      abs(results$distribution$pre_sub_per_hh_cost), abs(results$distribution$post_sub_per_hh_cost),
      results$revenue$gross_10yr, results$revenue$conventional_10yr,
      results$dynamic$dynamic_effect_10yr, results$dynamic$dynamic_10yr,
      results$macro$gdp_2025, results$macro$gdp_2026,
      results$macro$urate_2025, results$macro$urate_2026,
      results$macro$payroll_2025, results$macro$payroll_2026,
      results$products$food_sr, results$products$food_lr
    ),
    unit = c(
      'pct', 'pct',
      'pct', 'pct', 'pct',
      'pct', 'pct', 'dollars', 'dollars',
      'billions', 'billions',
      'billions', 'billions',
      'pct', 'pct', 'pp', 'pp', 'thousands', 'thousands',
      'pct', 'pct'
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
  # Include both delta ETRs and level ETRs (if available)
  has_levels <- !is.null(results$etr$baseline_country_levels) &&
                !is.null(results$etr$presim_country_levels) &&
                !is.null(results$etr$postsim_country_levels)

  goods_etrs <- tibble(
    country = country_names,
    country_code = country_codes,
    postsub_imports = sapply(country_codes, function(code)
      results$etr$postsim_country[[paste0('imports_', code)]]),
    postsub_etr = sapply(country_codes, function(code)
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
        postsub_level = sapply(country_codes, function(code)
          results$etr$postsim_country_levels[[paste0('etr_', code)]])
      )
  }

  # Add totals row
  total_row <- tibble(
    country = 'TOTAL',
    country_code = 'all',
    postsub_imports = sum(goods_etrs$postsub_imports),
    postsub_etr = results$etr$post_sub_increase,
    presub_imports = sum(goods_etrs$presub_imports),
    presub_etr = results$etr$pre_sub_increase
  )

  if (has_levels) {
    total_row <- total_row %>%
      mutate(
        baseline_level = results$etr$baseline_etr,
        presub_level = results$etr$pre_sub_all_in,
        postsub_level = results$etr$post_sub_all_in
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
