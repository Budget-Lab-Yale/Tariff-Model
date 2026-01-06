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
  key_results <- key_results %>%
    add_row(metric = 'pre_sub_price_increase', value = results$prices$pre_sub_price_increase, unit = 'pct') %>%
    add_row(metric = 'post_sub_price_increase', value = results$prices$post_sub_price_increase, unit = 'pct') %>%
    add_row(metric = 'pre_sub_per_hh_cost', value = abs(results$prices$pre_sub_per_hh_cost), unit = 'dollars') %>%
    add_row(metric = 'post_sub_per_hh_cost', value = abs(results$prices$post_sub_per_hh_cost), unit = 'dollars')

  # Revenue
  key_results <- key_results %>%
    add_row(metric = 'gross_revenue_10yr', value = results$revenue$gross_10yr, unit = 'billions') %>%
    add_row(metric = 'conventional_revenue_10yr', value = results$revenue$conventional_10yr, unit = 'billions')

  # Dynamic revenue
  if (!is.null(results$dynamic)) {
    key_results <- key_results %>%
      add_row(metric = 'dynamic_effect_10yr', value = results$dynamic$dynamic_effect_10yr, unit = 'billions') %>%
      add_row(metric = 'dynamic_revenue_10yr', value = results$dynamic$dynamic_10yr, unit = 'billions')
  }

  # Macro
  if (!is.null(results$macro)) {
    key_results <- key_results %>%
      add_row(metric = 'gdp_2025_q4q4', value = results$macro$gdp_2025, unit = 'pct') %>%
      add_row(metric = 'gdp_2026_q4q4', value = results$macro$gdp_2026, unit = 'pct') %>%
      add_row(metric = 'urate_2025_q4', value = results$macro$urate_2025, unit = 'pp') %>%
      add_row(metric = 'urate_2026_q4', value = results$macro$urate_2026, unit = 'pp') %>%
      add_row(metric = 'payroll_2025_q4', value = results$macro$payroll_2025, unit = 'thousands') %>%
      add_row(metric = 'payroll_2026_q4', value = results$macro$payroll_2026, unit = 'thousands')
  }

  # Distribution
  if (!is.null(results$distribution)) {
    key_results <- key_results %>%
      add_row(metric = 'avg_per_hh_cost_pre_sub', value = abs(results$distribution$pre_sub$avg_per_hh_cost), unit = 'dollars') %>%
      add_row(metric = 'avg_per_hh_cost_post_sub', value = abs(results$distribution$post_sub$avg_per_hh_cost), unit = 'dollars') %>%
      add_row(metric = 'regressivity_ratio', value = results$distribution$regressivity$burden_ratio, unit = 'ratio')
  }

  # Products
  if (!is.null(results$products)) {
    key_results <- key_results %>%
      add_row(metric = 'food_price_sr', value = results$products$food_sr, unit = 'pct') %>%
      add_row(metric = 'food_price_lr', value = results$products$food_lr, unit = 'pct')
  }

  write_csv(key_results, file.path(output_dir, 'key_results.csv'))
  message(sprintf('    key_results.csv (%d metrics)', nrow(key_results)))

  # ============================
  # Revenue by Year
  # ============================

  if (!is.null(results$revenue$by_year)) {
    write_csv(results$revenue$by_year, file.path(output_dir, 'revenue_by_year.csv'))
    message(sprintf('    revenue_by_year.csv (%d years)', nrow(results$revenue$by_year)))
  }

  # ============================
  # Dynamic Revenue by Year
  # ============================

  if (!is.null(results$dynamic) && !is.null(results$dynamic$by_year)) {
    write_csv(results$dynamic$by_year, file.path(output_dir, 'dynamic_revenue_by_year.csv'))
    message(sprintf('    dynamic_revenue_by_year.csv (%d years)', nrow(results$dynamic$by_year)))
  }

  # ============================
  # Macro Effects (Quarterly)
  # ============================

  if (!is.null(results$macro) && !is.null(results$macro$quarterly)) {
    write_csv(results$macro$quarterly, file.path(output_dir, 'macro_quarterly.csv'))
    message(sprintf('    macro_quarterly.csv (%d quarters)', nrow(results$macro$quarterly)))
  }

  # ============================
  # Sector Effects
  # ============================

  if (!is.null(results$sectors)) {
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
  }

  # ============================
  # Foreign GDP Effects
  # ============================

  if (!is.null(results$foreign_gdp)) {
    foreign_gdp <- tibble(
      region = c('usa', 'china', 'canada', 'mexico', 'eu', 'uk', 'japan', 'fta', 'row'),
      gdp_change_pct = c(
        results$foreign_gdp$usa,
        results$foreign_gdp$china,
        results$foreign_gdp$canada,
        results$foreign_gdp$mexico,
        results$foreign_gdp$eu,
        results$foreign_gdp$uk,
        results$foreign_gdp$japan,
        results$foreign_gdp$fta,
        results$foreign_gdp$row
      )
    )
    write_csv(foreign_gdp, file.path(output_dir, 'foreign_gdp.csv'))
    message('    foreign_gdp.csv (9 regions)')
  }

  # ============================
  # Distribution by Decile
  # ============================

  if (!is.null(results$distribution)) {
    # Pre-substitution
    write_csv(results$distribution$pre_sub$by_decile,
              file.path(output_dir, 'distribution_pre_sub.csv'))
    message('    distribution_pre_sub.csv (10 deciles)')

    # Post-substitution
    write_csv(results$distribution$post_sub$by_decile,
              file.path(output_dir, 'distribution_post_sub.csv'))
    message('    distribution_post_sub.csv (10 deciles)')
  }

  # ============================
  # Product Prices
  # ============================

  if (!is.null(results$products) && !is.null(results$products$products)) {
    write_csv(results$products$products, file.path(output_dir, 'product_prices.csv'))
    message(sprintf('    product_prices.csv (%d products)', nrow(results$products$products)))
  }

  message(sprintf('  Done. All outputs written to %s/', output_dir))

  invisible(output_dir)
}
