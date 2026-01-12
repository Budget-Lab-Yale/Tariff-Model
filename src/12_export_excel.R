# =============================================================================
# 12_export_excel.R - Export model results to Excel data download
# =============================================================================
#
# Exports model outputs to an Excel workbook matching the structure of
# reports/data_download_template.xlsx for the "State of Tariffs" report.
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
})


# =============================================================================
# Historical ETR data (1790-2024)
# =============================================================================

HISTORICAL_ETR <- tibble(
  year = 1790:2024,
  etr = c(
    12.5, 10, 15.63, 22.58, 30, 17.46, 22.81, 24, 31.58, 44.44,  # 1790-1799
    29.63, 29.85, 34.88, 26.92, 41.18, 33.33, 34.21, 31.76, 24.44, 30,  # 1800-1809
    26.15, 23.81, 21.43, 36.84, 30.77, 48.1, 24.63, 26.83, 24.07, 28.38,  # 1810-1819
    29.82, 43.21, 35.23, 43.69, 47.39, 47.72, 45.28, 50.93, 44.74, 50.73,  # 1820-1829
    57.32, 44.23, 38.97, 28.99, 21.83, 21.25, 19.51, 16.05, 23.11, 17.57,  # 1830-1839
    17.6, 17.37, 18.96, 20.13, 30.5, 29.34, 27.7, 24.2, 23.49, 23.41,  # 1840-1849
    24.5, 24.26, 24.35, 23.37, 23.52, 23.36, 21.68, 19.09, 17.33, 15.43,  # 1850-1859
    15.67, 14.21, 26.08, 28.28, 32.04, 38.46, 41.81, 44.56, 46.56, 44.76,  # 1860-1869
    44.89, 40.51, 37.99, 27.9, 28.29, 29.36, 31.25, 29.2, 29, 30.33,  # 1870-1879
    29.12, 29.79, 30.16, 30.04, 28.5, 30.75, 30.35, 31.52, 30.55, 30.02,  # 1880-1889
    29.59, 25.65, 21.65, 23.91, 20.56, 20.44, 20.67, 21.89, 24.77, 29.48,  # 1890-1899
    27.62, 28.91, 27.95, 27.85, 26.29, 23.77, 24.22, 23.28, 23.88, 22.99,  # 1900-1909
    21.11, 20.29, 18.58, 17.69, 14.88, 12.49, 9.08, 7.01, 5.79, 6.2,  # 1910-1919
    6.38, 11.44, 14.68, 15.18, 14.89, 13.21, 13.39, 13.81, 13.3, 13.48,  # 1920-1929
    14.83, 17.75, 19.59, 19.8, 18.41, 17.52, 16.84, 15.63, 15.46, 14.41,  # 1930-1939
    12.51, 13.59, 11.51, 11.57, 9.45, 9.29, 9.9, 7.55, 5.71, 5.53,  # 1940-1949
    5.97, 5.47, 5.3, 5.42, 5.17, 5.59, 5.67, 5.76, 6.44, 7.02,  # 1950-1959
    7.4, 7.21, 7.5, 7.29, 7.2, 7.72, 7.57, 7.54, 7.08, 7.11,  # 1960-1969
    6.5, 6.08, 5.65, 5.24, 3.77, 3.92, 3.86, 3.73, 4.14, 3.5,  # 1970-1979
    3.09, 3.43, 3.437, 3.333, 3.54, 3.542, 3.701, 3.723, 3.624, 3.543,  # 1980-1989
    3.28, 3.232, 3.238, 3.218, 3.035, 2.468, 2.232, 2.069, 1.975, 1.781,  # 1990-1999
    1.59, 1.65, 1.556, 1.547, 1.438, 1.375, 1.312, 1.345, 1.273, 1.353,  # 2000-2009
    1.374, 1.337, 1.338, 1.421, 1.436, 1.529, 1.552, 1.486, 1.941, 2.944,  # 2010-2019
    2.845, 3.011, 3.005, 2.487, 2.418  # 2020-2024
  )
)


# =============================================================================
# GTAP sector name mapping
# =============================================================================

GTAP_SECTOR_NAMES <- tribble(
  ~gtap_sector, ~display_name,
  'pdr', 'Paddy rice',
  'wht', 'Wheat',
  'gro', 'Cereal grains nec',
  'v_f', 'Vegetables, fruit, nuts',
  'osd', 'Oil seeds',
  'c_b', 'Sugar cane, sugar beet',
  'pfb', 'Plant-based fibers',
  'ocr', 'Crops nec',
  'ctl', 'Bovine cattle, sheep and goats',
  'oap', 'Animal products nec',
  'rmk', 'Raw milk',
  'wol', 'Wool, silk-worm cocoons',
  'frs', 'Forestry',
  'fsh', 'Fishing',
  'coa', 'Coal',
  'oil', 'Oil',
  'gas', 'Gas',
  'oxt', 'Minerals nec',
  'cmt', 'Bovine meat products',
  'omt', 'Meat products nec',
  'vol', 'Vegetable oils and fats',
  'mil', 'Dairy products',
  'pcr', 'Processed rice',
  'sgr', 'Sugar',
  'ofd', 'Food products nec',
  'b_t', 'Beverages and tobacco products',
  'tex', 'Textiles',
  'wap', 'Wearing apparel',
  'lea', 'Leather products',
  'lum', 'Wood products',
  'ppp', 'Paper products, publishing',
  'p_c', 'Petroleum, coal products',
  'chm', 'Chemical products',
  'bph', 'Basic pharmaceutical products',
  'rpp', 'Rubber and plastic products',
  'nmm', 'Mineral products nec',
  'i_s', 'Ferrous metals',
  'nfm', 'Metals nec',
  'fmp', 'Metal products',
  'ele', 'Computer, electronic and optic',
  'eeq', 'Electrical equipment',
  'ome', 'Machinery and equipment nec',
  'mvh', 'Motor vehicles and parts',
  'otn', 'Transport equipment nec',
  'omf', 'Manufactures nec',
  'ely', 'Electricity',
  'gdt', 'Gas manufacture, distribution',
  'wtr', 'Water',
  'cns', 'Construction',
  'trd', 'Trade',
  'afs', 'Accommodation, Food and servic',
  'otp', 'Transport nec',
  'wtp', 'Water transport',
  'atp', 'Air transport',
  'whs', 'Warehousing and support activi',
  'cmn', 'Communication',
  'ofi', 'Financial services nec',
  'ins', 'Insurance',
  'rsa', 'Real estate activities',
  'obs', 'Business services nec',
  'ros', 'Recreational and other service',
  'osg', 'Public Administration and defe',
  'edu', 'Education',
  'hht', 'Human health and social work a',
  'dwe', 'Dwellings'
)


# =============================================================================
# Sector display name mapping
# =============================================================================

SECTOR_NAMES <- tribble(
  ~sector, ~display_name,
  'agriculture', 'Agriculture',
  'mining', 'Mining & Extraction',
  'manufacturing', 'Total Manufacturing',
  'durable', 'Durable Manufacturing',
  'nondurable', 'Nondurable Manufacturing',
  'advanced', 'Advanced Manufacturing',
  'utilities', 'Utilities',
  'construction', 'Construction',
  'services', 'Services',
  'overall_gdp', 'Overall Real GDP'
)


# =============================================================================
# Region display name mapping
# =============================================================================

REGION_NAMES <- tribble(
  ~region, ~display_name,
  'usa', 'USA',
  'china', 'China',
  'canada', 'Canada',
  'mexico', 'Mexico',
  'eu', 'EU',
  'uk', 'UK',
  'japan', 'Japan',
  'fta', 'FTROW',
  'row', 'ROW',
  'world', 'World Total',
  'world_ex_usa', 'World ex USA'
)


# =============================================================================
# Helper functions
# =============================================================================

#' Load model outputs from CSV files
#' @param scenario Scenario name
#' @return List of data frames
load_model_outputs <- function(scenario) {
  results_dir <- file.path('output', scenario, 'results')

  if (!dir.exists(results_dir)) {
    stop('Results directory not found: ', results_dir)
  }

  outputs <- list()

  # Required files
  required_files <- c(
    'key_results.csv',
    'goods_weighted_etrs.csv',
    'revenue_by_year.csv',
    'dynamic_revenue_by_year.csv',
    'macro_quarterly.csv',
    'sector_effects.csv',
    'foreign_gdp.csv',
    'distribution.csv',
    'product_prices.csv'
  )

  for (f in required_files) {
    path <- file.path(results_dir, f)
    if (!file.exists(path)) {
      stop('Required file not found: ', path)
    }
    name <- tools::file_path_sans_ext(f)
    outputs[[name]] <- read_csv(path, show_col_types = FALSE)
  }

  return(outputs)
}


# =============================================================================
# Constants
# =============================================================================

BASELINE_ETR <- 0.02418  # 2.418% baseline ETR


# =============================================================================
# Data TOC: Table of Contents sheet
# =============================================================================

build_data_toc <- function(report_date) {
  report_date_obj <- as.Date(report_date, format = '%B %d, %Y')
  if (is.na(report_date_obj)) {
    stop('Invalid report_date format. Expected "Month DD, YYYY".')
  }

  date_md <- format(report_date_obj, '%B %d')
  date_my <- format(report_date_obj, '%B %Y')

  # Create a data frame for the TOC content
  # The hyperlinks will be added separately when writing to Excel
  tibble(
    row = 1:14,
    content = c(
      sprintf('State of U.S. Tariffs: %s', report_date),
      date_my,
      'The Budget Lab at Yale',
      '',
      'Tables and Figures',
      sprintf('Table 1. Summary Economic & Fiscal Effects of 2025 Tariffs through %s', date_md),
      sprintf('Table 2. Average Effective US Tariff Rate, New 2025 Policy through %s', date_md),
      'Figure 1. US Average Effective Tariff Rate Since 1790',
      'Figure 2. US Real GDP Level Effects of 2025 Tariffs to Date',
      'Figure 3. Change in Long-Run Real US GDP by Sector from 2025 Tariffs',
      'Figure 4. Long-Run Change in Real GDP Level from 2025 Tariffs to Date',
      'Table 3. Estimated Revenue Effects of All 2025 Tariffs',
      'Figure 5. Short-Run Distributional Effects of 2025 Tariffs',
      sprintf('Figure 6. Commodity Price Effects from 2025 Tariffs through %s', date_md)
    )
  )
}


# =============================================================================
# T1: Summary Table
# =============================================================================

build_t1 <- function(outputs) {
  # Convert key_results to named list for easy access
  key <- outputs$key_results %>%
    select(metric, value) %>%
    deframe()

  # Get long-run GDP from sector_effects
  lr_gdp <- outputs$sector_effects %>%
    filter(sector == 'overall_gdp') %>%
    pull(output_change_pct)

  # ETR LEVELS = baseline + increase (not just the increase)
  pre_sub_etr_level <- BASELINE_ETR + key['pre_sub_etr_increase'] / 100
  post_sub_etr_level <- BASELINE_ETR + key['post_sub_etr_increase'] / 100

  tibble(
    value = c(
      NA,  # Section header
      pre_sub_etr_level,  # LEVEL (baseline + increase)
      post_sub_etr_level,  # LEVEL (baseline + increase)
      NA,  # Section header
      key['conventional_revenue_10yr'] / 1000,  # Billions to trillions
      key['dynamic_revenue_10yr'] / 1000,
      NA,  # Section header
      key['pre_sub_price_increase'] / 100,
      key['post_sub_price_increase'] / 100,
      key['pre_sub_per_hh_cost'],
      key['post_sub_per_hh_cost'],
      NA,  # Section header
      key['gdp_2025_q4q4'],  # Percentage points
      key['gdp_2026_q4q4'],  # Percentage points
      lr_gdp / 100,
      key['urate_2026_q4']  # Already in percentage points
    )
  )
}


# =============================================================================
# T2: ETR by Region
# =============================================================================

build_t2 <- function(outputs) {
  etrs <- outputs$goods_weighted_etrs

  # Get total imports for share calculation
  total_row <- etrs %>% filter(country_code == 'all')
  total_presub_imports <- total_row$presub_imports
  total_postsub_imports <- total_row$postsub_imports

  # Aggregate UK, Japan, EU, FTA, ROW into "Rest of World"
  row_countries <- c('uk', 'jp', 'eu', 'row', 'fta')

  # Filter ROW countries
  row_data <- etrs %>% filter(country_code %in% row_countries)

  # Debug: show what we're aggregating
  message(sprintf('    ROW aggregation: %d countries', nrow(row_data)))
  message(sprintf('    Sum presub_imports: %.0f', sum(row_data$presub_imports)))
  message(sprintf('    Weighted ETR numerator: %.0f', sum(row_data$presub_etr * row_data$presub_imports)))
  message(sprintf('    Weighted ETR: %.4f', sum(row_data$presub_etr * row_data$presub_imports) / sum(row_data$presub_imports)))

  # Calculate weighted averages BEFORE summing (dplyr summarise uses new values in sequence)
  row_agg <- row_data %>%
    summarise(
      country = 'Rest of World',
      country_code = 'row_agg',
      # Calculate ETRs first using original column values
      postsub_etr = sum(postsub_etr * postsub_imports) / sum(postsub_imports),
      presub_etr = sum(presub_etr * presub_imports) / sum(presub_imports),
      # Then sum imports (order matters in dplyr 1.0+!)
      postsub_imports = sum(postsub_imports),
      presub_imports = sum(presub_imports)
    )

  # Keep China, Canada, Mexico
  main_countries <- etrs %>%
    filter(country_code %in% c('chn', 'ca', 'mx')) %>%
    mutate(country = case_when(
      country_code == 'chn' ~ 'China',
      country_code == 'ca' ~ 'Canada',
      country_code == 'mx' ~ 'Mexico'
    ))

  # Combine and add Total
  combined <- bind_rows(main_countries, row_agg) %>%
    bind_rows(total_row %>% mutate(country = 'Total', country_code = 'total'))

  # Calculate shares and contributions
  result <- combined %>%
    mutate(
      share_presub = presub_imports / total_presub_imports,
      share_postsub = postsub_imports / total_postsub_imports,
      contrib_presub = presub_etr * share_presub,
      contrib_postsub = postsub_etr * share_postsub
    ) %>%
    select(
      Region = country,
      `Avg ETR Pre-Sub` = presub_etr,
      `Avg ETR Post-Sub` = postsub_etr,
      `Share Pre-Sub` = share_presub,
      `Share Post-Sub` = share_postsub,
      `Contrib Pre-Sub` = contrib_presub,
      `Contrib Post-Sub` = contrib_postsub
    )

  return(result)
}


# =============================================================================
# T3: Revenue Table
# =============================================================================

build_t3 <- function(outputs) {
  conventional <- outputs$revenue_by_year %>%
    filter(fiscal_year >= 2026) %>%
    select(fiscal_year, net_revenue)

  dynamic <- outputs$dynamic_revenue_by_year %>%
    filter(fiscal_year >= 2026) %>%
    select(fiscal_year, dynamic_revenue, dynamic_effect)

  # Join and pivot
  revenue <- conventional %>%
    left_join(dynamic, by = 'fiscal_year')

  # Calculate 10-year totals
  totals <- tibble(
    fiscal_year = '2026-35',
    net_revenue = sum(revenue$net_revenue),
    dynamic_revenue = sum(revenue$dynamic_revenue),
    dynamic_effect = sum(revenue$dynamic_effect)
  )

  # Pivot to wide format
  revenue_wide <- revenue %>%
    mutate(fiscal_year = as.character(fiscal_year)) %>%
    bind_rows(totals)

  # Create output table
  years <- c(as.character(2026:2035), '2026-35')

  result <- tibble(
    Metric = c('Conventional', 'Dynamic', 'Dynamic effect')
  )

  for (i in seq_along(years)) {
    result[[years[i]]] <- c(
      revenue_wide$net_revenue[i],
      revenue_wide$dynamic_revenue[i],
      revenue_wide$dynamic_effect[i]
    )
  }

  return(result)
}


# =============================================================================
# F1: Historical ETR Chart
# =============================================================================

build_f1 <- function(outputs) {
  key <- outputs$key_results %>%
    select(metric, value) %>%
    deframe()

  # ETR LEVELS = baseline + increase (convert increase from % to decimal first)
  pre_sub_etr_level <- (BASELINE_ETR + key['pre_sub_etr_increase'] / 100) * 100  # Back to %
  post_sub_etr_level <- (BASELINE_ETR + key['post_sub_etr_increase'] / 100) * 100  # Back to %

  last_year <- max(HISTORICAL_ETR$year)
  last_etr <- HISTORICAL_ETR %>%
    filter(year == last_year) %>%
    pull(etr)

  # Start with historical data
  result <- HISTORICAL_ETR %>%
    rename(`Effective Tariff Rate` = etr) %>%
    mutate(
      `Projected Post-Substitution Rate` = NA_real_,
      `Current Post-Substitution Rate` = post_sub_etr_level,
      `Projected Pre-Substitution Rate` = NA_real_,
      `Current Pre-Substituton Rate` = pre_sub_etr_level
    ) %>%
    mutate(
      `Projected Post-Substitution Rate` = if_else(
        year == last_year, last_etr, `Projected Post-Substitution Rate`
      ),
      `Projected Pre-Substitution Rate` = if_else(
        year == last_year, last_etr, `Projected Pre-Substitution Rate`
      )
    )

  # Add 2025 row with scenario LEVEL values
  row_2025 <- tibble(
    year = last_year + 1,
    `Effective Tariff Rate` = NA_real_,
    `Projected Post-Substitution Rate` = post_sub_etr_level,
    `Current Post-Substitution Rate` = post_sub_etr_level,
    `Projected Pre-Substitution Rate` = pre_sub_etr_level,
    `Current Pre-Substituton Rate` = pre_sub_etr_level
  )

  result <- bind_rows(result, row_2025)

  return(result)
}


# =============================================================================
# F2: GDP Level Effects (Blended MAUS-GTAP approach)
# =============================================================================

build_f2 <- function(outputs) {
  macro <- outputs$macro_quarterly

  # Get GTAP long-run US GDP effect for blending
  gtap_lr_gdp <- outputs$foreign_gdp %>%
    filter(region == 'usa') %>%
    pull(gdp_change_pct)

  # Calculate GDP deviation as percentage with GTAP floor for 2026+
  # This matches the "blended" approach: MIN(maus_deviation, gtap_long_run) for 2026 Q1+
  result <- macro %>%
    filter(year >= 2025) %>%
    mutate(
      Date = as.Date(sprintf('%d-%02d-15', year, quarter * 3)),  # Mid-quarter date
      raw_deviation = (gdp_tariff - gdp_baseline) / gdp_baseline * 100,
      # Apply GTAP floor for 2026+: use MIN (more negative = worse)
      `All 2025 Tariffs to Date` = case_when(
        year < 2026 ~ raw_deviation,
        TRUE ~ pmin(raw_deviation, gtap_lr_gdp)
      )
    ) %>%
    select(
      Date,
      `All 2025 Tariffs to Date`
    )

  return(result)
}


# =============================================================================
# F3: Sector Output Changes
# =============================================================================

build_f3 <- function(outputs) {
  sectors <- outputs$sector_effects

  # Map to display names and order
  result <- sectors %>%
    left_join(SECTOR_NAMES, by = 'sector') %>%
    mutate(
      Sector = coalesce(display_name, sector),
      `All 2025 Tariffs to Date` = output_change_pct
    ) %>%
    select(Sector, `All 2025 Tariffs to Date`)

  # Order sectors as in template
  sector_order <- c(
    'Agriculture', 'Mining & Extraction', 'Total Manufacturing',
    'Durable Manufacturing', 'Advanced Manufacturing', 'Nondurable Manufacturing',
    'Utilities', 'Construction', 'Services', 'Overall Real GDP'
  )

  result <- result %>%
    mutate(Sector = factor(Sector, levels = sector_order)) %>%
    arrange(Sector) %>%
    mutate(Sector = as.character(Sector))

  return(result)
}


# =============================================================================
# F4: International GDP Effects
# =============================================================================

build_f4 <- function(outputs) {
  gdp <- outputs$foreign_gdp

  # World and World ex USA now come from CSV (GDP-weighted in 08_calculate_foreign_gdp.R)
  result <- gdp %>%
    left_join(REGION_NAMES, by = 'region') %>%
    mutate(
      Region = coalesce(display_name, region),
      `All 2025 Tariffs to Date` = gdp_change_pct
    ) %>%
    select(Region, `All 2025 Tariffs to Date`)

  # Order as in template
  region_order <- c('USA', 'China', 'ROW', 'Canada', 'Mexico', 'FTROW', 'Japan', 'EU', 'UK', 'World Total', 'World ex USA')

  result <- result %>%
    filter(Region %in% region_order) %>%
    mutate(Region = factor(Region, levels = region_order)) %>%
    arrange(Region) %>%
    mutate(Region = as.character(Region))

  return(result)
}


# =============================================================================
# F5: Distribution by Decile
# =============================================================================

build_f5 <- function(outputs) {
  dist <- outputs$distribution

  dist <- dist %>%
    arrange(decile)

  # Percent should be NEGATIVE (burden/cost)
  pct <- dist %>%
    mutate(pct_of_income = -abs(pct_of_income)) %>%
    pull(pct_of_income)

  cost <- dist %>%
    pull(cost_per_hh)

  return(list(pct = pct, cost = cost))
}


# =============================================================================
# F6: Commodity Price Effects
# =============================================================================

build_f6 <- function(outputs) {
  prices <- outputs$product_prices

  result <- prices %>%
    left_join(GTAP_SECTOR_NAMES, by = 'gtap_sector') %>%
    mutate(
      Name = coalesce(display_name, gtap_sector)
    ) %>%
    select(
      Name,
      `Short-Run` = sr_price_effect,
      `Long-Run` = lr_price_effect
    ) %>%
    arrange(desc(`Short-Run`))  # Sort by short-run effect descending

  return(result)
}


# =============================================================================
# Main export function
# =============================================================================

#' Export model results to Excel data download
#'
#' Creates an Excel workbook matching the structure of the data download
#' template for the State of Tariffs report.
#'
#' @param scenario Name of the scenario
#' @param report_date Date string for the report (e.g., 'November 17, 2025')
#' @return Path to the exported Excel file (invisibly)
#'
#' @export
export_excel_tables <- function(scenario, report_date) {
  message('Exporting Excel data download...')

  # Load outputs
  outputs <- load_model_outputs(scenario)

  report_date_obj <- as.Date(report_date, format = '%B %d, %Y')
  if (is.na(report_date_obj)) {
    stop('Invalid report_date format. Expected "Month DD, YYYY".')
  }

  date_md <- format(report_date_obj, '%B %d')
  date_my <- format(report_date_obj, '%B %Y')

  template_path <- file.path('reports', 'data_download_template.xlsx')
  if (!file.exists(template_path)) {
    stop('Template not found: ', template_path)
  }

  suppress_openxlsx_external_link_warning <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl('externalLink', msg, fixed = TRUE) ||
            grepl('one argument not used by format', msg, fixed = TRUE)) {
          invokeRestart('muffleWarning')
        }
      }
    )
  }

  apply_white_background <- function(sheet, max_rows = 500, max_cols = 50) {
    white_style <- createStyle(fgFill = 'white')
    addStyle(
      wb,
      sheet = sheet,
      style = white_style,
      rows = 1:max_rows,
      cols = 1:max_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Load template workbook to preserve formatting
  wb <- suppress_openxlsx_external_link_warning(loadWorkbook(template_path))

  write_block <- function(sheet, data, start_row, start_col) {
    if (is.null(dim(data))) {
      data <- as.data.frame(data)
    }
    if (nrow(data) == 0 || ncol(data) == 0) {
      return(invisible(NULL))
    }

    deleteData(
      wb,
      sheet = sheet,
      cols = start_col:(start_col + ncol(data) - 1),
      rows = start_row:(start_row + nrow(data) - 1),
      gridExpand = TRUE
    )

    writeData(
      wb,
      sheet = sheet,
      x = data,
      startCol = start_col,
      startRow = start_row,
      colNames = FALSE
    )
  }

  # ==========================================================================
  # Sheet order matches template: Data TOC, T1, T2, F1, F2, F3, F4, T3, F5, F6
  # ==========================================================================

  # Data TOC (Table of Contents)
  message('  Building Data TOC...')
  toc <- build_data_toc(report_date)
  writeData(wb, 'Data TOC', toc$content[1], startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, 'Data TOC', date_my, startCol = 1, startRow = 2, colNames = FALSE)
  writeData(wb, 'Data TOC', toc$content[3], startCol = 1, startRow = 3, colNames = FALSE)
  writeData(wb, 'Data TOC', toc$content[5], startCol = 1, startRow = 5, colNames = FALSE)

  # Add hyperlinks to TOC entries
  toc_links <- list(
    list(row = 6, sheet = 'T1', label = toc$content[6]),
    list(row = 7, sheet = 'T2', label = toc$content[7]),
    list(row = 8, sheet = 'F1', label = toc$content[8]),
    list(row = 9, sheet = 'F2', label = toc$content[9]),
    list(row = 10, sheet = 'F3', label = toc$content[10]),
    list(row = 11, sheet = 'F4', label = toc$content[11]),
    list(row = 12, sheet = 'T3', label = toc$content[12]),
    list(row = 13, sheet = 'F5', label = toc$content[13]),
    list(row = 14, sheet = 'F6', label = toc$content[14])
  )

  suppress_openxlsx_external_link_warning({
    for (link in toc_links) {
      writeFormula(
        wb, 'Data TOC',
        x = makeHyperlinkString(sheet = link$sheet, row = 1, col = 1, text = link$label),
        startCol = 1, startRow = link$row
      )
    }
  })

  # T1: Summary Table
  message('  Building T1 (Summary)...')
  t1 <- build_t1(outputs)
  writeData(
    wb,
    'T1',
    sprintf('Table 1. Summary Economic & Fiscal Effects of 2025 Tariffs through %s', date_md),
    startCol = 1,
    startRow = 1,
    colNames = FALSE
  )
  writeData(
    wb,
    'T1',
    sprintf('Summary Economic & Fiscal Effects of 2025 Tariffs Through %s', date_md),
    startCol = 1,
    startRow = 4,
    colNames = FALSE
  )
  write_block('T1', t1, start_row = 6, start_col = 2)
  deleteData(
    wb,
    sheet = 'T1',
    cols = 3,
    rows = 5:(6 + nrow(t1) - 1),
    gridExpand = TRUE
  )
  writeData(wb, 'T1', '', startCol = 3, startRow = 5, colNames = FALSE)
  setColWidths(wb, 'T1', cols = 3, widths = 0)

  t1_pct_style <- createStyle(numFmt = '0.0%')
  t1_pct2_style <- createStyle(numFmt = '0.00%')
  t1_currency1_style <- createStyle(numFmt = '"$"#,##0.0')
  t1_currency0_style <- createStyle(numFmt = '"$"#,##0')
  t1_decimal1_style <- createStyle(numFmt = '0.0')

  addStyle(wb, 'T1', t1_pct_style, rows = c(7, 8, 13, 14), cols = 2, stack = TRUE)
  addStyle(wb, 'T1', t1_pct2_style, rows = 20, cols = 2, stack = TRUE)
  addStyle(wb, 'T1', t1_currency1_style, rows = c(10, 11), cols = 2, stack = TRUE)
  addStyle(wb, 'T1', t1_currency0_style, rows = c(15, 16), cols = 2, stack = TRUE)
  addStyle(wb, 'T1', t1_decimal1_style, rows = c(18, 19, 21), cols = 2, stack = TRUE)

  # T2: ETR by Region
  message('  Building T2 (ETR by Region)...')
  t2 <- build_t2(outputs)
  writeData(
    wb,
    'T2',
    sprintf('Table 2. Average Effective US Tariff Rate, New 2025 Policy through %s', date_md),
    startCol = 1,
    startRow = 1,
    colNames = FALSE
  )
  write_block('T2', t2, start_row = 8, start_col = 1)

  # F1: Historical ETR
  message('  Building F1 (Historical ETR)...')
  f1 <- build_f1(outputs)
  write_block('F1', f1, start_row = 6, start_col = 1)

  # F2: GDP Level Effects
  message('  Building F2 (GDP Effects)...')
  f2 <- build_f2(outputs)
  writeData(
    wb,
    'F2',
    sprintf(
      'Subtitle: US tariffs implemented through %s. Percentage point change against baseline',
      date_md
    ),
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  )
  write_block('F2', f2, start_row = 6, start_col = 1)

  # F3: Sector Output Changes
  message('  Building F3 (Sectors)...')
  f3 <- build_f3(outputs)
  writeData(
    wb,
    'F3',
    sprintf(
      'Subtitle: U.S. tariffs implemented through %s, plus foreign retaliation. Percentage points.',
      date_md
    ),
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  )
  write_block('F3', f3, start_row = 7, start_col = 1)

  # F4: International GDP Effects
  message('  Building F4 (International GDP)...')
  f4 <- build_f4(outputs)
  writeData(
    wb,
    'F4',
    sprintf(
      'Subtitle: U.S. tariffs implemented through %s. Percentage point change',
      date_md
    ),
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  )
  write_block('F4', f4, start_row = 7, start_col = 1)

  # T3: Revenue Table
  message('  Building T3 (Revenue)...')
  t3 <- build_t3(outputs)
  writeData(
    wb,
    'T3',
    sprintf('Subtitle: Through %s.', date_md),
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  )
  write_block('T3', t3, start_row = 6, start_col = 1)

  # F5: Distribution by Decile
  message('  Building F5 (Distribution)...')
  f5 <- build_f5(outputs)
  if (length(f5$pct) != 10 || length(f5$cost) != 10) {
    stop('Expected 10 deciles for distribution output.')
  }
  writeData(
    wb,
    'F5',
    sprintf('Subtitle: Through %s. By household income decile', date_md),
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  )
  pct_row <- as.data.frame(as.list(f5$pct))
  cost_row <- as.data.frame(as.list(f5$cost))
  write_block('F5', pct_row, start_row = 8, start_col = 2)
  write_block('F5', cost_row, start_row = 12, start_col = 2)

  # F6: Commodity Price Effects
  message('  Building F6 (Commodities)...')
  f6 <- build_f6(outputs)
  writeData(
    wb,
    'F6',
    sprintf('Figure 6. Commodity Price Effects from 2025 Tariffs through %s', date_md),
    startCol = 1,
    startRow = 1,
    colNames = FALSE
  )
  write_block('F6', f6, start_row = 7, start_col = 1)

  # Save workbook
  output_dir <- file.path('output', scenario, 'report')
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_path <- file.path(output_dir, 'data_download.xlsx')

  for (sheet_name in c('Data TOC', 'T1', 'T2', 'F1', 'F2', 'F3', 'F4', 'T3', 'F5', 'F6')) {
    apply_white_background(sheet_name)
  }

  suppress_openxlsx_external_link_warning(saveWorkbook(wb, output_path, overwrite = TRUE))

  message(sprintf('  Exported to: %s', output_path))

  invisible(output_path)
}
