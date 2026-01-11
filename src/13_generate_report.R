# =============================================================================
# 13_generate_report.R - Generate State of Tariffs report via Claude API
# =============================================================================
#
# This script generates a Markdown report from model outputs using the Claude
# API, then converts it to Word format using Pandoc.
#
# Usage:
#   source('src/13_generate_report.R')
#   generate_report('11-17', '11-17-ex-ieepa')
#
# Requirements:
#   - ANTHROPIC_API_KEY environment variable set
#   - Pandoc installed (for Word conversion)
#   - httr2 and jsonlite packages
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(jsonlite)
})


#' Load model outputs for report generation
#'
#' @param scenario Name of the baseline scenario
#' @return List containing all results data
load_report_data <- function(scenario) {
  output_dir <- file.path('output', scenario, 'results')

  if (!dir.exists(output_dir)) {
    stop('Output directory not found: ', output_dir)
  }

  data <- list(
    key_results = read_csv(
      file.path(output_dir, 'key_results.csv'),
      show_col_types = FALSE
    ),
    macro_quarterly = read_csv(
      file.path(output_dir, 'macro_quarterly.csv'),
      show_col_types = FALSE
    ),
    sector_effects = read_csv(
      file.path(output_dir, 'sector_effects.csv'),
      show_col_types = FALSE
    ),
    distribution = read_csv(
      file.path(output_dir, 'distribution.csv'),
      show_col_types = FALSE
    ),
    dynamic_revenue = read_csv(
      file.path(output_dir, 'dynamic_revenue_by_year.csv'),
      show_col_types = FALSE
    ),
    product_prices = read_csv(
      file.path(output_dir, 'product_prices.csv'),
      show_col_types = FALSE
    ),
    foreign_gdp = read_csv(
      file.path(output_dir, 'foreign_gdp.csv'),
      show_col_types = FALSE
    )
  )

  return(data)
}


#' Convert results data to YAML for API prompt
#'
#' @param data Results data from load_report_data()
#' @param scenario_name Name of the scenario (for labeling)
#' @return YAML string representation of the data
results_to_yaml <- function(data, scenario_name) {
  # Convert key_results to named list
  key_results <- setNames(
    as.list(data$key_results$value),
    data$key_results$metric
  )

  # Convert sector_effects to named list
  sector_effects <- setNames(
    as.list(data$sector_effects$output_change_pct),
    data$sector_effects$sector
  )

  # Get full distribution by decile
  distribution <- list(
    by_decile = lapply(1:10, function(i) {
      row <- data$distribution[i, ]
      list(
        decile = row$decile,
        income = round(row$income),
        pct_of_income = round(row$pct_of_income, 2),
        cost_per_hh = round(abs(row$cost_per_hh))
      )
    }),
    bottom_decile_pct = round(data$distribution$pct_of_income[1], 2),
    bottom_decile_cost = round(abs(data$distribution$cost_per_hh[1])),
    top_decile_pct = round(data$distribution$pct_of_income[10], 2),
    top_decile_cost = round(abs(data$distribution$cost_per_hh[10])),
    median_cost = round(abs(data$distribution$cost_per_hh[5]))  # Decile 5 as proxy
  )

  # Get top price impact products (top 15 for more coverage)
  top_products <- data$product_prices %>%
    arrange(desc(sr_price_effect)) %>%
    head(15) %>%
    mutate(
      sr_price_effect = round(sr_price_effect, 2),
      lr_price_effect = round(lr_price_effect, 2)
    ) %>%
    select(gtap_sector, sr_price_effect, lr_price_effect)

  # Get food price effects
  food_prices <- data$product_prices %>%
    filter(is_food == 1) %>%
    summarise(
      sr_weighted = sum(sr_price_effect * weight) / sum(weight),
      lr_weighted = sum(lr_price_effect * weight) / sum(weight)
    )

  # Get key product prices for narrative
  key_products <- list(
    leather = data$product_prices %>% filter(gtap_sector == 'lea') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    apparel = data$product_prices %>% filter(gtap_sector == 'wap') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    textiles = data$product_prices %>% filter(gtap_sector == 'tex') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    electrical_equip = data$product_prices %>% filter(gtap_sector == 'ele') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    electronics = data$product_prices %>% filter(gtap_sector == 'eeq') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    motor_vehicles = data$product_prices %>% filter(gtap_sector == 'mvh') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    metal_products = data$product_prices %>% filter(gtap_sector == 'fmp') %>%
      select(sr_price_effect, lr_price_effect) %>% as.list(),
    food_sr = round(food_prices$sr_weighted, 2),
    food_lr = round(food_prices$lr_weighted, 2)
  )

  # Get 10-year revenue totals
  revenue_10yr <- list(
    gross = round(sum(data$dynamic_revenue$net_revenue), 1),
    dynamic_effect = round(sum(data$dynamic_revenue$dynamic_effect), 1),
    net_dynamic = round(sum(data$dynamic_revenue$dynamic_revenue), 1)
  )

  # Get foreign GDP effects
  foreign_gdp <- setNames(
    as.list(round(data$foreign_gdp$gdp_change_pct, 2)),
    data$foreign_gdp$region
  )

  # Build combined structure
  combined <- list(
    scenario = scenario_name,
    key_results = key_results,
    sector_effects = sector_effects,
    distribution = distribution,
    top_price_impact_products = as.list(top_products),
    key_product_prices = key_products,
    revenue_10yr = revenue_10yr,
    foreign_gdp = foreign_gdp
  )

  return(yaml::as.yaml(combined))
}


#' Build the prompt for Claude API
#'
#' @param baseline_yaml YAML string for baseline scenario
#' @param comparison_yaml YAML string for comparison scenario (e.g., ex-IEEPA)
#' @param report_date Date string for the report (e.g., "November 17, 2025")
#' @param policy_changes Description of policy changes since last report
#' @param previous_report_date Date of the previous report (for comparison)
#' @return Complete prompt string
build_prompt <- function(baseline_yaml,
                         comparison_yaml,
                         report_date,
                         policy_changes,
                         previous_report_date = NULL) {

  # Load instructions and reference
  instructions <- readLines('reports/report_instructions.md', warn = FALSE) %>%
    paste(collapse = '\n')

  reference <- readLines('reports/reference_report.md', warn = FALSE) %>%
    paste(collapse = '\n')

  # Build comparison section if comparison data provided
  if (!is.null(comparison_yaml)) {
    comparison_section <- sprintf('
### Comparison Scenario (IEEPA tariffs invalidated)

```yaml
%s
```', comparison_yaml)

    task_instructions <- '
Important:
1. Use the baseline scenario numbers as the primary focus
2. Include IEEPA invalidation numbers in italicized parentheticals
3. Write the "Changes Since Last Report" section based on the policy changes provided
4. Follow the style and structure of the reference report exactly
5. Round numbers appropriately (see instructions)
6. Ensure internal consistency (same number appears the same everywhere)'

  } else {
    comparison_section <- '
(No comparison scenario provided - skip all IEEPA invalidation parentheticals)'

    task_instructions <- '
Important:
1. Use the baseline scenario numbers throughout
2. DO NOT include any IEEPA invalidation parentheticals or comparisons
3. Remove all italicized parentheticals that reference IEEPA scenarios
4. Write the "Changes Since Last Report" section based on the policy changes provided
5. Follow the style and structure of the reference report, but omit IEEPA references
6. Round numbers appropriately (see instructions)
7. Ensure internal consistency (same number appears the same everywhere)'
  }

  prompt <- sprintf('
You are generating a Yale Budget Lab "State of Tariffs" report.

## Instructions

%s

## Reference Report (for style and structure)

%s

## Current Model Outputs

### Baseline Scenario (tariffs remain in effect)

```yaml
%s
```
%s

## Report Parameters

- **Report Date:** %s
- **Previous Report Date:** %s

## Policy Changes Since Last Report

%s

## Task

Generate a complete State of Tariffs report in Markdown format for the date specified above.
%s

Output ONLY the Markdown report, no additional commentary.
',
    instructions,
    reference,
    baseline_yaml,
    comparison_section,
    report_date,
    ifelse(is.null(previous_report_date), 'Not specified', previous_report_date),
    policy_changes,
    task_instructions
  )

  return(prompt)
}


#' Call Claude API to generate report
#'
#' @param prompt The complete prompt string
#' @param model Claude model to use (default: claude-sonnet-4-20250514)
#' @param max_tokens Maximum tokens in response
#' @return Generated report text
call_claude_api <- function(prompt,
                            model = 'claude-sonnet-4-20250514',
                            max_tokens = 8192) {

  api_key <- Sys.getenv('ANTHROPIC_API_KEY')
  if (api_key == '') {
    stop('ANTHROPIC_API_KEY environment variable not set.\n',
         'Get your API key from https://console.anthropic.com/')
  }

  message('  Calling Claude API (', model, ')...')

  response <- tryCatch({
    request('https://api.anthropic.com/v1/messages') %>%
      req_headers(
        'x-api-key' = api_key,
        'anthropic-version' = '2023-06-01',
        'content-type' = 'application/json'
      ) %>%
      req_body_json(list(
        model = model,
        max_tokens = max_tokens,
        messages = list(
          list(role = 'user', content = prompt)
        )
      )) %>%
      req_timeout(300) %>%  # 5 minute timeout for long generation
      req_perform()
  }, error = function(e) {
    # Try to extract error details
    msg <- conditionMessage(e)
    cat('\nAPI Error:', msg, '\n')
    stop(e)
  })

  # Parse response
  body <- resp_body_json(response)

  if (is.null(body$content) || length(body$content) == 0) {
    stop('Empty response from Claude API')
  }

  report_text <- body$content[[1]]$text

  # Log token usage
  if (!is.null(body$usage)) {
    message(sprintf('  Tokens used: %d input, %d output',
                    body$usage$input_tokens,
                    body$usage$output_tokens))
  }

  return(report_text)
}


#' Find Pandoc executable
#'
#' @return Path to pandoc executable, or NULL if not found
find_pandoc <- function() {
  # Check if pandoc is in PATH
  pandoc_in_path <- Sys.which('pandoc')
  if (pandoc_in_path != '') {
    return(pandoc_in_path)
  }

  # Check common Windows install locations
  common_paths <- c(
    'C:/Program Files/Pandoc/pandoc.exe',
    file.path(Sys.getenv('LOCALAPPDATA'), 'Pandoc/pandoc.exe'),
    file.path(Sys.getenv('APPDATA'), 'Pandoc/pandoc.exe')
  )

  for (p in common_paths) {
    if (file.exists(p)) {
      return(p)
    }
  }

  return(NULL)
}


#' Convert Markdown to Word using Pandoc
#'
#' @param md_path Path to Markdown file
#' @param docx_path Path for output Word file
#' @param reference_doc Path to reference document for styling (optional)
#' @return TRUE if successful
convert_to_word <- function(md_path, docx_path, reference_doc = NULL) {

  # Find Pandoc

  pandoc_path <- find_pandoc()
  if (is.null(pandoc_path)) {
    warning('Pandoc not found. Skipping Word conversion.\n',
            'Install from: https://pandoc.org/installing.html')
    return(FALSE)
  }

  # Build command
  cmd <- sprintf('"%s" "%s" -o "%s"', pandoc_path, md_path, docx_path)

  if (!is.null(reference_doc) && file.exists(reference_doc)) {
    cmd <- sprintf('%s --reference-doc="%s"', cmd, reference_doc)
  }

  message('  Converting to Word via Pandoc...')
  result <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  if (result != 0) {
    warning('Pandoc conversion failed')
    return(FALSE)
  }

  return(TRUE)
}


#' Generate State of Tariffs report
#'
#' Main function to generate the report from model outputs.
#'
#' @param baseline_scenario Name of the baseline scenario
#' @param comparison_scenario Name of the comparison scenario (e.g., ex-IEEPA).
#'   Set to NULL to skip IEEPA invalidation comparison.
#' @param report_date Date for the report (e.g., "November 17, 2025")
#' @param policy_changes Text describing policy changes since last report.
#'   If NULL, will prompt for input.
#' @param previous_report_date Date of the previous report (optional)
#' @param model Claude model to use
#' @param output_dir Directory for output files (default: output/{baseline}/report)
#'
#' @return Path to generated Word document
#'
#' @examples
#' generate_report(
#'   baseline_scenario = '11-17',
#'   comparison_scenario = '11-17-ex-ieepa',
#'   report_date = 'November 17, 2025',
#'   policy_changes = 'The administration expanded agricultural exemptions...',
#'   previous_report_date = 'October 30, 2025'
#' )
#'
#' # Without IEEPA comparison:
#' generate_report(
#'   baseline_scenario = '1-11_hypothetical',
#'   comparison_scenario = NULL,
#'   report_date = 'January 11, 2025',
#'   policy_changes = 'Hypothetical doubling of China fentanyl rate.'
#' )
generate_report <- function(baseline_scenario,
                            comparison_scenario = NULL,
                            report_date,
                            policy_changes = NULL,
                            previous_report_date = NULL,
                            model = 'claude-sonnet-4-20250514',
                            output_dir = NULL) {

  message('\n==========================================================')
  message('Generating State of Tariffs Report')
  message('==========================================================\n')

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- file.path('output', baseline_scenario, 'report')
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Load data for baseline scenario
  message('Loading model outputs...')
  baseline_data <- load_report_data(baseline_scenario)

  # Load comparison data if provided
  comparison_yaml <- NULL
  if (!is.null(comparison_scenario)) {
    comparison_data <- load_report_data(comparison_scenario)
    comparison_yaml <- results_to_yaml(comparison_data, comparison_scenario)
  }

  # Convert to YAML
  message('Preparing data for API...')
  baseline_yaml <- results_to_yaml(baseline_data, baseline_scenario)

  # Handle policy changes input
  if (is.null(policy_changes)) {
    message('\nNo policy changes provided. Enter policy changes (end with empty line):')
    lines <- c()
    repeat {
      line <- readline()
      if (line == '') break
      lines <- c(lines, line)
    }
    policy_changes <- paste(lines, collapse = '\n')
  }

  # Build prompt
  message('Building prompt...')
  prompt <- build_prompt(
    baseline_yaml = baseline_yaml,
    comparison_yaml = comparison_yaml,
    report_date = report_date,
    policy_changes = policy_changes,
    previous_report_date = previous_report_date
  )

  # Save prompt for debugging
  prompt_path <- file.path(output_dir, 'prompt.txt')
  writeLines(prompt, prompt_path)
  message(sprintf('  Saved prompt to: %s', prompt_path))

  # Call Claude API
  report_md <- call_claude_api(prompt, model = model)

  # Save Markdown
  md_path <- file.path(output_dir, 'state_of_tariffs.md')
  writeLines(report_md, md_path)
  message(sprintf('  Saved Markdown to: %s', md_path))

  # Convert to Word
  docx_path <- file.path(output_dir, 'state_of_tariffs.docx')
  reference_doc <- 'reports/ybl_style.docx'

  success <- convert_to_word(
    md_path = md_path,
    docx_path = docx_path,
    reference_doc = if (file.exists(reference_doc)) reference_doc else NULL
  )

  if (success) {
    message(sprintf('  Saved Word document to: %s', docx_path))
  }

  message('\n==========================================================')
  message('Report generation complete!')
  message('==========================================================\n')

  return(docx_path)
}


#' Quick report generation with minimal inputs
#'
#' Convenience wrapper that infers scenario names and prompts for required info.
#'
#' @param scenario Base scenario name (e.g., "11-17")
#' @param report_date Date for the report
#' @param ... Additional arguments passed to generate_report()
#'
#' @examples
#' quick_report('11-17', 'November 17, 2025')
quick_report <- function(scenario, report_date, ...) {
  # Infer comparison scenario name
  comparison <- paste0(scenario, '-ex-ieepa')

  if (!dir.exists(file.path('output', comparison))) {
    stop('Comparison scenario not found: ', comparison, '\n',
         'Run the ex-IEEPA scenario first.')
  }

  generate_report(
    baseline_scenario = scenario,
    comparison_scenario = comparison,
    report_date = report_date,
    ...
  )
}
