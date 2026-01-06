# =============================================================================
# 06_calculate_sectors.R - Calculate sector output effects
# =============================================================================
#
# This module calculates aggregate sector output effects from GTAP results.
#
# Key formula (weighted average by output baseline):
#   Aggregate Effect = sum(output_baseline * output_pct_change) / sum(output_baseline)
#
# Output sectors:
#   - Agriculture (includes forestry and fishing)
#   - Mining & Extraction
#   - Total Manufacturing
#   - Durable Manufacturing
#   - Advanced Manufacturing (electronics only)
#   - Nondurable Manufacturing
#   - Utilities
#   - Construction
#   - Services
#
# =============================================================================

library(tidyverse)

#' Calculate sector output effects
#'
#' @param inputs List containing:
#'   - sector_outputs: GTAP sector output data (gtap_sector, output_baseline, output_pct_change, etc.)
#'   - gtap_sector_mapping: Sector mapping with flags (optional, used if sector_outputs lacks flags)
#'
#' @return List containing sector output effects (percentage changes)
calculate_sectors <- function(inputs) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  sector_data <- inputs$sector_outputs

  if (is.null(sector_data)) {
    stop('Sector outputs not found in inputs$sector_outputs')
  }

  # Validate required columns
  required_cols <- c('gtap_sector', 'aggregate_sector', 'output_baseline', 'output_pct_change')
  missing_cols <- setdiff(required_cols, names(sector_data))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in sector_outputs: ', paste(missing_cols, collapse = ', '))
  }

  message('  Processing ', nrow(sector_data), ' sectors')

  # -------------------------------------------------------------------------
  # Calculate aggregate sector effects
  # -------------------------------------------------------------------------

  # Helper function for weighted average
  weighted_avg <- function(data) {
    if (nrow(data) == 0 || sum(data$output_baseline) == 0) {
      return(NA_real_)
    }
    sum(data$output_baseline * data$output_pct_change) / sum(data$output_baseline)
  }

  # Main aggregate sectors
  agriculture <- sector_data %>%
    filter(aggregate_sector == 'Agriculture') %>%
    weighted_avg()

  mining <- sector_data %>%
    filter(aggregate_sector == 'Mining') %>%
    weighted_avg()

  manufacturing <- sector_data %>%
    filter(aggregate_sector == 'Manufacturing') %>%
    weighted_avg()

  utilities <- sector_data %>%
    filter(aggregate_sector == 'Utilities') %>%
    weighted_avg()

  construction <- sector_data %>%
    filter(aggregate_sector == 'Construction') %>%
    weighted_avg()

  services <- sector_data %>%
    filter(aggregate_sector == 'Services') %>%
    weighted_avg()

  # -------------------------------------------------------------------------
  # Manufacturing subcategories
  # -------------------------------------------------------------------------

  # Check if manufacturing flags exist in data
  has_flags <- all(c('is_durable', 'is_nondurable', 'is_advanced') %in% names(sector_data))

  if (has_flags) {
    # Use flags from sector_outputs
    mfg_data <- sector_data %>%
      filter(aggregate_sector == 'Manufacturing')

    durable <- mfg_data %>%
      filter(is_durable == 1) %>%
      weighted_avg()

    nondurable <- mfg_data %>%
      filter(is_nondurable == 1) %>%
      weighted_avg()

    # Advanced manufacturing is just electronics (ele)
    advanced <- mfg_data %>%
      filter(is_advanced == 1) %>%
      pull(output_pct_change) %>%
      first()

  } else if (!is.null(inputs$gtap_sector_mapping)) {
    # Fall back to mapping file for flags
    mapping <- inputs$gtap_sector_mapping

    mfg_data <- sector_data %>%
      filter(aggregate_sector == 'Manufacturing') %>%
      left_join(
        mapping %>% select(gtap_code, is_durable, is_nondurable, is_advanced),
        by = c('gtap_sector' = 'gtap_code')
      )

    durable <- mfg_data %>%
      filter(is_durable == 1) %>%
      weighted_avg()

    nondurable <- mfg_data %>%
      filter(is_nondurable == 1) %>%
      weighted_avg()

    advanced <- mfg_data %>%
      filter(is_advanced == 1) %>%
      pull(output_pct_change) %>%
      first()

  } else {
    # No flags available
    message('  Warning: Manufacturing subcategory flags not available')
    durable <- NA_real_
    nondurable <- NA_real_
    advanced <- NA_real_
  }

  # -------------------------------------------------------------------------
  # Calculate overall GDP effect
  # -------------------------------------------------------------------------

  # Overall GDP is weighted average across all sectors
  overall_gdp <- weighted_avg(sector_data)

  # -------------------------------------------------------------------------
  # Build results
  # -------------------------------------------------------------------------

  results <- list(
    # Main sectors
    agriculture = agriculture,
    mining = mining,
    manufacturing = manufacturing,
    utilities = utilities,
    construction = construction,
    services = services,

    # Manufacturing subcategories
    durable = durable,
    nondurable = nondurable,
    advanced = advanced,

    # Overall
    overall_gdp = overall_gdp
  )

  # Log results
  message(sprintf('  Agriculture: %.2f%%', agriculture))
  message(sprintf('  Mining: %.2f%%', mining))
  message(sprintf('  Manufacturing: %.2f%%', manufacturing))
  message(sprintf('    Durable: %.2f%%', durable))
  message(sprintf('    Nondurable: %.2f%%', nondurable))
  message(sprintf('    Advanced: %.2f%%', advanced))
  message(sprintf('  Utilities: %.2f%%', utilities))
  message(sprintf('  Construction: %.2f%%', construction))
  message(sprintf('  Services: %.2f%%', services))
  message(sprintf('  Overall GDP: %.2f%%', overall_gdp))

  return(results)
}


#' Print sector summary table
#'
#' @param sector_results Results from calculate_sectors()
print_sector_summary <- function(sector_results) {

  cat('\n----------------------------------------------------------\n')
  cat('SECTOR OUTPUT EFFECTS\n')
  cat('----------------------------------------------------------\n')
  cat(sprintf('%-25s %10s\n', 'Sector', 'Change (%)'))
  cat('----------------------------------------------------------\n')

  cat(sprintf('%-25s %10.2f\n', 'Agriculture', sector_results$agriculture))
  cat(sprintf('%-25s %10.2f\n', 'Mining & Extraction', sector_results$mining))
  cat(sprintf('%-25s %10.2f\n', 'Total Manufacturing', sector_results$manufacturing))
  cat(sprintf('%-25s %10.2f\n', '  Durable Manufacturing', sector_results$durable))
  cat(sprintf('%-25s %10.2f\n', '  Advanced Manufacturing', sector_results$advanced))
  cat(sprintf('%-25s %10.2f\n', '  Nondurable Manufacturing', sector_results$nondurable))
  cat(sprintf('%-25s %10.2f\n', 'Utilities', sector_results$utilities))
  cat(sprintf('%-25s %10.2f\n', 'Construction', sector_results$construction))
  cat(sprintf('%-25s %10.2f\n', 'Services', sector_results$services))
  cat('----------------------------------------------------------\n')
  cat(sprintf('%-25s %10.2f\n', 'Overall Real GDP', sector_results$overall_gdp))
  cat('----------------------------------------------------------\n')
}
