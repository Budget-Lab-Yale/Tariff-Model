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
  # Calculate aggregate sector effects (weighted average by output baseline)
  # -------------------------------------------------------------------------

  sector_effects <- sector_data %>%
    group_by(aggregate_sector) %>%
    summarise(
      effect = sum(output_baseline * output_pct_change) / sum(output_baseline),
      .groups = 'drop'
    )

  # Extract individual sector effects
  get_effect <- function(sector) sector_effects$effect[sector_effects$aggregate_sector == sector]

  agriculture <- get_effect('Agriculture')
  mining <- get_effect('Mining')
  manufacturing <- get_effect('Manufacturing')
  utilities <- get_effect('Utilities')
  construction <- get_effect('Construction')
  services <- get_effect('Services')

  # -------------------------------------------------------------------------
  # Manufacturing subcategories
  # -------------------------------------------------------------------------

  required_flags <- c('is_durable', 'is_nondurable', 'is_advanced')
  missing_flags <- setdiff(required_flags, names(sector_data))
  if (length(missing_flags) > 0) {
    stop('Missing required manufacturing flag columns in sector_outputs: ',
         paste(missing_flags, collapse = ', '))
  }
  if (any(is.na(sector_data$is_durable)) ||
      any(is.na(sector_data$is_nondurable)) ||
      any(is.na(sector_data$is_advanced))) {
    stop('Manufacturing flag columns contain NA values in sector_outputs')
  }

  mfg_data <- sector_data %>%
    filter(aggregate_sector == 'Manufacturing')

  # Helper for manufacturing subcategories (inline weighted average)
  calc_weighted_avg <- function(data) {
    sum(data$output_baseline * data$output_pct_change) / sum(data$output_baseline)
  }

  durable <- mfg_data %>%
    filter(is_durable == 1) %>%
    calc_weighted_avg()

  nondurable <- mfg_data %>%
    filter(is_nondurable == 1) %>%
    calc_weighted_avg()

  # Advanced manufacturing is just electronics (ele)
  advanced <- mfg_data %>%
    filter(is_advanced == 1) %>%
    pull(output_pct_change) %>%
    first()

  # -------------------------------------------------------------------------
  # Calculate overall GDP effect
  # -------------------------------------------------------------------------

  # Overall GDP comes from GTAP qgdp variable directly, not weighted sector average
  if (!is.null(inputs$qgdp) && 'usa' %in% names(inputs$qgdp)) {
    overall_gdp <- inputs$qgdp['usa']
  } else {
    # Fallback to weighted average if qgdp not available
    overall_gdp <- calc_weighted_avg(sector_data)
  }

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
