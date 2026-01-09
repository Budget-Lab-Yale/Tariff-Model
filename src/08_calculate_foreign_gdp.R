# =============================================================================
# 08_calculate_foreign_gdp.R - Calculate foreign GDP effects
# =============================================================================
#
# This module reports long-run GDP effects by region from GTAP results.
#
# The GDP effects come from the GTAP qgdp variable (real GDP percent change).
# These are equilibrium effects after trade adjustments.
#
# Output regions:
#   - USA (also reported in macro effects, but this is long-run GTAP estimate)
#   - China
#   - Canada
#   - Mexico
#   - EU
#   - UK
#   - Japan
#   - FTA countries (other FTA partners)
#   - ROW (rest of world)
#
# =============================================================================

library(tidyverse)

#' Calculate foreign GDP effects from GTAP results
#'
#' @param inputs List containing:
#'   - foreign_gdp: GTAP foreign GDP data (region, gdp_pct_change)
#'
#' @return List containing GDP effects by region (percentage changes)
calculate_foreign_gdp <- function(inputs) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  gdp_data <- inputs$foreign_gdp

  if (is.null(gdp_data)) {
    stop('Foreign GDP data not found in inputs$foreign_gdp')
  }

  # Validate required columns
  required_cols <- c('region', 'gdp_pct_change')
  missing_cols <- setdiff(required_cols, names(gdp_data))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in foreign_gdp: ', paste(missing_cols, collapse = ', '))
  }

  message('  Processing ', nrow(gdp_data), ' regions')

  # -------------------------------------------------------------------------
  # Load VGDP baseline for weighted averages
  # -------------------------------------------------------------------------

  vgdp_baseline_file <- 'resources/gtap_baseline/vgdp_baseline.csv'
  if (!file.exists(vgdp_baseline_file)) {
    stop('vgdp_baseline.csv not found: ', vgdp_baseline_file)
  }
  vgdp_baseline <- read_csv(vgdp_baseline_file, show_col_types = FALSE)

  # -------------------------------------------------------------------------
  # Extract GDP effects by region
  # -------------------------------------------------------------------------

  # Region mapping: output name -> gtap region code
  region_codes <- c(usa = 'usa', china = 'chn', canada = 'can', mexico = 'mex',
                    eu = 'eu', uk = 'gbr', japan = 'jpn', fta = 'fta', row = 'row')

  # Build results by looking up each region
  gdp_lookup <- setNames(gdp_data$gdp_pct_change, gdp_data$region)
  results <- lapply(region_codes, function(code) {
    if (!code %in% names(gdp_lookup)) stop('Missing GDP effect for region: ', code)
    gdp_lookup[[code]]
  })

  # -------------------------------------------------------------------------
  # Calculate World and World ex USA (GDP-weighted averages)
  # -------------------------------------------------------------------------

  # Map region codes to GTAP region names
  region_map <- c(
    usa = 'usa', chn = 'china', row = 'row', can = 'canada',
    mex = 'mexico', fta = 'ftrow', jpn = 'japan', eu = 'eu', gbr = 'uk'
  )

  # Join gdp_data with vgdp_baseline
  gdp_with_levels <- gdp_data %>%
    mutate(gtap_region = region_map[region]) %>%
    left_join(vgdp_baseline, by = c('gtap_region' = 'region'))

  if (any(is.na(gdp_with_levels$vgdp))) {
    missing_regions <- gdp_with_levels$gtap_region[is.na(gdp_with_levels$vgdp)]
    stop('Missing vgdp baseline for GTAP region(s): ', paste(missing_regions, collapse = ', '))
  }

  # Calculate post-sim GDP and changes
  gdp_with_levels <- gdp_with_levels %>%
    mutate(
      vgdp_pre = vgdp,
      vgdp_post = vgdp * (1 + gdp_pct_change / 100),
      vgdp_change = vgdp_post - vgdp_pre
    )

  # World Total
  world_pre <- sum(gdp_with_levels$vgdp_pre, na.rm = TRUE)
  world_post <- sum(gdp_with_levels$vgdp_post, na.rm = TRUE)
  world_pct <- (world_post / world_pre - 1) * 100

  # World ex USA
  world_ex_usa <- gdp_with_levels %>% filter(region != 'usa')
  world_ex_usa_pre <- sum(world_ex_usa$vgdp_pre, na.rm = TRUE)
  world_ex_usa_post <- sum(world_ex_usa$vgdp_post, na.rm = TRUE)
  world_ex_usa_pct <- (world_ex_usa_post / world_ex_usa_pre - 1) * 100

  results$world <- world_pct
  results$world_ex_usa <- world_ex_usa_pct

  # Store detailed GDP levels
  results$gdp_levels <- gdp_with_levels %>%
    select(region, gtap_region, gdp_pct_change, vgdp_pre, vgdp_post, vgdp_change)

  # Log results
  message(sprintf('  USA:     %+.2f%%', results$usa))
  message(sprintf('  China:   %+.2f%%', results$china))
  message(sprintf('  Canada:  %+.2f%%', results$canada))
  message(sprintf('  Mexico:  %+.2f%%', results$mexico))
  message(sprintf('  EU:      %+.2f%%', results$eu))
  message(sprintf('  UK:      %+.2f%%', results$uk))
  message(sprintf('  Japan:   %+.2f%%', results$japan))
  message(sprintf('  FTA:     %+.2f%%', results$fta))
  message(sprintf('  ROW:     %+.2f%%', results$row))

  message(sprintf('  World:       %+.4f%%', results$world))
  message(sprintf('  World exUSA: %+.4f%%', results$world_ex_usa))

  return(results)
}


#' Print foreign GDP summary table
#'
#' @param foreign_gdp_results Results from calculate_foreign_gdp()
print_foreign_gdp_summary <- function(foreign_gdp_results) {

  cat('\n----------------------------------------------------------\n')
  cat('FOREIGN GDP EFFECTS (Long-Run)\n')
  cat('----------------------------------------------------------\n')
  cat(sprintf('%-20s %10s\n', 'Region', 'Change (%)'))
  cat('----------------------------------------------------------\n')

  cat(sprintf('%-20s %+10.2f\n', 'USA', foreign_gdp_results$usa))
  cat(sprintf('%-20s %+10.2f\n', 'China', foreign_gdp_results$china))
  cat(sprintf('%-20s %+10.2f\n', 'Canada', foreign_gdp_results$canada))
  cat(sprintf('%-20s %+10.2f\n', 'Mexico', foreign_gdp_results$mexico))
  cat(sprintf('%-20s %+10.2f\n', 'EU', foreign_gdp_results$eu))
  cat(sprintf('%-20s %+10.2f\n', 'UK', foreign_gdp_results$uk))
  cat(sprintf('%-20s %+10.2f\n', 'Japan', foreign_gdp_results$japan))
  cat(sprintf('%-20s %+10.2f\n', 'FTA Partners', foreign_gdp_results$fta))
  cat(sprintf('%-20s %+10.2f\n', 'Rest of World', foreign_gdp_results$row))
  cat('----------------------------------------------------------\n')
}
