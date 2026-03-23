#' Read GTAP Solution Files
#'
#' Functions to read GTAP .sl4 and .sol files directly using HARr package.
#' This eliminates the manual copy-paste step from ViewSOL to CSV.
#'
#' @details
#' GTAP solution files contain results from CGE model runs:
#' - .sl4 files: Cumulative solution values with variable metadata
#' - .sol files: Solution arrays organized by numbered headers
#'
#' Key header mappings (discovered through validation):
#' - Header 0160 = qgdp (GDP % change by region)
#' - Header 0052 = qo (sector output % change, 65 commodities x 9 regions)
#' - Header 0058 = qva (value-added % change, 65 commodities x 9 regions)
#' - Header 0181 = qmwreg (aggregate import % change by region)

library(HARr)
library(dplyr)

# ============================================================================
# CONSTANTS
# ============================================================================

# Header mappings from .sol file to GTAP variables
# Note: These headers are for sltoht-extracted .sol files
GTAP_HEADERS <- list(
  qgdp = '0160',      # GDP % change by region (9 values)
  qo = '0052',        # Sector output % change (65 x 9)
  qva = '0058',       # Value-added % change (65 x 9) - for sector GDP aggregation
  qxwreg = '0177',    # Aggregate export % change by region (9 values)
  qmwreg = '0181',    # Aggregate import % change by region (9 values)
  ppa = '0095',       # Import price from all sources (65 x 9)
  ppm = '0022',       # Import market price (65 x 9)
  ppd = '0021'        # Domestic price (65 x 9)
)

# Header mappings from .slc file (updated/level values)
GTAP_SLC_HEADERS <- list(
  viws = 'u028',           # VALIMPORTS - imports by [comm, src, dst] (65 x 9 x 9)
  viws_baseline = '0028',  # Baseline VALIMPORTS (pre-simulation)
  mtax = 'u063',           # MTAX - tariff revenue by [comm, src, dst] (65 x 9 x 9)
  mtax_baseline = '0063',  # Baseline MTAX (pre-simulation)
  vgdp = '0161',           # GDP levels by region ($millions, 9 values)
  vom = '0078',            # VOM - Value of Output at Market prices (65 x 9) - postsim
  nvpp_domestic = '0016',  # NVPP domestic baseline (65 x 9)
  nvpp_imported = '0017',  # NVPP imported baseline (65 x 9)
  nvpp_domestic_post = 'u016',  # NVPP domestic postsim (65 x 9)
  nvpp_imported_post = 'u017'   # NVPP imported postsim (65 x 9)
)


# ============================================================================
# MAIN FUNCTIONS
# ============================================================================

#' Read GTAP solution files
#'
#' @param sol Parsed .sol HAR object
#' @param regions Character vector of region names
#' @param commodities Character vector of commodity names
#' @return List containing extracted GTAP data
extract_sol_data <- function(sol, regions, commodities) {

  result <- list(
    regions = regions,
    commodities = commodities
  )

  # Extract qgdp (GDP % change by region)
  if (!is.null(sol[[GTAP_HEADERS$qgdp]])) {
    qgdp <- as.vector(sol[[GTAP_HEADERS$qgdp]])
    names(qgdp) <- regions
    result$qgdp <- qgdp
  }

  # Extract qo (sector outputs)
  if (!is.null(sol[[GTAP_HEADERS$qo]])) {
    qo <- sol[[GTAP_HEADERS$qo]]
    if (!is.null(commodities)) {
      rownames(qo) <- commodities
    }
    colnames(qo) <- regions
    result$qo <- qo
  }

  # Extract qva (value-added % change) - used for sector GDP aggregation
  if (!is.null(sol[[GTAP_HEADERS$qva]])) {
    qva <- sol[[GTAP_HEADERS$qva]]
    if (!is.null(commodities)) {
      rownames(qva) <- commodities
    }
    colnames(qva) <- regions
    result$qva <- qva
  }

  # Extract qmwreg (aggregate imports)
  if (!is.null(sol[[GTAP_HEADERS$qmwreg]])) {
    qmwreg <- as.vector(sol[[GTAP_HEADERS$qmwreg]])
    names(qmwreg) <- regions
    result$qmwreg <- qmwreg
  }

  # Extract qxwreg (aggregate exports)
  if (!is.null(sol[[GTAP_HEADERS$qxwreg]])) {
    qxwreg <- as.vector(sol[[GTAP_HEADERS$qxwreg]])
    names(qxwreg) <- regions
    result$qxwreg <- qxwreg
  }

  # Extract ppa (import prices from all sources)
  if (!is.null(sol[[GTAP_HEADERS$ppa]])) {
    ppa <- sol[[GTAP_HEADERS$ppa]]
    if (!is.null(commodities)) {
      rownames(ppa) <- commodities
    }
    colnames(ppa) <- regions
    result$ppa <- ppa
  }

  # Extract ppm (import market price)
  if (!is.null(sol[[GTAP_HEADERS$ppm]])) {
    ppm <- sol[[GTAP_HEADERS$ppm]]
    if (!is.null(commodities)) {
      rownames(ppm) <- commodities
    }
    colnames(ppm) <- regions
    result$ppm <- ppm
  }

  # Extract ppd (domestic price)
  if (!is.null(sol[[GTAP_HEADERS$ppd]])) {
    ppd <- sol[[GTAP_HEADERS$ppd]]
    if (!is.null(commodities)) {
      rownames(ppd) <- commodities
    }
    colnames(ppd) <- regions
    result$ppd <- ppd
  }

  return(result)
}

#' Get foreign GDP effects
#'
#' @param gtap_data Result from read_gtap_full()
#' @return Data frame with region and gdp_pct_change columns
get_foreign_gdp <- function(gtap_data) {
  if (is.null(gtap_data$qgdp)) {
    stop('qgdp not found in GTAP data')
  }

  result <- data.frame(
    region = GTAP_TO_ABBR[names(gtap_data$qgdp)],
    gdp_pct_change = round(gtap_data$qgdp, 2),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  return(result)
}

#' Get sector output effects
#'
#' @param gtap_data Result from read_gtap_full()
#' @param target_region Region to extract (default 'usa')
#' @return Data frame with gtap_sector and output_pct_change columns
get_sector_outputs <- function(gtap_data, target_region = 'usa') {
  if (is.null(gtap_data$qo)) {
    stop('qo not found in GTAP data')
  }

  qo <- gtap_data$qo
  region_idx <- which(colnames(qo) == target_region)

  if (length(region_idx) == 0) {
    stop(paste('Region not found:', target_region))
  }

  result <- data.frame(
    gtap_sector = rownames(qo),
    output_pct_change = round(qo[, region_idx], 2),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  return(result)
}

#' Get aggregate import change
#'
#' @param gtap_data Result from read_gtap_full()
#' @param target_region Region to extract (default 'usa')
#' @return Numeric value (% change)
get_import_change <- function(gtap_data, target_region = 'usa') {
  if (is.null(gtap_data$qmwreg)) {
    stop('qmwreg not found in GTAP data')
  }

  return(gtap_data$qmwreg[target_region])
}

#' Get aggregate export change
#'
#' @param gtap_data Result from read_gtap_full()
#' @param target_region Region to extract (default 'usa')
#' @return Numeric value (% change)
get_export_change <- function(gtap_data, target_region = 'usa') {
  if (is.null(gtap_data$qxwreg)) {
    stop('qxwreg not found in GTAP data')
  }

  return(gtap_data$qxwreg[target_region])
}

#' Get import prices by commodity
#'
#' @param gtap_data Result from read_gtap_full()
#' @param target_region Region to extract (default 'usa')
#' @return Data frame with gtap_sector and price_pct_change columns
get_import_prices <- function(gtap_data, target_region = 'usa') {
  if (is.null(gtap_data$ppa)) {
    stop('ppa not found in GTAP data')
  }

  ppa <- gtap_data$ppa
  region_idx <- which(colnames(ppa) == target_region)

  if (length(region_idx) == 0) {
    stop(paste('Region not found:', target_region))
  }

  result <- data.frame(
    gtap_sector = rownames(ppa),
    price_pct_change = round(ppa[, region_idx], 2),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  return(result)
}

# ============================================================================
# SLC EXTRACTION FUNCTIONS (operate on pre-parsed slc object)
# ============================================================================

#' Extract VIWS (VALIMPORTS) from parsed .slc data
#'
#' Extracts post-simulation import values by commodity, source, and destination.
#' Returns imports TO a target region (default USA) FROM all source regions.
#'
#' @param slc Parsed .slc HAR object
#' @param regions Character vector of region names
#' @param commodities Character vector of commodity names
#' @param target_region Index of destination region (default 1 = USA)
#' @return Matrix of imports [commodity x source_region] in $millions
extract_viws <- function(slc, regions, commodities, target_region = 1) {

  viws_header <- GTAP_SLC_HEADERS$viws
  if (is.null(slc[[viws_header]])) {
    stop(paste('VIWS header not found:', viws_header))
  }

  viws_full <- slc[[viws_header]]

  # Extract imports to target region (dst dimension)
  # viws_full[comm, src, dst] -> viws[comm, src] for dst=target_region
  viws <- viws_full[, , target_region]

  if (!is.null(commodities)) {
    rownames(viws) <- commodities
  }
  colnames(viws) <- regions

  return(viws)
}

#' Extract VGDP (GDP levels) from parsed .slc data
#'
#' @param slc Parsed .slc HAR object
#' @param regions Character vector of region names
#' @return Named vector of GDP levels by region ($millions)
extract_vgdp <- function(slc, regions) {

  vgdp_header <- GTAP_SLC_HEADERS$vgdp
  if (is.null(slc[[vgdp_header]])) {
    stop(paste('VGDP header not found:', vgdp_header))
  }

  vgdp <- as.vector(slc[[vgdp_header]])
  names(vgdp) <- regions

  return(vgdp)
}

#' Extract NVPP and calculate adjustment factors for post-substitution price
#'
#' NVPP (National Value of Production and Purchases) provides domestic and
#' imported values by commodity. The adjustment factors scale the base
#' goods_share and import_share parameters for post-substitution price effects.
#'
#' Formulas (from Excel ricco_price_effects_and_etr row 25):
#' - goods_adjustment = (goods_total_postsim / all_total_postsim) /
#'                      (goods_total_baseline / all_total_baseline)
#' - import_adjustment = (goods_imp_postsim / goods_total_postsim) /
#'                       (goods_imp_baseline / goods_total_baseline)
#'
#' @param slc Parsed .slc HAR object
#' @param commodities Character vector of commodity names (from stel)
#' @param sector_mapping Data frame with gtap_code and aggregate_sector columns
#' @param target_region Index of region (default 1 = USA)
#' @return List with goods_adjustment and import_adjustment factors
extract_nvpp_adjustment <- function(slc, commodities, sector_mapping,
                                    target_region = 1) {

  # Extract NVPP arrays (65 commodities x 9 regions)
  domestic_baseline <- slc[[GTAP_SLC_HEADERS$nvpp_domestic]]
  imported_baseline <- slc[[GTAP_SLC_HEADERS$nvpp_imported]]
  domestic_postsim <- slc[[GTAP_SLC_HEADERS$nvpp_domestic_post]]
  imported_postsim <- slc[[GTAP_SLC_HEADERS$nvpp_imported_post]]

  if (is.null(domestic_baseline) || is.null(imported_baseline) ||
      is.null(domestic_postsim) || is.null(imported_postsim)) {
    stop('NVPP headers not found in .slc file')
  }

  # Extract USA column (target_region)
  dom_bl <- domestic_baseline[, target_region]
  imp_bl <- imported_baseline[, target_region]
  dom_ps <- domestic_postsim[, target_region]
  imp_ps <- imported_postsim[, target_region]

  # Derive goods indices from sector mapping (Agriculture, Mining, Manufacturing)
  goods_sectors <- sector_mapping$aggregate_sector %in%
    c('Agriculture', 'Mining', 'Manufacturing')
  goods_codes <- tolower(sector_mapping$gtap_code[goods_sectors])
  goods_indices <- which(tolower(commodities) %in% goods_codes)

  if (length(goods_indices) == 0) {
    stop('No goods sectors matched between sector_mapping and GTAP commodities')
  }
  message(sprintf('    NVPP goods indices: %d of %d commodities (from sector_mapping)',
                  length(goods_indices), length(commodities)))

  # Calculate totals for goods sectors vs all sectors
  goods_dom_baseline <- sum(dom_bl[goods_indices])
  goods_imp_baseline <- sum(imp_bl[goods_indices])
  goods_total_baseline <- goods_dom_baseline + goods_imp_baseline

  all_dom_baseline <- sum(dom_bl)
  all_imp_baseline <- sum(imp_bl)
  all_total_baseline <- all_dom_baseline + all_imp_baseline

  goods_dom_postsim <- sum(dom_ps[goods_indices])
  goods_imp_postsim <- sum(imp_ps[goods_indices])
  goods_total_postsim <- goods_dom_postsim + goods_imp_postsim

  all_dom_postsim <- sum(dom_ps)
  all_imp_postsim <- sum(imp_ps)
  all_total_postsim <- all_dom_postsim + all_imp_postsim

  # Calculate adjustment factors
  goods_share_baseline <- goods_total_baseline / all_total_baseline
  goods_share_postsim <- goods_total_postsim / all_total_postsim
  goods_adjustment <- goods_share_postsim / goods_share_baseline

  import_share_baseline <- goods_imp_baseline / goods_total_baseline
  import_share_postsim <- goods_imp_postsim / goods_total_postsim
  import_adjustment <- import_share_postsim / import_share_baseline

  # Per-commodity import share ratio (for post-substitution omega_M adjustment)
  imp_share_bl <- imp_bl / (imp_bl + dom_bl)
  imp_share_ps <- imp_ps / (imp_ps + dom_ps)
  commodity_ratio <- imp_share_ps / imp_share_bl
  # Handle NaN from zero totals
  commodity_ratio[is.nan(commodity_ratio) | is.infinite(commodity_ratio)] <- 1.0
  names(commodity_ratio) <- commodities

  return(list(
    goods_adjustment = goods_adjustment,
    import_adjustment = import_adjustment,
    # Include intermediate values for debugging
    goods_share_baseline = goods_share_baseline,
    goods_share_postsim = goods_share_postsim,
    import_share_baseline = import_share_baseline,
    import_share_postsim = import_share_postsim,
    # Per-commodity data
    commodity_ratio = commodity_ratio
  ))
}

#' Extract MTAX (tariff revenue) and calculate etr_increase
#'
#' Calculates etr_increase = (scenario_mtax / scenario_imports) - (baseline_mtax / baseline_imports)
#' This uses total imports (including services) as the denominator.
#'
#' @param slc Parsed .slc HAR object
#' @param target_region Index of destination region (default 1 = USA)
#' @return List with mtax_scenario, mtax_baseline, imports_scenario, imports_baseline, etr_increase
extract_mtax_etr_increase <- function(slc, target_region = 1) {

  # Get scenario mtax (tariff revenue)
  if (is.null(slc[[GTAP_SLC_HEADERS$mtax]])) {
    stop('MTAX header not found in .slc: ', GTAP_SLC_HEADERS$mtax)
  }
  mtax_scenario_full <- slc[[GTAP_SLC_HEADERS$mtax]]
  mtax_scenario <- sum(mtax_scenario_full[, , target_region])

  # Get baseline mtax
  if (is.null(slc[[GTAP_SLC_HEADERS$mtax_baseline]])) {
    stop('MTAX baseline header not found in .slc: ', GTAP_SLC_HEADERS$mtax_baseline)
  }
  mtax_baseline_full <- slc[[GTAP_SLC_HEADERS$mtax_baseline]]
  mtax_baseline <- sum(mtax_baseline_full[, , target_region])

  # Get scenario imports (VIWS)
  if (is.null(slc[[GTAP_SLC_HEADERS$viws]])) {
    stop('VIWS header not found in .slc: ', GTAP_SLC_HEADERS$viws)
  }
  viws_scenario_full <- slc[[GTAP_SLC_HEADERS$viws]]
  imports_scenario <- sum(viws_scenario_full[, , target_region])

  # Get baseline imports
  if (is.null(slc[[GTAP_SLC_HEADERS$viws_baseline]])) {
    stop('VIWS baseline header not found in .slc: ', GTAP_SLC_HEADERS$viws_baseline)
  }
  viws_baseline_full <- slc[[GTAP_SLC_HEADERS$viws_baseline]]
  imports_baseline <- sum(viws_baseline_full[, , target_region])

  # Calculate ETRs
  etr_scenario <- mtax_scenario / imports_scenario
  etr_baseline <- mtax_baseline / imports_baseline

  # Calculate etr_increase
  etr_increase <- etr_scenario - etr_baseline

  return(list(
    mtax_scenario = mtax_scenario,
    mtax_baseline = mtax_baseline,
    imports_scenario = imports_scenario,
    imports_baseline = imports_baseline,
    etr_scenario = etr_scenario,
    etr_baseline = etr_baseline,
    etr_increase = etr_increase
  ))
}

#' Read GTAP solution with VIWS and VGDP
#'
#' Master reader that parses each file once and passes parsed objects to
#' extraction functions.
#'
#' @param sol_path Path to .sol file
#' @param slc_path Path to .slc file
#' @param sl4_path Path to .sl4 file (for metadata)
#' @param sector_mapping Data frame with gtap_code and aggregate_sector columns
#'   (used to derive goods indices for NVPP adjustment)
#' @return List containing all extracted GTAP data including viws and vgdp
read_gtap_full <- function(sol_path, slc_path, sl4_path, sector_mapping) {

  # ---- Parse each file once ----
  if (is.null(sl4_path) || !file.exists(sl4_path)) {
    stop('GTAP .sl4 file not found: ', sl4_path)
  }
  if (!file.exists(slc_path)) {
    stop('GTAP .slc file not found: ', slc_path)
  }

  sol <- read_har(sol_path)
  slc <- read_har(slc_path)
  sl4 <- read_har(sl4_path)

  stel <- sl4[['stel']]
  if (is.null(stel)) {
    stop('GTAP .sl4 file missing stel metadata: ', sl4_path)
  }
  regions <- stel[1:9]
  commodities <- stel[10:74]

  # ---- Extract .sol data ----
  result <- extract_sol_data(sol, regions, commodities)

  # ---- Extract .slc data ----
  result$viws <- extract_viws(slc, regions, commodities, target_region = 1)
  result$vgdp <- extract_vgdp(slc, regions)

  # Calculate etr_increase from mtax
  mtax_data <- extract_mtax_etr_increase(slc, target_region = 1)
  result$etr_increase <- mtax_data$etr_increase
  result$mtax_data <- mtax_data

  # Read NVPP adjustment factors (goods indices derived from sector_mapping)
  nvpp_adjustment <- extract_nvpp_adjustment(slc, commodities, sector_mapping,
                                             target_region = 1)
  result$nvpp_adjustment <- nvpp_adjustment

  # VOM (Value of Output at Market prices) for sector weighting
  if (!is.null(slc[[GTAP_SLC_HEADERS$vom]])) {
    vom <- slc[[GTAP_SLC_HEADERS$vom]]
    rownames(vom) <- commodities
    colnames(vom) <- regions
    result$vom <- vom
  }

  return(result)
}

# ============================================================================
# IMPORTS BY COUNTRY
# ============================================================================

#' Get total imports by source country
#'
#' Aggregates the VIWS (imports) matrix across commodities to get
#' total imports by source country.
#'
#' @param gtap_data Result from read_gtap_full() containing viws matrix
#' @return Named vector of imports by country (in $millions)
get_imports_by_country <- function(gtap_data) {
  if (is.null(gtap_data$viws)) {
    stop('viws not found in GTAP data - need to use read_gtap_full()')
  }

  # Sum across commodities (rows) for each source country (columns)
  country_totals <- colSums(gtap_data$viws)

  names(country_totals) <- GTAP_TO_ABBR[names(country_totals)]

  return(country_totals)
}

# ============================================================================
# SECTOR OUTPUTS WITH MAPPINGS
# ============================================================================

#' Get sector outputs with full mapping metadata
#'
#' Joins GTAP sector value-added changes (qva) with static mapping file.
#' Uses VOM (Value of Output) to calculate baseline for weighting.
#' Formula: baseline = VOM_postsim / (1 + qva/100)
#'
#' @param gtap_data Result from read_gtap_full() (must contain qva and vom)
#' @param sector_mapping Data frame with gtap_code and sector flags
#' @param target_region Region to extract (default 'usa')
#' @return Data frame matching sector_outputs.csv structure
get_sector_outputs_full <- function(gtap_data, sector_mapping, target_region = 'usa') {
  # Use qva (value-added % change) instead of qo for sector aggregation
  if (is.null(gtap_data$qva)) {
    stop('qva not found in GTAP data')
  }
  if (is.null(gtap_data$vom)) {
    stop('vom not found in GTAP data')
  }

  qva <- gtap_data$qva
  vom <- gtap_data$vom

  region_idx <- which(colnames(qva) == target_region)
  if (length(region_idx) == 0) {
    stop(paste('Region not found:', target_region))
  }

  # Get qva and VOM for target region
  qva_values <- qva[, region_idx]
  vom_postsim <- vom[, region_idx]

  # Calculate baseline output: baseline = postsim / (1 + pct_change/100)
  output_baseline <- vom_postsim / (1 + qva_values / 100)

  # Build outputs data frame
  outputs <- data.frame(
    gtap_sector = rownames(qva),
    output_pct_change = round(qva_values, 2),
    output_baseline = output_baseline,
    stringsAsFactors = FALSE
  )
  rownames(outputs) <- NULL

  # Join with mapping
  result <- outputs %>%
    left_join(
      sector_mapping %>%
        select(gtap_code, aggregate_sector, is_manufacturing,
               is_durable, is_nondurable, is_advanced),
      by = c('gtap_sector' = 'gtap_code')
    )

  return(result)
}

# ============================================================================
# MASTER LOADING FUNCTION
# ============================================================================

#' Load all GTAP outputs from solution files
#'
#' Main entry point for loading GTAP data from .sol/.sl4/.slc files.
#' Returns all data needed by the pipeline in a standardized format.
#'
#' @param solution_dir Directory containing GTAP solution files
#' @param file_prefix Optional file prefix to filter files (e.g., '11-17')
#' @param sector_mapping Data frame with sector classifications
#' @return List containing all GTAP outputs
load_gtap_from_files <- function(solution_dir, file_prefix = NULL,
                                 sector_mapping = NULL) {
  # Build file pattern based on prefix
  if (!is.null(file_prefix)) {
    sol_pattern <- paste0('^', file_prefix, '\\.sol$')
    sl4_pattern <- paste0('^', file_prefix, '\\.sl4$')
    slc_pattern <- paste0('^', file_prefix, '\\.slc$')
  } else {
    sol_pattern <- '\\.sol$'
    sl4_pattern <- '\\.sl4$'
    slc_pattern <- '\\.slc$'
  }

  # Find files
  sol_files <- list.files(solution_dir, pattern = sol_pattern, full.names = TRUE)
  sl4_files <- list.files(solution_dir, pattern = sl4_pattern, full.names = TRUE)
  slc_files <- list.files(solution_dir, pattern = slc_pattern, full.names = TRUE)

  if (length(sol_files) == 0) {
    stop(paste('No .sol files found in:', solution_dir,
               if (!is.null(file_prefix)) paste('with prefix:', file_prefix) else ''))
  }
  if (length(slc_files) == 0) {
    stop(paste('No .slc files found in:', solution_dir,
               if (!is.null(file_prefix)) paste('with prefix:', file_prefix) else ''))
  }

  sol_path <- sol_files[1]
  if (length(sl4_files) == 0) {
    stop(paste('No .sl4 files found in:', solution_dir,
               if (!is.null(file_prefix)) paste('with prefix:', file_prefix) else ''))
  }
  sl4_path <- sl4_files[1]
  slc_path <- slc_files[1]

  message(sprintf('  Reading GTAP solution: %s', basename(sol_path)))

  if (is.null(sector_mapping)) {
    stop('Sector mapping data is required')
  }

  # Read full GTAP data (parses each file once)
  gtap_data <- read_gtap_full(sol_path, slc_path, sl4_path, sector_mapping)

  result <- list()

  # Foreign GDP
  result$foreign_gdp <- get_foreign_gdp(gtap_data)
  message(sprintf('    - Foreign GDP: %d regions', nrow(result$foreign_gdp)))

  # Import change (qmwreg)
  result$qmwreg <- get_import_change(gtap_data, 'usa')
  message(sprintf('    - Import change (qmwreg): %.2f%%', result$qmwreg))

  # Export change (qxwreg)
  result$qxwreg <- get_export_change(gtap_data, 'usa')
  message(sprintf('    - Export change (qxwreg): %.2f%%', result$qxwreg))

  # Imports by country
  result$imports_by_country <- get_imports_by_country(gtap_data)
  message(sprintf('    - Imports by country: %d countries', length(result$imports_by_country)))

  result$sector_outputs <- get_sector_outputs_full(gtap_data, sector_mapping, 'usa')
  message(sprintf('    - Sector outputs: %d sectors (with mappings)', nrow(result$sector_outputs)))

  # Raw data for downstream calculations
  result$viws <- gtap_data$viws
  result$vgdp <- gtap_data$vgdp
  result$ppm <- gtap_data$ppm
  result$ppd <- gtap_data$ppd
  result$ppa <- gtap_data$ppa
  result$qva <- gtap_data$qva
  result$vom <- gtap_data$vom
  result$qgdp <- gtap_data$qgdp

  # ETR increase from mtax (for revenue calculations)
  if (is.null(gtap_data$etr_increase)) {
    stop('ETR increase not found in GTAP data')
  }
  result$etr_increase <- gtap_data$etr_increase
  result$mtax_data <- gtap_data$mtax_data
  message(sprintf('    - ETR increase (from mtax): %.4f (%.2f%%)',
                  result$etr_increase, result$etr_increase * 100))

  # NVPP adjustment factors for post-substitution price calculation
  if (is.null(gtap_data$nvpp_adjustment)) {
    stop('NVPP adjustment not found in GTAP data')
  }
  result$nvpp_adjustment <- gtap_data$nvpp_adjustment
  message(sprintf('    - NVPP adjustments: goods=%.4f, import=%.4f',
                  result$nvpp_adjustment$goods_adjustment,
                  result$nvpp_adjustment$import_adjustment))

  # Per-commodity NVPP import share ratio (for post-substitution omega_M)
  result$nvpp_commodity_ratio <- gtap_data$nvpp_adjustment$commodity_ratio

  return(result)
}
