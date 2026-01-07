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
#' - Header 0042 = qgdp (GDP % change by region)
#' - Header 0001 = qo (sector output % change, 65 commodities x 9 regions)
#' - Header 0030 = qmwreg (aggregate import % change by region)

library(HARr)
library(dplyr)

# ============================================================================
# CONSTANTS
# ============================================================================

# Header mappings from .sol file to GTAP variables
GTAP_HEADERS <- list(
  qgdp = '0042',      # GDP % change by region (9 values)
  qo = '0001',        # Sector output % change (65 x 9)
  qmwreg = '0030',    # Aggregate import % change by region (9 values)
  ppa = '0058',       # Import price from all sources (65 x 9)
  ppm = '0078',       # Import market price (65 x 9)
  ppd = '0066'        # Domestic price (65 x 9)
)

# Header mappings from .slc file (updated/level values)
GTAP_SLC_HEADERS <- list(
  viws = 'u028',           # VALIMPORTS - imports by [comm, src, dst] (65 x 9 x 9)
  viws_baseline = '0028',  # Baseline VALIMPORTS (pre-simulation)
  mtax = 'u063',           # MTAX - tariff revenue by [comm, src, dst] (65 x 9 x 9)
  mtax_baseline = '0063',  # Baseline MTAX (pre-simulation)
  vgdp = '0161',           # GDP levels by region ($millions, 9 values)
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
#' @param sol_path Path to .sol file
#' @param sl4_path Path to .sl4 file (for metadata)
#' @return List containing extracted GTAP data
read_gtap_solution <- function(sol_path, sl4_path) {
  # Read solution file
  sol <- read_har(sol_path)

  # Get set labels from sl4
  if (is.null(sl4_path) || !file.exists(sl4_path)) {
    stop('GTAP .sl4 file not found: ', sl4_path)
  }
  sl4 <- read_har(sl4_path)
  stel <- sl4[['stel']]
  if (is.null(stel)) {
    stop('GTAP .sl4 file missing stel metadata: ', sl4_path)
  }
  regions <- stel[1:9]
  commodities <- stel[10:74]

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

  # Extract qmwreg (aggregate imports)
  if (!is.null(sol[[GTAP_HEADERS$qmwreg]])) {
    qmwreg <- as.vector(sol[[GTAP_HEADERS$qmwreg]])
    names(qmwreg) <- regions
    result$qmwreg <- qmwreg
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
#' @param gtap_data Result from read_gtap_solution()
#' @return Data frame with region and gdp_pct_change columns
get_foreign_gdp <- function(gtap_data) {
  if (is.null(gtap_data$qgdp)) {
    stop('qgdp not found in GTAP data')
  }

  # Map GTAP region codes to standard abbreviations
  region_map <- c(
    usa = 'usa',
    china = 'chn',
    row = 'row',
    canada = 'can',
    mexico = 'mex',
    ftrow = 'fta',
    japan = 'jpn',
    eu = 'eu',
    uk = 'gbr'
  )

  result <- data.frame(
    region = region_map[names(gtap_data$qgdp)],
    gdp_pct_change = round(gtap_data$qgdp, 2),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  return(result)
}

#' Get sector output effects
#'
#' @param gtap_data Result from read_gtap_solution()
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
#' @param gtap_data Result from read_gtap_solution()
#' @param target_region Region to extract (default 'usa')
#' @return Numeric value (% change)
get_import_change <- function(gtap_data, target_region = 'usa') {
  if (is.null(gtap_data$qmwreg)) {
    stop('qmwreg not found in GTAP data')
  }

  return(gtap_data$qmwreg[target_region])
}

#' Get import prices by commodity
#'
#' @param gtap_data Result from read_gtap_solution()
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
# SLC FILE FUNCTIONS (Level/Updated Values)
# ============================================================================

#' Read VIWS (VALIMPORTS) from .slc file
#'
#' Extracts post-simulation import values by commodity, source, and destination.
#' Returns imports TO a target region (default USA) FROM all source regions.
#'
#' @param slc_path Path to .slc file
#' @param sl4_path Path to .sl4 file (for metadata)
#' @param target_region Index of destination region (default 1 = USA)
#' @return Matrix of imports [commodity x source_region] in $millions
read_viws <- function(slc_path, sl4_path, target_region = 1) {
  slc <- read_har(slc_path)

  # Get set labels from sl4
  if (is.null(sl4_path) || !file.exists(sl4_path)) {
    stop('GTAP .sl4 file not found: ', sl4_path)
  }
  sl4 <- read_har(sl4_path)
  stel <- sl4[['stel']]
  if (is.null(stel)) {
    stop('GTAP .sl4 file missing stel metadata: ', sl4_path)
  }
  regions <- stel[1:9]
  commodities <- stel[10:74]

  # Extract VIWS array [comm, src, dst]
  viws_header <- GTAP_SLC_HEADERS$viws
  if (is.null(slc[[viws_header]])) {
    stop(paste('VIWS header not found:', viws_header))
  }

  viws_full <- slc[[viws_header]]

  # Extract imports to target region (dst dimension)
  # viws_full[comm, src, dst] -> viws[comm, src] for dst=target_region
  viws <- viws_full[, , target_region]

  # Set dimnames
  if (!is.null(commodities)) {
    rownames(viws) <- commodities
  }
  colnames(viws) <- regions

  return(viws)
}

#' Read VGDP (GDP levels) from .slc file
#'
#' @param slc_path Path to .slc file
#' @param sl4_path Path to .sl4 file (for metadata)
#' @return Named vector of GDP levels by region ($millions)
read_vgdp <- function(slc_path, sl4_path) {
  slc <- read_har(slc_path)

  # Get region labels from sl4
  if (is.null(sl4_path) || !file.exists(sl4_path)) {
    stop('GTAP .sl4 file not found: ', sl4_path)
  }
  sl4 <- read_har(sl4_path)
  stel <- sl4[['stel']]
  if (is.null(stel)) {
    stop('GTAP .sl4 file missing stel metadata: ', sl4_path)
  }
  regions <- stel[1:9]

  # Extract VGDP
  vgdp_header <- GTAP_SLC_HEADERS$vgdp
  if (is.null(slc[[vgdp_header]])) {
    stop(paste('VGDP header not found:', vgdp_header))
  }

  vgdp <- as.vector(slc[[vgdp_header]])
  names(vgdp) <- regions

  return(vgdp)
}

#' Read NVPP and calculate adjustment factors for post-substitution price
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
#' @param slc_path Path to .slc file
#' @param goods_indices Indices of goods sectors (default 1:45 for 45 goods)
#' @param target_region Index of region (default 1 = USA)
#' @return List with goods_adjustment and import_adjustment factors
read_nvpp_adjustment <- function(slc_path, goods_indices = 1:45, target_region = 1) {
  slc <- read_har(slc_path)

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

  return(list(
    goods_adjustment = goods_adjustment,
    import_adjustment = import_adjustment,
    # Include intermediate values for debugging
    goods_share_baseline = goods_share_baseline,
    goods_share_postsim = goods_share_postsim,
    import_share_baseline = import_share_baseline,
    import_share_postsim = import_share_postsim
  ))
}

#' Read MTAX (tariff revenue) and calculate etr_increase
#'
#' Calculates etr_increase = (scenario_mtax / scenario_imports) - (baseline_mtax / baseline_imports)
#' This uses total imports (including services) as the denominator.
#'
#' @param slc_path Path to .slc file
#' @param target_region Index of destination region (default 1 = USA)
#' @return List with mtax_scenario, mtax_baseline, imports_scenario, imports_baseline, etr_increase
read_mtax_etr_increase <- function(slc_path, target_region = 1) {
  slc <- read_har(slc_path)

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
#' Extended version that also reads .slc file for level values.
#'
#' @param sol_path Path to .sol file
#' @param slc_path Path to .slc file
#' @param sl4_path Path to .sl4 file (for metadata)
#' @return List containing all extracted GTAP data including viws and vgdp
read_gtap_full <- function(sol_path, slc_path, sl4_path) {
  # Get base solution data
  result <- read_gtap_solution(sol_path, sl4_path)

  # Add VIWS from .slc file
  if (!file.exists(slc_path)) {
    stop('GTAP .slc file not found: ', slc_path)
  }
  result$viws <- read_viws(slc_path, sl4_path, target_region = 1)
  result$vgdp <- read_vgdp(slc_path, sl4_path)

  # Calculate etr_increase from mtax
  mtax_data <- read_mtax_etr_increase(slc_path, target_region = 1)
  result$etr_increase <- mtax_data$etr_increase
  result$mtax_data <- mtax_data

  # Read NVPP adjustment factors for post-substitution price calculation
  nvpp_adjustment <- read_nvpp_adjustment(slc_path, goods_indices = 1:45, target_region = 1)
  result$nvpp_adjustment <- nvpp_adjustment

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

  # Map GTAP region names to standard abbreviations
  region_map <- c(
    usa = 'usa',
    china = 'chn',
    row = 'row',
    canada = 'can',
    mexico = 'mex',
    ftrow = 'fta',
    japan = 'jpn',
    eu = 'eu',
    uk = 'gbr'
  )

  names(country_totals) <- region_map[names(country_totals)]

  return(country_totals)
}

# ============================================================================
# SECTOR OUTPUTS WITH MAPPINGS
# ============================================================================

#' Get sector outputs with full mapping metadata
#'
#' Joins GTAP sector output changes with static mapping file to include
#' aggregate sector and classification flags. Uses VIWS import totals
#' as a proxy for output_baseline (for weighting in aggregate calculations).
#'
#' @param gtap_data Result from read_gtap_full() (must contain viws matrix)
#' @param sector_mapping Data frame with gtap_code and sector flags
#' @param target_region Region to extract (default 'usa')
#' @return Data frame matching sector_outputs.csv structure
get_sector_outputs_full <- function(gtap_data, sector_mapping, target_region = 'usa') {
  # Get base outputs
  outputs <- get_sector_outputs(gtap_data, target_region)

  # Add output_baseline from VIWS (import totals by sector as proxy for weighting)
  if (is.null(gtap_data$viws)) {
    stop('viws not found in GTAP data')
  }
  viws_totals <- rowSums(gtap_data$viws)
  outputs$output_baseline <- viws_totals[match(outputs$gtap_sector, names(viws_totals))]
  if (any(is.na(outputs$output_baseline))) {
    stop('Missing VIWS totals for some sectors in GTAP data')
  }

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
# PRICE EFFECTS (SR AND LR)
# ============================================================================

#' Calculate product price effects
#'
#' Calculates short-run and long-run price effects by product.
#' - SR: Derived from ETR matrix weighted by VIWS imports
#' - LR: Derived from GTAP ppm (import market price) changes
#'
#' @param gtap_data Result from read_gtap_full()
#' @param etr_matrix ETR matrix from Tariff-ETRs (gtap_sector x country)
#' @param product_params Product parameters with is_food flag
#' @param overall_sr_effect Overall short-run price effect (scalar)
#' @param overall_lr_effect Overall long-run price effect (scalar)
#' @param target_region Region to extract (default 'usa')
#' @return Data frame with gtap_sector, sr_price_effect, lr_price_effect, is_food
get_price_effects <- function(gtap_data, etr_matrix, product_params,
                              overall_sr_effect, overall_lr_effect,
                              target_region = 'usa') {
  if (is.null(gtap_data$viws)) {
    stop('viws not found in GTAP data')
  }
  if (is.null(gtap_data$ppm)) {
    stop('ppm not found in GTAP data')
  }

  viws_full <- gtap_data$viws
  ppm_full <- gtap_data$ppm

  # Only process commodities that exist in ETR matrix (goods sectors only)
  all_commodities <- rownames(viws_full)
  goods_sectors <- etr_matrix$gtap_code
  commodities <- intersect(all_commodities, goods_sectors)

  # Filter viws and ppm to goods sectors only
  goods_indices <- match(commodities, all_commodities)
  viws <- viws_full[goods_indices, , drop = FALSE]
  ppm <- ppm_full[goods_indices, , drop = FALSE]

  # Get ppm for target region
  region_idx <- which(colnames(ppm) == target_region)
  ppm_values <- ppm[, region_idx]

  # Calculate weighted average ppm
  total_imports_by_product <- rowSums(viws)
  weighted_avg_ppm <- sum(ppm_values * total_imports_by_product) / sum(total_imports_by_product)

  # LR price effect: scale each product's ppm by overall effect
  lr_effects <- overall_lr_effect * ppm_values / weighted_avg_ppm

  # SR price effect: ETR-weighted calculation
  # Need to match ETR matrix columns to VIWS columns
  sr_effects <- numeric(length(commodities))
  names(sr_effects) <- commodities

  # Country columns in ETR matrix (same names as VIWS columns)
  # ETR matrix has: china, canada, mexico, uk, japan, eu, row, ftrow
  etr_countries <- c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow')

  for (i in seq_along(commodities)) {
    comm <- commodities[i]

    # Get imports for this commodity by country
    comm_imports <- viws[i, ]
    total_comm_imports <- sum(comm_imports)

    if (total_comm_imports == 0) {
      sr_effects[i] <- 0
      next
    }

    # Get ETR row for this commodity (if exists in etr_matrix)
    # Note: etr_matrix uses gtap_code column
    etr_row_idx <- which(etr_matrix$gtap_code == comm)
    if (length(etr_row_idx) == 0) {
      stop('ETR matrix missing gtap_code for sector: ', comm)
    }

    # Calculate weighted ETR for this product
    weighted_etr <- 0
    for (country in etr_countries) {
      if (!country %in% names(etr_matrix)) {
        stop('ETR matrix missing required country column: ', country)
      }
      if (!country %in% names(comm_imports)) {
        stop('VIWS missing required country column: ', country)
      }
      import_weight <- comm_imports[country]
      etr_value <- etr_matrix[[country]][etr_row_idx]
      weighted_etr <- weighted_etr + (etr_value * import_weight)
    }

    sr_effects[i] <- weighted_etr / total_comm_imports
  }

  # Scale SR effects by overall effect ratio
  avg_sr <- sum(sr_effects * total_imports_by_product) / sum(total_imports_by_product)
  if (avg_sr > 0) {
    sr_effects <- overall_sr_effect * sr_effects / avg_sr
  }

  # Build result data frame
  result <- data.frame(
    gtap_sector = commodities,
    sr_price_effect = round(sr_effects * 100, 2),  # Convert to percentage
    lr_price_effect = round(lr_effects, 2),
    stringsAsFactors = FALSE
  )

  # Add is_food from product_params
  required_cols <- c('gtap_sector', 'is_food')
  missing_cols <- setdiff(required_cols, names(product_params))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in product_params: ', paste(missing_cols, collapse = ', '))
  }

  result <- result %>%
    left_join(
      product_params %>% select(gtap_sector, is_food),
      by = 'gtap_sector'
    )

  if (any(is.na(result$is_food))) {
    missing_food <- result$gtap_sector[is.na(result$is_food)]
    stop('Missing is_food flag for gtap_sector(s): ', paste(missing_food, collapse = ', '))
  }

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
#' @param product_params Data frame with product parameters (is_food)
#' @param etr_matrix ETR matrix from Tariff-ETRs (optional, for price effects)
#' @param overall_sr_effect Overall SR price effect (optional)
#' @param overall_lr_effect Overall LR price effect (optional)
#' @return List containing all GTAP outputs
load_gtap_from_files <- function(solution_dir, file_prefix = NULL,
                                 sector_mapping = NULL,
                                 product_params = NULL, etr_matrix = NULL,
                                 overall_sr_effect = NULL, overall_lr_effect = NULL) {
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

  # Read full GTAP data
  gtap_data <- read_gtap_full(sol_path, slc_path, sl4_path)

  result <- list()

  # Foreign GDP
  result$foreign_gdp <- get_foreign_gdp(gtap_data)
  message(sprintf('    - Foreign GDP: %d regions', nrow(result$foreign_gdp)))

  # Import change (qmwreg)
  result$qmwreg <- get_import_change(gtap_data, 'usa')
  message(sprintf('    - Import change (qmwreg): %.2f%%', result$qmwreg))

  # Imports by country
  result$imports_by_country <- get_imports_by_country(gtap_data)
  message(sprintf('    - Imports by country: %d countries', length(result$imports_by_country)))

  if (is.null(sector_mapping)) {
    stop('Sector mapping data is required')
  }
  result$sector_outputs <- get_sector_outputs_full(gtap_data, sector_mapping, 'usa')
  message(sprintf('    - Sector outputs: %d sectors (with mappings)', nrow(result$sector_outputs)))

  # Price effects (if ETR matrix provided)
  if (!is.null(etr_matrix) && !is.null(product_params) &&
      !is.null(overall_sr_effect) && !is.null(overall_lr_effect)) {
    result$product_prices <- get_price_effects(
      gtap_data, etr_matrix, product_params,
      overall_sr_effect, overall_lr_effect, 'usa'
    )
    message(sprintf('    - Product prices: %d products', nrow(result$product_prices)))
  }

  # Raw data for downstream calculations
  result$viws <- gtap_data$viws
  result$vgdp <- gtap_data$vgdp
  result$ppm <- gtap_data$ppm
  result$ppa <- gtap_data$ppa

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

  return(result)
}

