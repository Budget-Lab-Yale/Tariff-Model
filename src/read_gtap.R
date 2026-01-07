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
  viws = 'u028',      # VALIMPORTS - imports by [comm, src, dst] (65 x 9 x 9)
  vgdp = '0161'       # GDP levels by region ($millions, 9 values)
)

# Region codes in GTAP model order
GTAP_REGIONS <- c('usa', 'china', 'row', 'canada', 'mexico', 'ftrow', 'japan', 'eu', 'uk')

# ============================================================================
# MAIN FUNCTIONS
# ============================================================================

#' Read GTAP solution files
#'
#' @param sol_path Path to .sol file
#' @param sl4_path Path to .sl4 file (optional, for metadata)
#' @return List containing extracted GTAP data
read_gtap_solution <- function(sol_path, sl4_path = NULL) {
  # Read solution file
  sol <- read_har(sol_path)

  # Get set labels from sl4 if provided
  if (!is.null(sl4_path) && file.exists(sl4_path)) {
    sl4 <- read_har(sl4_path)
    stel <- sl4[['stel']]
    regions <- stel[1:9]
    commodities <- stel[10:74]
  } else {
    regions <- GTAP_REGIONS
    commodities <- NULL
  }

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
read_viws <- function(slc_path, sl4_path = NULL, target_region = 1) {
  slc <- read_har(slc_path)

  # Get set labels from sl4 if provided
  if (!is.null(sl4_path) && file.exists(sl4_path)) {
    sl4 <- read_har(sl4_path)
    stel <- sl4[['stel']]
    regions <- stel[1:9]
    commodities <- stel[10:74]
  } else {
    regions <- GTAP_REGIONS
    commodities <- NULL
  }

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
read_vgdp <- function(slc_path, sl4_path = NULL) {
  slc <- read_har(slc_path)

  # Get region labels from sl4 if provided
 if (!is.null(sl4_path) && file.exists(sl4_path)) {
    sl4 <- read_har(sl4_path)
    regions <- sl4[['stel']][1:9]
  } else {
    regions <- GTAP_REGIONS
  }

  # Extract VGDP
  vgdp_header <- GTAP_SLC_HEADERS$vgdp
  if (is.null(slc[[vgdp_header]])) {
    stop(paste('VGDP header not found:', vgdp_header))
  }

  vgdp <- as.vector(slc[[vgdp_header]])
  names(vgdp) <- regions

  return(vgdp)
}

#' Read GTAP solution with VIWS and VGDP
#'
#' Extended version that also reads .slc file for level values.
#'
#' @param sol_path Path to .sol file
#' @param slc_path Path to .slc file
#' @param sl4_path Path to .sl4 file (optional, for metadata)
#' @return List containing all extracted GTAP data including viws and vgdp
read_gtap_full <- function(sol_path, slc_path, sl4_path = NULL) {
  # Get base solution data
  result <- read_gtap_solution(sol_path, sl4_path)

  # Add VIWS from .slc file
  if (file.exists(slc_path)) {
    result$viws <- read_viws(slc_path, sl4_path, target_region = 1)
    result$vgdp <- read_vgdp(slc_path, sl4_path)
  }

  return(result)
}

# ============================================================================
# CONVENIENCE FUNCTION
# ============================================================================

#' Load all GTAP outputs for a scenario
#'
#' @param scenario_dir Directory containing GTAP output files
#' @return List with foreign_gdp, sector_outputs, import_change
load_gtap_outputs <- function(scenario_dir) {
  # Look for .sol and .sl4 files
  sol_files <- list.files(scenario_dir, pattern = '\\.sol$', full.names = TRUE)
  sl4_files <- list.files(scenario_dir, pattern = '\\.sl4$', full.names = TRUE)

  if (length(sol_files) == 0) {
    stop(paste('No .sol files found in:', scenario_dir))
  }

  sol_path <- sol_files[1]
  sl4_path <- if (length(sl4_files) > 0) sl4_files[1] else NULL

  # Read solution
  gtap_data <- read_gtap_solution(sol_path, sl4_path)

  # Extract key outputs
  result <- list(
    foreign_gdp = get_foreign_gdp(gtap_data),
    sector_outputs = get_sector_outputs(gtap_data, 'usa'),
    import_change = get_import_change(gtap_data, 'usa'),
    import_prices = get_import_prices(gtap_data, 'usa')
  )

  return(result)
}
