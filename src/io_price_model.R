# =============================================================================
# io_price_model.R - Boston Fed I-O Price Model (Barbiero & Stein 2025)
# =============================================================================
#
# Implements the Boston Fed approach to computing tariff price effects using
# input-output tables. Produces consumer price effects at the NIPA PCE
# category level (76 categories).
#
# The key formula (Leontief price model with import/domestic decomposition):
#
#   P_commodity = [omega_M * tau + omega_D * (L_D' %*% B_MD' %*% tau)] * (1 - usd_offset)
#   P_pce_cat   = C %*% P_commodity  (weighted by PCE bridge purchaser's values)
#
# Where:
#   tau         = vector of tariff shocks by BEA commodity
#   omega_M     = import share of commodity supply
#   omega_D     = domestic share of commodity supply
#   B_MD        = CxI import input coefficients matrix
#   L_D         = IxC domestic Leontief inverse (requirements matrix)
#   C           = PCE bridge table (BEA commodities -> NIPA consumer categories)
#   usd_offset  = dollar appreciation offset
#
# Post-substitution adjustments use GTAP GE results to scale tau_M and omega_M:
#   - tau_M_post: scaled by GTAP import volume ratios (viws_postsim / viws_baseline)
#   - omega_M_post: scaled by GTAP NVPP import share ratios
#
# Data sources:
#   - BEA Use tables (import and domestic, before redefinitions)
#   - BEA industry output
#   - BEA domestic requirements matrix (IxC)
#   - BEA PCE bridge (consumption weights by commodity)
#   - GTAP-BEA crosswalk (for post-substitution adjustments)
#
# =============================================================================

library(tidyverse)


# ==== Loading functions ======================================================

#' Load BEA requirements matrix (Leontief inverse)
#'
#' @param type Either 'domestic' or 'total'
#' @return Named matrix (rows = industries, cols = commodities)
load_bea_requirements <- function(type = 'domestic') {

  valid_types <- c('domestic', 'total')
  if (!type %in% valid_types) {
    stop('Invalid BEA requirements type: ', type, '. Must be one of: ',
         paste(valid_types, collapse = ', '))
  }

  path <- file.path('resources', 'io', paste0('bea_requirements_', type, '.csv'))
  if (!file.exists(path)) {
    stop('BEA requirements file not found: ', path)
  }

  data <- read_csv(path, show_col_types = FALSE)
  bea_codes <- data$bea_code

  mat <- as.matrix(data[, -1])
  rownames(mat) <- bea_codes

  message(sprintf('  Loaded BEA %s requirements: %d x %d', type, nrow(mat), ncol(mat)))
  return(mat)
}


#' Load GTAP-BEA crosswalk (full, including services)
#'
#' @return Tibble with gtap_code, bea_code, bea_description
load_gtap_bea_crosswalk <- function() {

  path <- 'resources/io/gtap_bea_crosswalk.csv'
  if (!file.exists(path)) {
    stop('GTAP-BEA crosswalk not found: ', path)
  }

  crosswalk <- read_csv(path, show_col_types = FALSE)
  message(sprintf('  Loaded GTAP-BEA crosswalk: %d mappings', nrow(crosswalk)))
  return(crosswalk)
}


#' Load BEA Use table (import or domestic)
#'
#' Reads the commodity-by-industry Use table from CSV. First column is
#' bea_code (commodity), remaining columns are BEA industry codes.
#'
#' @param type Either 'import' or 'domestic'
#' @return Named matrix (rows = BEA commodity codes, cols = BEA industry codes)
load_bea_use_data <- function(type) {

  valid_types <- c('import', 'domestic')
  if (!type %in% valid_types) {
    stop('Invalid BEA use table type: ', type, '. Must be one of: ',
         paste(valid_types, collapse = ', '))
  }

  path <- file.path('resources', 'io', paste0('bea_use_', type, '.csv'))
  if (!file.exists(path)) {
    stop('BEA use table not found: ', path)
  }

  data <- read_csv(path, show_col_types = FALSE)
  bea_codes <- data$bea_code

  mat <- as.matrix(data[, -1])
  rownames(mat) <- bea_codes

  message(sprintf('  Loaded BEA %s use table: %d commodities x %d industries',
                  type, nrow(mat), ncol(mat)))
  return(mat)
}


#' Load BEA PCE weights by commodity
#'
#' Reads PCE (personal consumption expenditure) by BEA commodity code.
#'
#' @return Named numeric vector (names = bea_code, values = pce)
load_bea_pce_weights <- function() {

  path <- 'resources/io/bea_pce_by_commodity.csv'
  if (!file.exists(path)) {
    stop('BEA PCE weights file not found: ', path)
  }

  data <- read_csv(path, show_col_types = FALSE)
  weights <- setNames(data$pce, data$bea_code)

  message(sprintf('  Loaded BEA PCE weights: %d commodities, total PCE = %.1f',
                  length(weights), sum(weights)))
  return(weights)
}


#' Load BEA industry output
#'
#' Reads total output by BEA industry code.
#'
#' @return Named numeric vector (names = bea_code, values = output)
load_bea_industry_output <- function() {

  path <- 'resources/io/bea_industry_output.csv'
  if (!file.exists(path)) {
    stop('BEA industry output file not found: ', path)
  }

  data <- read_csv(path, show_col_types = FALSE)
  output <- setNames(data$output, data$bea_code)

  message(sprintf('  Loaded BEA industry output: %d industries, total = %.1f',
                  length(output), sum(output)))
  return(output)
}


#' Load PCE Bridge Table (BEA commodity -> consumer PCE category)
#'
#' @return Tibble with nipa_line, pce_category, bea_code, commodity_description,
#'   producers_value, purchasers_value
load_pce_bridge <- function() {

  path <- 'resources/io/pce_bridge_2024.csv'
  if (!file.exists(path)) {
    stop('PCE bridge file not found: ', path,
         '\n  Run scripts/build_use_table_data.R or extract from PCEBridge_Summary.xlsx')
  }

  bridge <- read_csv(path, show_col_types = FALSE)
  message(sprintf('  Loaded PCE bridge: %d mappings across %d consumer categories',
                  nrow(bridge), length(unique(bridge$pce_category))))
  return(bridge)
}


# ==== Core computation =======================================================

#' Build Boston Fed matrices from BEA Use tables
#'
#' Computes the import input coefficients matrix (B_MD), domestic input
#' coefficients matrix (B_D), and import/domestic shares of commodity supply
#' (omega_M, omega_D).
#'
#' @param use_import CxI matrix of imported commodity use by industry
#' @param use_domestic CxI matrix of domestic commodity use by industry
#' @param industry_output Named numeric vector of total output by industry
#'
#' @return List with:
#'   - B_MD: CxI import input coefficients matrix
#'   - B_D: CxI domestic input coefficients matrix
#'   - omega_M: named vector of import shares by commodity
#'   - omega_D: named vector of domestic shares by commodity
build_boston_fed_matrices <- function(use_import, use_domestic, industry_output) {

  if (ncol(use_import) != ncol(use_domestic)) {
    stop('Import and domestic use tables have different numbers of industries')
  }
  if (nrow(use_import) != nrow(use_domestic)) {
    stop('Import and domestic use tables have different numbers of commodities')
  }

  # Align industry output to column order of use tables
  industry_codes <- colnames(use_import)
  output_vec <- industry_output[industry_codes]

  missing_industries <- industry_codes[is.na(output_vec)]
  if (length(missing_industries) > 0) {
    stop('Industry output missing for: ',
         paste(missing_industries, collapse = ', '))
  }

  # ---- Compute input coefficients matrices ----
  output_matrix <- matrix(
    rep(output_vec, each = nrow(use_import)),
    nrow = nrow(use_import),
    ncol = ncol(use_import)
  )

  B_MD <- use_import / output_matrix
  B_D <- use_domestic / output_matrix

  rownames(B_MD) <- rownames(use_import)
  colnames(B_MD) <- industry_codes
  rownames(B_D) <- rownames(use_domestic)
  colnames(B_D) <- industry_codes

  # ---- Compute import/domestic shares by commodity ----
  import_totals <- rowSums(use_import)
  domestic_totals <- rowSums(use_domestic)
  total_use <- import_totals + domestic_totals

  omega_M <- import_totals / total_use
  omega_D <- domestic_totals / total_use

  # Handle commodities with zero total use
  zero_use <- total_use == 0
  if (any(zero_use)) {
    message(sprintf('    Warning: %d commodities with zero total use, setting omega to 0',
                    sum(zero_use)))
    omega_M[zero_use] <- 0
    omega_D[zero_use] <- 0
  }

  message(sprintf('  Built Boston Fed matrices:'))
  message(sprintf('    B_MD: %d x %d (CxI)', nrow(B_MD), ncol(B_MD)))
  message(sprintf('    B_D:  %d x %d (CxI)', nrow(B_D), ncol(B_D)))
  message(sprintf('    omega_M range: [%.4f, %.4f], mean: %.4f',
                  min(omega_M), max(omega_M), mean(omega_M)))
  message(sprintf('    omega_D range: [%.4f, %.4f], mean: %.4f',
                  min(omega_D), max(omega_D), mean(omega_D)))

  return(list(
    B_MD = B_MD,
    B_D = B_D,
    omega_M = omega_M,
    omega_D = omega_D
  ))
}


#' Compute Boston Fed price effects from tariff shocks
#'
#' Applies the Barbiero & Stein (2025) Leontief price model to compute
#' consumer price effects at the NIPA PCE category level. Decomposes into
#' direct (imported final goods) and supply chain (imported intermediates
#' propagated through domestic production) components, then maps to consumer
#' categories via the PCE bridge table (the C matrix).
#'
#' @param tau_M Named vector of tariff shocks by BEA commodity (fractional)
#' @param B_MD CxI import input coefficients matrix
#' @param leontief_domestic IxC domestic requirements matrix
#' @param omega_M Named vector of import shares by commodity
#' @param omega_D Named vector of domestic shares by commodity
#' @param pce_bridge PCE bridge tibble from load_pce_bridge()
#' @param usd_offset Scalar USD appreciation offset (e.g., 0.174)
#'
#' @return List with:
#'   - pce_category_prices: tibble (nipa_line, pce_category, sr_price_effect [pp],
#'     purchasers_value, pce_share)
#'   - bea_commodity_prices: named vector of per-commodity price effects (fractional)
#'   - aggregate: PCE-weighted aggregate price effect (fractional)
#'   - direct_aggregate: PCE-weighted aggregate from direct effects only (fractional)
#'   - supply_chain_aggregate: PCE-weighted aggregate from supply chain only (fractional)
compute_boston_fed_prices <- function(tau_M, B_MD, leontief_domestic,
                                     omega_M, omega_D, pce_bridge,
                                     usd_offset) {

  # Align tau_M to commodity dimension of B_MD
  commodities <- rownames(B_MD)
  tau_aligned <- rep(0, length(commodities))
  names(tau_aligned) <- commodities

  matched <- intersect(names(tau_M), commodities)
  tau_aligned[matched] <- tau_M[matched]

  unmatched <- setdiff(names(tau_M), commodities)
  if (length(unmatched) > 0) {
    message(sprintf('    Warning: %d tariff codes not in B_MD commodities: %s',
                    length(unmatched),
                    paste(head(unmatched, 5), collapse = ', ')))
  }

  # Align omega vectors
  omega_M_aligned <- omega_M[commodities]
  omega_D_aligned <- omega_D[commodities]

  # ---- Direct effect: omega_M * tau ----
  direct <- omega_M_aligned * tau_aligned

  # ---- Supply chain effect ----
  # Step 1: Cost increase in each industry from imported inputs
  # B_MD' is IxC, tau is Cx1 -> B_MD' %*% tau gives Ix1
  imported_input_cost <- as.numeric(t(B_MD) %*% tau_aligned)
  names(imported_input_cost) <- colnames(B_MD)

  # Step 2: Propagate through domestic Leontief inverse
  # L_D is IxC, need L_D' %*% imported_input_cost -> Cx1
  leontief_industries <- rownames(leontief_domestic)
  bmd_industries <- colnames(B_MD)
  common_industries <- intersect(bmd_industries, leontief_industries)

  if (length(common_industries) < length(bmd_industries)) {
    message(sprintf('    Aligning industry dimensions: %d common of %d B_MD / %d L_D',
                    length(common_industries), length(bmd_industries),
                    length(leontief_industries)))
  }

  cost_aligned <- rep(0, length(leontief_industries))
  names(cost_aligned) <- leontief_industries
  cost_aligned[common_industries] <- imported_input_cost[common_industries]

  propagated <- as.numeric(t(leontief_domestic) %*% cost_aligned)
  names(propagated) <- colnames(leontief_domestic)

  # Align propagated to commodity dimension
  propagated_aligned <- rep(0, length(commodities))
  names(propagated_aligned) <- commodities
  prop_matched <- intersect(names(propagated), commodities)
  propagated_aligned[prop_matched] <- propagated[prop_matched]

  supply_chain <- omega_D_aligned * propagated_aligned

  # ---- Apply USD offset ----
  total <- (direct + supply_chain) * (1 - usd_offset)
  direct_scaled <- direct * (1 - usd_offset)
  supply_chain_scaled <- supply_chain * (1 - usd_offset)

  # ---- Derive per-commodity PCE weights from bridge ----
  commodity_pce <- pce_bridge %>%
    group_by(bea_code) %>%
    summarise(pce = sum(purchasers_value), .groups = 'drop')
  pce_lookup <- setNames(commodity_pce$pce, commodity_pce$bea_code)

  pce_aligned <- rep(0, length(commodities))
  names(pce_aligned) <- commodities
  pce_matched <- intersect(names(pce_lookup), commodities)
  pce_aligned[pce_matched] <- pce_lookup[pce_matched]

  total_pce <- sum(pce_aligned)
  if (total_pce == 0) {
    stop('Total PCE weight is zero - cannot compute aggregate')
  }

  aggregate <- sum(total * pce_aligned) / total_pce
  direct_aggregate <- sum(direct_scaled * pce_aligned) / total_pce
  supply_chain_aggregate <- sum(supply_chain_scaled * pce_aligned) / total_pce

  # ---- Map to PCE categories via bridge (the C matrix) ----
  price_df <- tibble(
    bea_code = commodities,
    price = as.numeric(total)
  )

  pce_category_prices <- pce_bridge %>%
    left_join(price_df, by = 'bea_code') %>%
    mutate(price = coalesce(price, 0)) %>%
    group_by(nipa_line, pce_category) %>%
    summarise(
      sr_price_effect = if (sum(abs(purchasers_value)) > 0) {
        sum(price * purchasers_value) / sum(purchasers_value)
      } else {
        0
      },
      purchasers_value = sum(purchasers_value),
      n_commodities = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      sr_price_effect = sr_price_effect * 100,  # Convert to pp
      pce_share = purchasers_value / sum(purchasers_value) * 100
    ) %>%
    arrange(nipa_line)

  # ---- Diagnostics ----
  message(sprintf('  Boston Fed price effects:'))
  message(sprintf('    Commodities with nonzero tariff: %d of %d',
                  sum(tau_aligned > 0), length(tau_aligned)))
  message(sprintf('    Aggregate price effect: %.4f%%', aggregate * 100))
  message(sprintf('    Direct component:       %.4f%%', direct_aggregate * 100))
  message(sprintf('    Supply chain component: %.4f%%', supply_chain_aggregate * 100))
  message(sprintf('    USD offset applied:     %.3f', usd_offset))
  message(sprintf('    PCE categories:         %d', nrow(pce_category_prices)))

  return(list(
    pce_category_prices = pce_category_prices,
    bea_commodity_prices = total,
    aggregate = aggregate,
    direct_aggregate = direct_aggregate,
    supply_chain_aggregate = supply_chain_aggregate
  ))
}


# ==== Post-substitution adjustments ==========================================

#' Compute post-substitution tau_M using GTAP import volume changes
#'
#' For each BEA commodity, scales the pre-sub tariff shock by the ratio of
#' post-simulation to baseline import volumes from GTAP. This captures the
#' effect of trade diversion: sectors with larger import declines get smaller
#' effective tariff shocks.
#'
#' @param tau_M_presub Named vector of pre-sub tariff shocks by BEA commodity
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns
#' @param viws_postsim Matrix [commodity x source_region] of post-sim imports
#' @param viws_baseline Matrix [commodity x source_region] of baseline imports
#'
#' @return Named vector of post-sub tariff shocks by BEA commodity
compute_postsub_tau_M <- function(tau_M_presub, gtap_bea_crosswalk,
                                  viws_postsim, viws_baseline) {

  # Per-GTAP-sector total import ratio
  baseline_totals <- rowSums(viws_baseline)
  postsim_totals <- rowSums(viws_postsim)

  gtap_ratios <- tibble(
    gtap_code = toupper(rownames(viws_baseline)),
    import_ratio = postsim_totals / baseline_totals,
    baseline_imports = baseline_totals
  ) %>%
    mutate(import_ratio = if_else(
      is.nan(import_ratio) | is.infinite(import_ratio), 1.0, import_ratio
    ))

  # Map to BEA: weighted average of GTAP ratios within each BEA group
  bea_ratios <- gtap_bea_crosswalk %>%
    mutate(gtap_code = toupper(gtap_code)) %>%
    inner_join(gtap_ratios, by = 'gtap_code') %>%
    group_by(bea_code) %>%
    summarise(
      ratio = if (sum(baseline_imports) > 0) {
        sum(import_ratio * baseline_imports) / sum(baseline_imports)
      } else {
        mean(import_ratio)
      },
      .groups = 'drop'
    )

  ratio_lookup <- setNames(bea_ratios$ratio, bea_ratios$bea_code)

  # Apply ratios to tau_M
  tau_M_postsub <- tau_M_presub
  matched <- intersect(names(tau_M_postsub), names(ratio_lookup))
  tau_M_postsub[matched] <- tau_M_presub[matched] * ratio_lookup[matched]

  message(sprintf('  Post-sub tau_M: %d BEA commodities adjusted, mean ratio = %.4f',
                  length(matched), mean(ratio_lookup[matched])))

  return(tau_M_postsub)
}


#' Compute post-substitution omega_M using GTAP NVPP import share changes
#'
#' For each BEA commodity, scales the import share (omega_M) by the ratio
#' of post-simulation to baseline import shares from GTAP NVPP data. This
#' captures both "imports fell" and "domestic production rose" -- the full
#' GE response.
#'
#' @param omega_M Named vector of import shares by BEA commodity
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns
#' @param nvpp_commodity_ratio Named vector of per-GTAP-commodity import share
#'   ratios (postsim_share / baseline_share), names = GTAP commodity codes
#'
#' @return Named vector of post-sub import shares by BEA commodity (capped at 1.0)
compute_postsub_omega_M <- function(omega_M, gtap_bea_crosswalk,
                                    nvpp_commodity_ratio) {

  crosswalk <- gtap_bea_crosswalk %>%
    mutate(gtap_code = toupper(gtap_code))

  # Uppercase NVPP names to match crosswalk
  names(nvpp_commodity_ratio) <- toupper(names(nvpp_commodity_ratio))

  # Build BEA-level ratios from GTAP commodity ratios
  bea_ratios <- crosswalk %>%
    mutate(ratio = nvpp_commodity_ratio[gtap_code]) %>%
    filter(!is.na(ratio)) %>%
    group_by(bea_code) %>%
    summarise(ratio = mean(ratio), .groups = 'drop')

  ratio_lookup <- setNames(bea_ratios$ratio, bea_ratios$bea_code)

  omega_M_post <- omega_M
  matched <- intersect(names(omega_M_post), names(ratio_lookup))
  omega_M_post[matched] <- omega_M[matched] * ratio_lookup[matched]

  # Cap at 1.0
  omega_M_post <- pmin(omega_M_post, 1.0)

  message(sprintf('  Post-sub omega_M: %d BEA commodities adjusted, mean ratio = %.4f',
                  length(matched), mean(ratio_lookup[matched])))

  return(omega_M_post)
}
