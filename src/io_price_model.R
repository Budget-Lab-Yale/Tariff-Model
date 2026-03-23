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

#' Resolve the BEA I-O data directory based on table level
#'
#' @param level Either 'summary' (73 commodities, 2024) or 'detail' (~400, 2017)
#' @return Path to the data directory
resolve_io_data_dir <- function(level = 'summary') {
  if (level == 'detail') {
    return('resources/io/detail')
  }
  return('resources/io')
}


#' Disaggregate summary-level tau_M to detail-level BEA codes
#'
#' When using detail-level BEA tables, the tariff vector (tau_M) from
#' Tariff-ETRs uses summary BEA codes (e.g., 311FT). This function expands
#' it to detail codes (e.g., 311111, 311119, ...) using the summary-to-detail
#' mapping. Each detail code inherits its parent summary code's tariff.
#'
#' @param tau_M Named vector of tariff shocks keyed by summary BEA codes
#' @param data_dir Path to detail BEA I-O data directory
#' @return Named vector of tariff shocks keyed by detail BEA codes
disaggregate_tau_M <- function(tau_M, data_dir) {

  mapping_path <- file.path(data_dir, 'bea_summary_to_detail.csv')
  if (!file.exists(mapping_path)) {
    stop('Summary-to-detail mapping not found: ', mapping_path,
         '\n  Run python src/util/extract_bea_detail.py')
  }

  mapping <- read_csv(mapping_path, show_col_types = FALSE)

  # For each detail code, look up its summary code's tariff
  detail_tau <- setNames(
    tau_M[mapping$summary_code],
    mapping$detail_code
  )

  # Codes with no tariff (services, etc.) get 0
  detail_tau[is.na(detail_tau)] <- 0

  matched <- sum(mapping$summary_code %in% names(tau_M))
  nonzero <- sum(detail_tau > 0)
  message(sprintf('  Disaggregated tau_M: %d summary -> %d detail codes (%d nonzero)',
                  length(tau_M), length(detail_tau), nonzero))

  return(detail_tau)
}


#' Reaggregate detail commodity prices to summary-level NIPA categories
#'
#' When using detail-level BEA tables, the price model produces ~400 commodity
#' prices and maps them through the 2017 detail PCE bridge (212 NIPA categories).
#' The distribution pipeline requires 2024 summary NIPA line numbers (76 categories)
#' to join with the BLS distributional spending data.
#'
#' This function:
#'   1. Aggregates detail commodity prices to summary BEA codes, weighted by
#'      total commodity use (import + domestic intermediate use)
#'   2. Applies the summary PCE bridge to get 76 categories with correct line numbers
#'
#' @param bea_commodity_prices Named vector of per-commodity price effects (fractional)
#' @param use_import Detail-level CxI import use matrix
#' @param use_domestic Detail-level CxI domestic use matrix
#' @param summary_to_detail Tibble with summary_code, detail_code columns
#' @param summary_bridge Summary-level PCE bridge tibble
#' @param markup_assumption Either 'constant_percentage' or 'constant_dollar'
#'
#' @return Tibble matching pce_category_prices format (nipa_line, pce_category,
#'   sr_price_effect, purchasers_value, pce_share)
reaggregate_to_summary_pce <- function(bea_commodity_prices,
                                       use_import, use_domestic,
                                       summary_to_detail,
                                       summary_bridge,
                                       markup_assumption = 'constant_percentage') {

  # ---- Total commodity use as weights ----
  total_use <- rowSums(use_import) + rowSums(use_domestic)

  # ---- Build detail prices tibble ----
  detail_prices <- tibble(
    detail_code = names(bea_commodity_prices),
    price = as.numeric(bea_commodity_prices),
    use_weight = as.numeric(total_use[names(bea_commodity_prices)])
  ) %>%
    mutate(use_weight = coalesce(use_weight, 0))

  # ---- Aggregate to summary codes ----
  summary_prices <- detail_prices %>%
    inner_join(summary_to_detail, by = 'detail_code') %>%
    group_by(summary_code) %>%
    summarise(
      price = if (sum(use_weight) > 0) {
        sum(price * use_weight) / sum(use_weight)
      } else {
        mean(price)
      },
      total_use = sum(use_weight),
      n_detail = n(),
      .groups = 'drop'
    ) %>%
    rename(bea_code = summary_code)

  nonzero <- sum(summary_prices$price > 0)
  message(sprintf('  Reaggregated %d detail -> %d summary commodity prices (%d nonzero)',
                  nrow(detail_prices), nrow(summary_prices), nonzero))

  # ---- Apply summary PCE bridge (C matrix) ----
  if (markup_assumption == 'constant_dollar') {
    numerator_col <- 'producers_value'
  } else {
    numerator_col <- 'purchasers_value'
  }

  pce_category_prices <- summary_bridge %>%
    left_join(summary_prices %>% select(bea_code, price), by = 'bea_code') %>%
    mutate(price = coalesce(price, 0)) %>%
    group_by(nipa_line, pce_category) %>%
    summarise(
      sr_price_effect = if (sum(abs(.data[[numerator_col]])) > 0) {
        sum(price * .data[[numerator_col]]) / sum(purchasers_value)
      } else {
        0
      },
      purchasers_value = sum(purchasers_value),
      n_commodities = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      sr_price_effect = sr_price_effect * 100,
      pce_share = purchasers_value / sum(purchasers_value) * 100
    ) %>%
    arrange(nipa_line)

  message(sprintf('  Summary PCE categories for distribution: %d categories',
                  nrow(pce_category_prices)))

  return(pce_category_prices)
}


#' Load BEA requirements matrix (Leontief inverse)
#'
#' @param type Either 'domestic' or 'total'
#' @param data_dir Path to BEA I-O data directory
#' @return Named matrix (rows = industries, cols = commodities)
load_bea_requirements <- function(type = 'domestic', data_dir = 'resources/io') {

  valid_types <- c('domestic', 'total')
  if (!type %in% valid_types) {
    stop('Invalid BEA requirements type: ', type, '. Must be one of: ',
         paste(valid_types, collapse = ', '))
  }

  path <- file.path(data_dir, paste0('bea_requirements_', type, '.csv'))
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
#' @param data_dir Path to BEA I-O data directory
#' @return Tibble with gtap_code, bea_code, bea_description
load_gtap_bea_crosswalk <- function(data_dir = 'resources/io') {

  path <- file.path(data_dir, 'gtap_bea_crosswalk.csv')
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
#' @param data_dir Path to BEA I-O data directory
#' @return Named matrix (rows = BEA commodity codes, cols = BEA industry codes)
load_bea_use_data <- function(type, data_dir = 'resources/io') {

  valid_types <- c('import', 'domestic')
  if (!type %in% valid_types) {
    stop('Invalid BEA use table type: ', type, '. Must be one of: ',
         paste(valid_types, collapse = ', '))
  }

  path <- file.path(data_dir, paste0('bea_use_', type, '.csv'))
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
#' @param data_dir Path to BEA I-O data directory
#' @return Named numeric vector (names = bea_code, values = pce)
load_bea_pce_weights <- function(data_dir = 'resources/io') {

  path <- file.path(data_dir, 'bea_pce_by_commodity.csv')
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
#' @param data_dir Path to BEA I-O data directory
#' @return Named numeric vector (names = bea_code, values = output)
load_bea_industry_output <- function(data_dir = 'resources/io') {

  path <- file.path(data_dir, 'bea_industry_output.csv')
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
#' @param data_dir Path to BEA I-O data directory
#' @return Tibble with nipa_line, pce_category, bea_code, commodity_description,
#'   producers_value, purchasers_value
load_pce_bridge <- function(data_dir = 'resources/io') {

  path <- file.path(data_dir, 'pce_bridge.csv')
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
#' The markup_assumption parameter controls how distribution margins respond
#' to cost changes (Barbiero & Stein Section "Response of Prices to Tariffs"):
#'   - 'constant_percentage': margins scale proportionally with cost (upper bound).
#'     A 10% cost increase on a $1 good with $1 markup -> $2.20 (+10%).
#'   - 'constant_dollar': margins stay fixed in dollar terms (lower bound).
#'     A 10% cost increase on a $1 good with $1 markup -> $2.10 (+5%).
#'
#' @param tau_M Named vector of tariff shocks by BEA commodity (fractional)
#' @param B_MD CxI import input coefficients matrix
#' @param leontief_domestic IxC domestic requirements matrix
#' @param omega_M Named vector of import shares by commodity
#' @param omega_D Named vector of domestic shares by commodity
#' @param pce_bridge PCE bridge tibble from load_pce_bridge()
#' @param usd_offset Scalar USD appreciation offset (e.g., 0.174)
#' @param markup_assumption Either 'constant_percentage' (default, upper bound)
#'   or 'constant_dollar' (lower bound)
#'
#' @return List with:
#'   - pce_category_prices: tibble (nipa_line, pce_category, sr_price_effect [pp],
#'     purchasers_value, pce_share)
#'   - bea_commodity_prices: named vector of per-commodity price effects (fractional)
#'   - aggregate: PCE-weighted aggregate price effect (fractional)
#'   - direct_aggregate: PCE-weighted aggregate from direct effects only (fractional)
#'   - supply_chain_aggregate: PCE-weighted aggregate from supply chain only (fractional)
#'   - markup_assumption: which assumption was used
compute_boston_fed_prices <- function(tau_M, B_MD, leontief_domestic,
                                     omega_M, omega_D, pce_bridge,
                                     usd_offset,
                                     markup_assumption = 'constant_percentage') {

  valid_markups <- c('constant_percentage', 'constant_dollar')
  if (!markup_assumption %in% valid_markups) {
    stop('Invalid markup_assumption: ', markup_assumption,
         '. Must be one of: ', paste(valid_markups, collapse = ', '))
  }

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

  # ---- Markup assumption: choose numerator weight ----
  # Constant-percentage: weight by purchasers_value (margins scale with cost)
  # Constant-dollar: weight by producers_value (margins stay fixed in $)
  # Denominator is always purchasers_value (what consumers actually pay)
  if (markup_assumption == 'constant_dollar') {
    numerator_col <- 'producers_value'
  } else {
    numerator_col <- 'purchasers_value'
  }

  # ---- Derive per-commodity weights from bridge ----
  commodity_weights <- pce_bridge %>%
    group_by(bea_code) %>%
    summarise(
      numerator_wt = sum(.data[[numerator_col]]),
      purchasers_wt = sum(purchasers_value),
      .groups = 'drop'
    )
  num_lookup <- setNames(commodity_weights$numerator_wt, commodity_weights$bea_code)
  pv_lookup <- setNames(commodity_weights$purchasers_wt, commodity_weights$bea_code)

  num_aligned <- rep(0, length(commodities))
  names(num_aligned) <- commodities
  pv_aligned <- rep(0, length(commodities))
  names(pv_aligned) <- commodities

  matched_codes <- intersect(names(num_lookup), commodities)
  num_aligned[matched_codes] <- num_lookup[matched_codes]
  pv_aligned[matched_codes] <- pv_lookup[matched_codes]

  total_pce <- sum(pv_aligned)
  if (total_pce == 0) {
    stop('Total PCE weight is zero - cannot compute aggregate')
  }

  aggregate <- sum(total * num_aligned) / total_pce
  direct_aggregate <- sum(direct_scaled * num_aligned) / total_pce
  supply_chain_aggregate <- sum(supply_chain_scaled * num_aligned) / total_pce

  # ---- Map to PCE categories via bridge (the C matrix) ----
  price_df <- tibble(
    bea_code = commodities,
    price = as.numeric(total)
  )

  pce_category_prices <- pce_bridge %>%
    left_join(price_df, by = 'bea_code') %>%
    # BEA commodities not in the tariff vector (services, etc.) have zero tariff shock
    mutate(price = coalesce(price, 0)) %>%
    group_by(nipa_line, pce_category) %>%
    summarise(
      sr_price_effect = if (sum(abs(.data[[numerator_col]])) > 0) {
        sum(price * .data[[numerator_col]]) / sum(purchasers_value)
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
  message(sprintf('  Boston Fed price effects (%s markups):', markup_assumption))
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
    supply_chain_aggregate = supply_chain_aggregate,
    markup_assumption = markup_assumption
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

  unmatched_bea <- setdiff(names(tau_M_presub), names(ratio_lookup))
  nonzero_unmatched <- unmatched_bea[tau_M_presub[unmatched_bea] > 0]
  message(sprintf('  Post-sub tau_M: %d BEA commodities adjusted, mean ratio = %.4f',
                  length(matched), mean(ratio_lookup[matched])))
  if (length(nonzero_unmatched) > 0) {
    message(sprintf('    Warning: %d BEA codes with nonzero tariff not in crosswalk (kept at pre-sub): %s',
                    length(nonzero_unmatched),
                    paste(head(nonzero_unmatched, 5), collapse = ', ')))
  }

  return(tau_M_postsub)
}


#' Compute post-substitution omega_M using GTAP NVPP import share changes
#'
#' For each BEA commodity, scales the import share (omega_M) by the ratio
#' of post-simulation to baseline import shares from GTAP NVPP data. This
#' captures both "imports fell" and "domestic production rose" -- the full
#' GE response.
#'
#' When multiple GTAP sectors map to one BEA commodity, ratios are averaged
#' weighted by baseline imports (consistent with compute_postsub_tau_M).
#'
#' @param omega_M Named vector of import shares by BEA commodity
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns
#' @param nvpp_commodity_ratio Named vector of per-GTAP-commodity import share
#'   ratios (postsim_share / baseline_share), names = GTAP commodity codes
#' @param viws_baseline Matrix [commodity x source_region] of baseline imports
#'   (for weighting when multiple GTAP sectors map to one BEA commodity)
#'
#' @return Named vector of post-sub import shares by BEA commodity (capped at 1.0)
compute_postsub_omega_M <- function(omega_M, gtap_bea_crosswalk,
                                    nvpp_commodity_ratio, viws_baseline) {

  crosswalk <- gtap_bea_crosswalk %>%
    mutate(gtap_code = toupper(gtap_code))

  # Uppercase NVPP names to match crosswalk
  names(nvpp_commodity_ratio) <- toupper(names(nvpp_commodity_ratio))

  # Get per-GTAP-sector baseline import totals for weighting
  baseline_totals <- rowSums(viws_baseline)
  names(baseline_totals) <- toupper(names(baseline_totals))

  # Build BEA-level ratios from GTAP commodity ratios (import-weighted)
  bea_ratios <- crosswalk %>%
    mutate(
      ratio = nvpp_commodity_ratio[gtap_code],
      baseline_imports = baseline_totals[gtap_code]
    ) %>%
    filter(!is.na(ratio)) %>%
    mutate(baseline_imports = coalesce(baseline_imports, 0)) %>%
    group_by(bea_code) %>%
    summarise(
      ratio = if (sum(baseline_imports) > 0) {
        sum(ratio * baseline_imports) / sum(baseline_imports)
      } else {
        mean(ratio)
      },
      .groups = 'drop'
    )

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
