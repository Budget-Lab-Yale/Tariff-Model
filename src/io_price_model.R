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
#   P_commodity = [omega_M * tau + omega_D * (L_D' %*% B_MD' %*% tau)] * (1 + domestic_pricing - usd_offset)
#   P_pce_cat   = C %*% P_commodity  (weighted by PCE bridge purchaser's values)
#
# Where:
#   tau         = vector of tariff shocks by BEA commodity
#   omega_M     = import share of commodity supply
#   omega_D     = domestic share of commodity supply
#   B_MD        = CxI import input coefficients matrix
#   L_D         = IxC domestic Leontief inverse (requirements matrix)
#   C           = PCE bridge table (BEA commodities -> NIPA consumer categories)
#   usd_offset        = dollar appreciation offset
#   domestic_pricing  = domestic competitive pricing effect (0.5)
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


#' Load full commodity use totals (intermediate + final demand)
#'
#' Per Boston Fed appendix (April 2025), omega_M should reflect total commodity
#' absorption including final demand (PCE, investment, government, exports),
#' not just intermediate use. This file provides the full totals.
#'
#' @param data_dir Path to BEA I-O data directory
#' @return Tibble with bea_code, import_total, domestic_total
load_bea_commodity_use_totals <- function(data_dir = 'resources/io') {

  path <- file.path(data_dir, 'bea_commodity_use_totals.csv')
  if (!file.exists(path)) {
    stop('bea_commodity_use_totals.csv not found in ', data_dir,
         '\n  omega_M requires total commodity absorption (incl final demand)',
         '\n  Run: python src/util/extract_bea_totals.py --level <summary|detail>')
  }

  data <- read_csv(path, show_col_types = FALSE)
  message(sprintf('  Loaded commodity use totals (incl final demand): %d commodities',
                  nrow(data)))
  return(data)
}


#' Load industry variable cost (for constant-percentage B normalization)
#'
#' Per Boston Fed appendix (April 2025), B matrices under constant-percentage
#' markup are normalized by total intermediates + compensation of employees,
#' not by total industry output.
#'
#' @param data_dir Path to BEA I-O data directory
#' @return Named numeric vector (names = bea_code, values = variable_cost)
load_bea_industry_variable_cost <- function(data_dir = 'resources/io') {

  path <- file.path(data_dir, 'bea_industry_variable_cost.csv')
  if (!file.exists(path)) {
    stop('bea_industry_variable_cost.csv not found in ', data_dir,
         '\n  B matrix normalization requires variable cost data',
         '\n  Run: python src/util/extract_bea_totals.py --level <summary|detail>')
  }

  data <- read_csv(path, show_col_types = FALSE)
  vc <- setNames(data$variable_cost, data$bea_code)

  message(sprintf('  Loaded industry variable cost: %d industries, total = %.1f',
                  length(vc), sum(vc)))
  return(vc)
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


# ==== Mapping helpers (GTAP -> BEA, BEA -> PCE) ==============================

#' Resolve PCE bridge numerator column from markup assumption
#'
#' Under constant-percentage markup, distribution margins scale with cost,
#' so weight by purchasers_value. Under constant-dollar markup, margins are
#' fixed, so weight by producers_value (the cost base).
#'
#' @param markup_assumption Either 'constant_percentage' or 'constant_dollar'
#' @return Column name string: 'purchasers_value' or 'producers_value'
resolve_markup_numerator_col <- function(markup_assumption) {
  if (markup_assumption == 'constant_dollar') 'producers_value' else 'purchasers_value'
}


#' Aggregate GTAP-level data to BEA commodity codes via import-weighted average
#'
#' Given a tibble of per-GTAP-commodity values and baseline import totals,
#' computes import-weighted averages within each BEA commodity group defined
#' by the crosswalk. When a BEA group has zero total baseline imports, falls
#' back to an unweighted mean.
#'
#' @param gtap_data Tibble with a 'gtap_code' column (uppercase) and one or
#'   more numeric value columns. If it already contains 'baseline_imports',
#'   that column is used for weighting; otherwise weights are derived from
#'   viws_baseline via rowSums.
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns.
#' @param viws_baseline Matrix [commodity x source_region] of baseline imports.
#'   Used only when gtap_data lacks a 'baseline_imports' column.
#' @param value_cols Character vector of column names to aggregate as
#'   import-weighted averages. If NULL, all numeric columns except
#'   'baseline_imports' are used.
#' @param sum_cols Character vector of column names to aggregate as simple
#'   sums (e.g., 'baseline_imports'). Default NULL.
#' @param include_n_gtap If TRUE, include an n_gtap column counting the
#'   number of GTAP sectors per BEA code. Default FALSE.
#'
#' @return Tibble with bea_code plus one column per value_col (weighted avg),
#'   one per sum_col (sum), and optionally n_gtap.
aggregate_gtap_to_bea <- function(gtap_data, gtap_bea_crosswalk, viws_baseline,
                                  value_cols = NULL, sum_cols = NULL,
                                  include_n_gtap = FALSE) {

  crosswalk <- gtap_bea_crosswalk %>%
    mutate(gtap_code = toupper(gtap_code))

  # Add baseline_imports from viws if not already present
  if (!'baseline_imports' %in% names(gtap_data)) {
    baseline_totals <- rowSums(viws_baseline)
    names(baseline_totals) <- toupper(names(baseline_totals))
    gtap_data <- gtap_data %>%
      mutate(baseline_imports = coalesce(
        as.numeric(baseline_totals[gtap_code]), 0
      ))
  }

  # Auto-detect value columns if not specified
  if (is.null(value_cols)) {
    numeric_cols <- names(gtap_data)[sapply(gtap_data, is.numeric)]
    value_cols <- setdiff(numeric_cols, c('baseline_imports', sum_cols))
  }

  joined <- crosswalk %>%
    inner_join(gtap_data, by = 'gtap_code')

  # Weighted-average function: import-weighted when possible, else mean
  wavg <- function(col, bi) {
    if (sum(bi) > 0) sum(col * bi) / sum(bi) else mean(col)
  }

  result <- joined %>%
    group_by(bea_code) %>%
    summarise(
      across(all_of(value_cols), ~ wavg(.x, baseline_imports)),
      across(any_of(sum_cols), sum),
      .groups = 'drop'
    )

  if (include_n_gtap) {
    n_counts <- joined %>%
      group_by(bea_code) %>%
      summarise(n_gtap = n(), .groups = 'drop')
    result <- result %>%
      left_join(n_counts, by = 'bea_code')
  }

  return(result)
}


#' Map BEA commodity prices to PCE categories via the PCE bridge table
#'
#' Joins a BEA-level price/value tibble to the PCE bridge, then computes
#' a weighted average within each NIPA PCE category. The weight numerator
#' is controlled by numerator_col (from the markup assumption), and the
#' denominator is always purchasers_value.
#'
#' @param bea_price_df Tibble with 'bea_code' and one or more numeric
#'   value columns to aggregate.
#' @param pce_bridge PCE bridge tibble from load_pce_bridge()
#' @param numerator_col Column name in pce_bridge for weighting numerator:
#'   'purchasers_value' (constant-percentage) or 'producers_value'
#'   (constant-dollar).
#' @param value_cols Character vector of column names in bea_price_df to
#'   aggregate. Each produces a corresponding column in the output.
#' @param na_handling How to treat BEA codes in the bridge missing from
#'   bea_price_df: 'error' stops, 'zero' replaces NA with 0 and messages.
#' @param pp_scale If TRUE, multiply aggregated values by 100 to convert
#'   fractional to percentage points. Default FALSE.
#' @param include_pce_share If TRUE, add a pce_share column (% of total
#'   purchasers_value). Default FALSE.
#'
#' @return Tibble with nipa_line, pce_category, purchasers_value,
#'   one column per value_col, and optionally pce_share.
aggregate_bea_to_pce <- function(bea_price_df, pce_bridge, numerator_col,
                                 value_cols, na_handling = 'error',
                                 pp_scale = FALSE, include_pce_share = FALSE) {

  bridge_joined <- pce_bridge %>%
    left_join(bea_price_df, by = 'bea_code')

  # Check for NA values in any value column
  na_mask <- rowSums(is.na(bridge_joined[value_cols])) > 0
  na_codes <- unique(bridge_joined$bea_code[na_mask])

  if (length(na_codes) > 0) {
    if (na_handling == 'error') {
      stop(sprintf('%d PCE bridge BEA codes not found in price data: %s',
                   length(na_codes), paste(na_codes, collapse = ', ')))
    } else {
      message(sprintf('    %d PCE bridge BEA codes not in price data (set to 0): %s',
                      length(na_codes),
                      paste(head(na_codes, 10), collapse = ', ')))
      bridge_joined <- bridge_joined %>%
        mutate(across(all_of(value_cols), ~ if_else(is.na(.x), 0, .x)))
    }
  }

  # Weighted aggregation: sum(val * numerator) / sum(purchasers_value)
  agg_fn <- function(col, num, pv) {
    if (sum(abs(num)) > 0) sum(col * num) / sum(pv) else 0
  }

  result <- bridge_joined %>%
    group_by(nipa_line, pce_category) %>%
    summarise(
      across(all_of(value_cols),
             ~ agg_fn(.x, .data[[numerator_col]], purchasers_value)),
      purchasers_value = sum(purchasers_value),
      n_commodities = n(),
      .groups = 'drop'
    )

  if (pp_scale) {
    result <- result %>%
      mutate(across(all_of(value_cols), ~ .x * 100))
  }

  if (include_pce_share) {
    result <- result %>%
      mutate(pce_share = purchasers_value / sum(purchasers_value) * 100)
  }

  result <- result %>% arrange(nipa_line)

  return(result)
}


# ==== Core computation =======================================================

#' Build I-O matrices from BEA Use tables
#'
#' Computes the import input coefficients matrix (B_MD), domestic input
#' coefficients matrix (B_D), and import/domestic shares of commodity supply
#' (omega_M, omega_D).
#'
#' Per the Boston Fed appendix (Barbiero & Stein, April 2025):
#'   - omega_M is computed from total commodity absorption (intermediate +
#'     final demand), not just intermediate use
#'   - B matrices are normalized by industry variable cost (total intermediates +
#'     compensation of employees) under constant-percentage markup, or by total
#'     industry output under constant-dollar markup
#'
#' @param use_import CxI matrix of imported commodity use by industry
#' @param use_domestic CxI matrix of domestic commodity use by industry
#' @param industry_output Named numeric vector of total output by industry
#' @param markup_assumption 'constant_percentage' or 'constant_dollar'; controls
#'   B-matrix normalization
#' @param commodity_use_totals Tibble with bea_code, import_total,
#'   domestic_total (full commodity absorption incl final demand); NULL falls
#'   back to intermediate-only rowSums
#' @param industry_variable_cost Named numeric vector of variable cost
#'   (total intermediates + compensation of employees) per industry; required
#'   when markup_assumption = 'constant_percentage'
#'
#' @return List with:
#'   - B_MD: CxI import input coefficients matrix
#'   - B_D: CxI domestic input coefficients matrix
#'   - omega_M: named vector of import shares by commodity
#'   - omega_D: named vector of domestic shares by commodity
build_io_matrices <- function(use_import, use_domestic, industry_output,
                                     markup_assumption = 'constant_dollar',
                                     commodity_use_totals,
                                     industry_variable_cost) {

  if (ncol(use_import) != ncol(use_domestic)) {
    stop('Import and domestic use tables have different numbers of industries')
  }
  if (nrow(use_import) != nrow(use_domestic)) {
    stop('Import and domestic use tables have different numbers of commodities')
  }
  stopifnot(all(colnames(use_import) == colnames(use_domestic)))
  stopifnot(all(rownames(use_import) == rownames(use_domestic)))

  industry_codes <- colnames(use_import)
  commodity_codes <- rownames(use_import)

  # ---- Choose B-matrix normalization denominator ----
  # Boston Fed appendix: constant-dollar uses industry output,
  # constant-percentage uses total intermediates + compensation of employees
  if (markup_assumption == 'constant_percentage') {
    norm_vec <- industry_variable_cost[industry_codes]
    norm_label <- 'variable cost (intermediates + comp of employees)'
    missing <- industry_codes[is.na(norm_vec)]
    if (length(missing) > 0) {
      stop('Industry variable cost missing for: ',
           paste(missing, collapse = ', '))
    }
  } else {
    norm_vec <- industry_output[industry_codes]
    norm_label <- 'industry output'
    missing <- industry_codes[is.na(norm_vec)]
    if (length(missing) > 0) {
      stop('Industry output missing for: ',
           paste(missing, collapse = ', '))
    }
  }

  # Handle zero-normalization industries (e.g., imputed industries with zero
  # variable cost but nonzero output). These produce Inf/NaN in B matrices.
  # Zero norm -> zero B (no cost propagation through that industry).
  zero_norm <- norm_vec == 0
  if (any(zero_norm)) {
    message(sprintf('    Warning: %d industries with zero normalization denominator, B set to 0: %s',
                    sum(zero_norm),
                    paste(industry_codes[zero_norm], collapse = ', ')))
    norm_vec[zero_norm] <- 1  # placeholder to avoid division by zero
  }

  # ---- Compute input coefficients matrices ----
  norm_matrix <- matrix(
    rep(norm_vec, each = nrow(use_import)),
    nrow = nrow(use_import),
    ncol = ncol(use_import)
  )

  B_MD <- use_import / norm_matrix
  B_D <- use_domestic / norm_matrix

  # Zero out columns for industries with zero normalization denominator
  if (any(zero_norm)) {
    B_MD[, zero_norm] <- 0
    B_D[, zero_norm] <- 0
  }

  rownames(B_MD) <- commodity_codes
  colnames(B_MD) <- industry_codes
  rownames(B_D) <- commodity_codes
  colnames(B_D) <- industry_codes

  # ---- Compute import/domestic shares by commodity ----
  # Boston Fed appendix: omega_M from total commodity absorption (intermediate +
  # final demand), not just intermediate use
  totals_lookup <- commodity_use_totals %>%
    mutate(total = import_total + domestic_total)
  import_lookup <- setNames(totals_lookup$import_total, totals_lookup$bea_code)
  total_lookup <- setNames(totals_lookup$total, totals_lookup$bea_code)

  import_totals <- import_lookup[commodity_codes]
  total_use <- total_lookup[commodity_codes]

  # Commodities in use table but not in totals file: fall back to intermediate
  missing_codes <- commodity_codes[is.na(total_use)]
  if (length(missing_codes) > 0) {
    message(sprintf('    %d commodities not in use totals file, using intermediate-only: %s',
                    length(missing_codes),
                    paste(head(missing_codes, 5), collapse = ', ')))
    import_totals[is.na(import_totals)] <- rowSums(use_import)[is.na(import_totals)]
    total_use[is.na(total_use)] <- (rowSums(use_import) + rowSums(use_domestic))[is.na(total_use)]
  }
  names(import_totals) <- commodity_codes
  names(total_use) <- commodity_codes
  omega_source <- 'full absorption (incl final demand)'

  omega_M <- import_totals / total_use
  omega_D <- 1 - omega_M

  # Handle commodities with zero total use
  zero_use <- total_use == 0
  if (any(zero_use)) {
    message(sprintf('    Warning: %d commodities with zero total use, setting omega to 0',
                    sum(zero_use)))
    omega_M[zero_use] <- 0
    omega_D[zero_use] <- 0
  }

  # Clamp any slightly negative omega_M from rounding (e.g., -0.008 in detail)
  neg_omega <- omega_M < 0 & !zero_use
  if (any(neg_omega)) {
    message(sprintf('    Warning: %d commodities with slightly negative omega_M (clamped to 0)',
                    sum(neg_omega)))
    omega_M[neg_omega] <- 0
    omega_D[neg_omega] <- 1
  }

  # Zero out omega for BEA special adjustment commodities
  # These are statistical adjustments, not real produced goods
  BEA_ADJUSTMENT_CODES <- c(
    'S00300',  # Noncomparable imports
    'S00401',  # Scrap
    'S00402',  # Used and secondhand goods
    'S00900',  # Rest-of-world adjustment
    'Other',   # Summary: noncomparable imports + rest-of-world
    'Used'     # Summary: scrap, used and secondhand goods
  )
  adj_mask <- commodity_codes %in% BEA_ADJUSTMENT_CODES
  if (any(adj_mask)) {
    message(sprintf('    Zeroing omega_M for %d BEA adjustment commodities: %s',
                    sum(adj_mask),
                    paste(commodity_codes[adj_mask], collapse = ', ')))
    omega_M[adj_mask] <- 0
    omega_D[adj_mask] <- 1
  }

  message(sprintf('  Built I-O matrices:'))
  message(sprintf('    B normalization: %s', norm_label))
  message(sprintf('    B_MD: %d x %d (CxI)', nrow(B_MD), ncol(B_MD)))
  message(sprintf('    B_D:  %d x %d (CxI)', nrow(B_D), ncol(B_D)))
  message(sprintf('    omega_M source: %s', omega_source))
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


#' Compute I-O price effects from tariff shocks
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
#' @param domestic_pricing Scalar domestic competitive pricing effect (e.g., 0.5).
#'   Fraction of tariff-induced cost increases passed through to prices of
#'   domestically produced competing goods. Combined with usd_offset as
#'   (1 + domestic_pricing - usd_offset).
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
compute_io_prices <- function(tau_M, B_MD, leontief_domestic,
                                     omega_M, omega_D, pce_bridge,
                                     usd_offset, domestic_pricing,
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

  missing_from_leontief <- setdiff(bmd_industries, leontief_industries)
  missing_from_bmd <- setdiff(leontief_industries, bmd_industries)
  if (length(missing_from_leontief) > 0 || length(missing_from_bmd) > 0) {
    msg <- 'B_MD and Leontief domestic industry dimensions do not match'
    if (length(missing_from_leontief) > 0) {
      msg <- paste0(msg, sprintf('\n  In B_MD but not L_D (%d): %s',
                                 length(missing_from_leontief),
                                 paste(head(missing_from_leontief, 10), collapse = ', ')))
    }
    if (length(missing_from_bmd) > 0) {
      msg <- paste0(msg, sprintf('\n  In L_D but not B_MD (%d): %s',
                                 length(missing_from_bmd),
                                 paste(head(missing_from_bmd, 10), collapse = ', ')))
    }
    stop(msg)
  }

  cost_aligned <- imported_input_cost[leontief_industries]

  propagated <- as.numeric(t(leontief_domestic) %*% cost_aligned)
  names(propagated) <- colnames(leontief_domestic)

  # Verify propagated commodity dimension matches expected commodities
  missing_from_prop <- setdiff(commodities, names(propagated))
  extra_in_prop <- setdiff(names(propagated), commodities)
  if (length(missing_from_prop) > 0 || length(extra_in_prop) > 0) {
    msg <- 'Propagated price commodity dimension does not match expected commodities'
    if (length(missing_from_prop) > 0) {
      msg <- paste0(msg, sprintf('\n  Expected but missing (%d): %s',
                                 length(missing_from_prop),
                                 paste(head(missing_from_prop, 10), collapse = ', ')))
    }
    if (length(extra_in_prop) > 0) {
      msg <- paste0(msg, sprintf('\n  Extra in propagated (%d): %s',
                                 length(extra_in_prop),
                                 paste(head(extra_in_prop, 10), collapse = ', ')))
    }
    stop(msg)
  }
  propagated_aligned <- propagated[commodities]

  supply_chain <- omega_D_aligned * propagated_aligned

  # ---- Apply domestic pricing and USD offset ----
  scaling <- 1 + domestic_pricing - usd_offset
  total <- (direct + supply_chain) * scaling
  direct_scaled <- direct * scaling
  supply_chain_scaled <- supply_chain * scaling

  # ---- Markup assumption: choose numerator weight ----
  numerator_col <- resolve_markup_numerator_col(markup_assumption)

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

  # Direct/supply chain decomposition (only available at commodity level)
  direct_aggregate <- sum(direct_scaled * num_aligned) / total_pce
  supply_chain_aggregate <- sum(supply_chain_scaled * num_aligned) / total_pce

  # ---- Map to PCE categories via bridge (the C matrix) ----
  price_df <- tibble(
    bea_code = commodities,
    price = as.numeric(total)
  )

  pce_category_prices <- aggregate_bea_to_pce(
    price_df, pce_bridge, numerator_col,
    value_cols = 'price', na_handling = 'error',
    pp_scale = TRUE, include_pce_share = TRUE
  ) %>%
    rename(sr_price_effect = price)

  # ---- Derive aggregate from category table (single source of truth) ----
  aggregate <- sum(pce_category_prices$sr_price_effect / 100 *
                   pce_category_prices$purchasers_value) /
               sum(pce_category_prices$purchasers_value)

  # ---- Diagnostics ----
  message(sprintf('  I-O price effects (%s markups):', markup_assumption))
  message(sprintf('    Commodities with nonzero tariff: %d of %d',
                  sum(tau_aligned > 0), length(tau_aligned)))
  message(sprintf('    Aggregate price effect: %.4f%%', aggregate * 100))
  message(sprintf('    Direct component:       %.4f%%', direct_aggregate * 100))
  message(sprintf('    Supply chain component: %.4f%%', supply_chain_aggregate * 100))
  message(sprintf('    Scaling (1 + dp - fx):  %.3f  (domestic_pricing=%.3f, usd_offset=%.3f)',
                  scaling, domestic_pricing, usd_offset))
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

#' Compute post-substitution tau_M using GTAP source-composition shifts
#'
#' For each GTAP sector, recomputes the import-weighted average tariff rate
#' using post-simulation import weights (from viws) but the SAME tariff rates.
#' The ratio of post-sim to baseline weighted-average tariff isolates the pure
#' source-composition effect: if imports shift from high-tariff China to
#' low-tariff Vietnam, the effective tariff falls — even though no tariff rate
#' changed.
#'
#' This deliberately excludes the overall import volume decline (margin 2:
#' import-to-domestic substitution), which is already captured by the omega_M
#' adjustment in compute_postsub_omega_M(). Using total import volume ratios
#' here would double-count that margin.
#'
#' @param tau_M_presub Named vector of pre-sub tariff shocks by BEA commodity
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns
#' @param etr_matrix Tibble with gtap_code column and country columns of tariff
#'   rates (fractional, e.g. 0.10 = 10%). Country columns must align with
#'   viws column names (minus 'usa').
#' @param viws_postsim Matrix [commodity x source_region] of post-sim imports
#' @param viws_baseline Matrix [commodity x source_region] of baseline imports
#'
#' @return Named vector of post-sub tariff shocks by BEA commodity
compute_postsub_tau_M <- function(tau_M_presub, gtap_bea_crosswalk,
                                  etr_matrix, viws_postsim, viws_baseline) {

  gtap_codes <- toupper(rownames(viws_baseline))

  # Country columns shared between etr_matrix and viws
  etr_countries <- setdiff(names(etr_matrix), c('gtap_code', 'date'))
  viws_countries <- colnames(viws_baseline)
  common_countries <- intersect(etr_countries, viws_countries)

  if (length(common_countries) == 0) {
    stop('No common country columns between etr_matrix and viws matrices')
  }

  # Build tariff rate matrix aligned to viws rows (GTAP sectors x countries)
  etr_lookup <- etr_matrix %>%
    mutate(gtap_code = toupper(gtap_code))
  etr_mat <- matrix(0, nrow = length(gtap_codes), ncol = length(common_countries),
                    dimnames = list(gtap_codes, common_countries))
  matched_sectors <- intersect(gtap_codes, etr_lookup$gtap_code)
  for (sector in matched_sectors) {
    row_idx <- which(etr_lookup$gtap_code == sector)
    for (cty in common_countries) {
      etr_mat[sector, cty] <- etr_lookup[[cty]][row_idx]
    }
  }

  # Extract viws submatrices for common countries only
  viws_bl <- viws_baseline[, common_countries, drop = FALSE]
  viws_ps <- viws_postsim[, common_countries, drop = FALSE]

  # Per-GTAP-sector: import-weighted average tariff, baseline vs post-sim weights
  # avg_tariff = sum(tariff_r * imports_r) / sum(imports_r)
  weighted_tariff_bl <- rowSums(etr_mat * viws_bl)
  weighted_tariff_ps <- rowSums(etr_mat * viws_ps)
  total_imports_bl <- rowSums(viws_bl)
  total_imports_ps <- rowSums(viws_ps)

  avg_tariff_bl <- weighted_tariff_bl / total_imports_bl
  avg_tariff_ps <- weighted_tariff_ps / total_imports_ps

  # Ratio: how did the source-weighted average tariff change?
  # >1 means imports shifted toward higher-tariff sources (rare)
  # <1 means imports shifted toward lower-tariff sources (trade diversion)
  tariff_ratio <- avg_tariff_ps / avg_tariff_bl

  # Handle edge cases: zero baseline tariff, zero imports, NaN/Inf
  # If baseline avg tariff is 0, no tariff to shift composition of -> ratio = 1
  # If post-sim imports are 0, sector has no imports left -> ratio doesn't matter
  #   (omega_M adjustment will handle this)
  tariff_ratio[is.nan(tariff_ratio) | is.infinite(tariff_ratio)] <- 1.0

  gtap_ratios <- tibble(
    gtap_code = gtap_codes,
    tariff_ratio = as.numeric(tariff_ratio),
    baseline_imports = as.numeric(total_imports_bl)
  )

  # Map to BEA: weighted average of GTAP ratios within each BEA group
  bea_ratios <- aggregate_gtap_to_bea(
    gtap_ratios, gtap_bea_crosswalk, viws_baseline,
    value_cols = 'tariff_ratio'
  )

  ratio_lookup <- setNames(bea_ratios$tariff_ratio, bea_ratios$bea_code)

  # Apply ratios to tau_M
  tau_M_postsub <- tau_M_presub
  matched <- intersect(names(tau_M_postsub), names(ratio_lookup))
  tau_M_postsub[matched] <- tau_M_presub[matched] * ratio_lookup[matched]

  unmatched_bea <- setdiff(names(tau_M_presub), names(ratio_lookup))
  nonzero_unmatched <- unmatched_bea[tau_M_presub[unmatched_bea] > 0]
  message(sprintf('  Post-sub tau_M (source-composition): %d BEA commodities adjusted, mean ratio = %.4f',
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

  # Uppercase NVPP names to match crosswalk
  names(nvpp_commodity_ratio) <- toupper(names(nvpp_commodity_ratio))

  # Build GTAP-level data, filtering out sectors with no NVPP ratio
  baseline_totals <- rowSums(viws_baseline)
  names(baseline_totals) <- toupper(names(baseline_totals))
  gtap_codes <- unique(toupper(gtap_bea_crosswalk$gtap_code))
  gtap_data <- tibble(
    gtap_code = gtap_codes,
    ratio = as.numeric(nvpp_commodity_ratio[gtap_codes]),
    baseline_imports = coalesce(as.numeric(baseline_totals[gtap_codes]), 0)
  ) %>%
    filter(!is.na(ratio))

  # Map to BEA: weighted average of GTAP ratios within each BEA group
  bea_ratios <- aggregate_gtap_to_bea(
    gtap_data, gtap_bea_crosswalk, viws_baseline,
    value_cols = 'ratio'
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


# ==== GE price model (GTAP ppa) ==============================================

#' Compute GE price effects from GTAP private consumption prices
#'
#' Uses GTAP's ppa (private consumption price % change by commodity) as the
#' full general-equilibrium price response, mapped to BEA commodities via the
#' GTAP-BEA crosswalk and then to 76 NIPA PCE categories via the PCE bridge.
#'
#' Unlike the PE Boston Fed model, this captures all GE channels:
#'   - Domestic competitive repricing (pricing umbrella)
#'   - Foreign exporter absorption (incomplete passthrough)
#'   - Input substitution in production (CES nesting)
#'   - Cross-sector feedback and terms-of-trade effects
#'
#' No USD offset is applied -- GTAP's equilibrium prices already internalize
#' exchange rate responses under its closure.
#'
#' @param ppa_usa Named vector of GTAP commodity price % changes for USA
#'   (65 commodities, names = GTAP commodity codes)
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns
#' @param viws_baseline Matrix [commodity x source_region] of baseline imports
#'   (for import-weighting when multiple GTAP sectors map to one BEA code)
#' @param pce_bridge PCE bridge tibble from load_pce_bridge()
#' @param markup_assumption Either 'constant_percentage' or 'constant_dollar'
#'
#' @return List with:
#'   - pce_category_prices: tibble (nipa_line, pce_category, sr_price_effect [pp],
#'     purchasers_value, pce_share)
#'   - bea_commodity_prices: named vector of per-commodity price effects (fractional)
#'   - aggregate: PCE-weighted aggregate price effect (fractional)
#'   - markup_assumption: which assumption was used
compute_ge_prices <- function(ppa_usa, gtap_bea_crosswalk, viws_baseline,
                              pce_bridge, markup_assumption = 'constant_percentage') {

  valid_markups <- c('constant_percentage', 'constant_dollar')
  if (!markup_assumption %in% valid_markups) {
    stop('Invalid markup_assumption: ', markup_assumption,
         '. Must be one of: ', paste(valid_markups, collapse = ', '))
  }

  # ---- Map GTAP ppa to BEA codes via crosswalk (import-weighted) ----

  gtap_codes <- toupper(names(ppa_usa))
  gtap_ppa <- tibble(
    gtap_code = gtap_codes,
    ppa = as.numeric(ppa_usa)
  )

  bea_ppa <- aggregate_gtap_to_bea(
    gtap_ppa, gtap_bea_crosswalk, viws_baseline,
    value_cols = 'ppa', include_n_gtap = TRUE
  )

  # Convert % change to fractional price effect
  bea_prices <- setNames(bea_ppa$ppa / 100, bea_ppa$bea_code)

  message(sprintf('  GE prices: %d GTAP -> %d BEA commodities, mean ppa = %.4f%%',
                  length(gtap_codes), nrow(bea_ppa), mean(bea_ppa$ppa)))

  # ---- Apply PCE bridge (C matrix) ----

  numerator_col <- resolve_markup_numerator_col(markup_assumption)

  price_df <- tibble(
    bea_code = names(bea_prices),
    price = as.numeric(bea_prices)
  )

  pce_category_prices <- aggregate_bea_to_pce(
    price_df, pce_bridge, numerator_col,
    value_cols = 'price', na_handling = 'zero',
    pp_scale = TRUE, include_pce_share = TRUE
  ) %>%
    rename(sr_price_effect = price)

  # ---- Aggregate from category table (single source of truth) ----
  aggregate <- sum(pce_category_prices$sr_price_effect / 100 *
                   pce_category_prices$purchasers_value) /
               sum(pce_category_prices$purchasers_value)

  # ---- Diagnostics ----
  message(sprintf('  GE price effects (%s markups, no USD offset):', markup_assumption))
  message(sprintf('    Aggregate price effect: %.4f%%', aggregate * 100))
  message(sprintf('    PCE categories:         %d', nrow(pce_category_prices)))
  message(sprintf('    Nonzero categories:     %d',
                  sum(pce_category_prices$sr_price_effect != 0)))

  return(list(
    pce_category_prices = pce_category_prices,
    bea_commodity_prices = bea_prices,
    aggregate = aggregate,
    markup_assumption = markup_assumption
  ))
}


#' Decompose GTAP GE price effects into import, domestic, and share-shift terms
#'
#' Produces a diagnostic decomposition of GTAP household consumption prices.
#' The decomposition is approximate at the GTAP commodity level:
#'   ppa ~= s0 * ppm + (1 - s0) * ppd + (s1 - s0) * (ppm - ppd) + residual
#' where s0 is the baseline household import share and s1 is the post-simulation
#' import share inferred from GTAP NVPP quantities when available.
#'
#' The same GTAP -> BEA -> PCE mapping used for GE prices is then applied to each
#' term, so the category-level contributions sum to the reported GE price effect.
#'
#' @param ppa_usa Named vector of GTAP private consumption price changes (%)
#' @param ppm_usa Named vector of GTAP imported household price changes (%)
#' @param ppd_usa Named vector of GTAP domestic household price changes (%)
#' @param gtap_bea_crosswalk Tibble with gtap_code, bea_code columns
#' @param viws_baseline Matrix [commodity x source_region] of baseline imports
#' @param pce_bridge PCE bridge tibble from load_pce_bridge()
#' @param nvpp_adjustment List from extract_nvpp_adjustment()
#' @param markup_assumption Either 'constant_percentage' or 'constant_dollar'
#' @param presub_pce_category_prices Optional presub category table for gap calc
#'
#' @return List with gtap_commodity, bea_commodity, pce_category, and summary tibbles
decompose_ge_prices <- function(ppa_usa, ppm_usa, ppd_usa,
                                gtap_bea_crosswalk, viws_baseline, pce_bridge,
                                nvpp_adjustment,
                                markup_assumption = 'constant_percentage',
                                presub_pce_category_prices = NULL) {

  valid_markups <- c('constant_percentage', 'constant_dollar')
  if (!markup_assumption %in% valid_markups) {
    stop('Invalid markup_assumption: ', markup_assumption,
         '. Must be one of: ', paste(valid_markups, collapse = ', '))
  }
  if (is.null(nvpp_adjustment$baseline_import_share) ||
      is.null(nvpp_adjustment$postsim_import_share)) {
    stop('nvpp_adjustment must contain baseline_import_share and postsim_import_share')
  }

  gtap_codes <- Reduce(intersect, list(
    toupper(names(ppa_usa)),
    toupper(names(ppm_usa)),
    toupper(names(ppd_usa)),
    toupper(names(nvpp_adjustment$baseline_import_share)),
    toupper(names(nvpp_adjustment$postsim_import_share))
  ))

  baseline_share <- nvpp_adjustment$baseline_import_share
  names(baseline_share) <- toupper(names(baseline_share))
  postsim_share <- nvpp_adjustment$postsim_import_share
  names(postsim_share) <- toupper(names(postsim_share))

  ppa_lookup <- ppa_usa
  names(ppa_lookup) <- toupper(names(ppa_lookup))
  ppm_lookup <- ppm_usa
  names(ppm_lookup) <- toupper(names(ppm_lookup))
  ppd_lookup <- ppd_usa
  names(ppd_lookup) <- toupper(names(ppd_lookup))

  gtap_commodity <- tibble(
    gtap_code = gtap_codes,
    ppa = as.numeric(ppa_lookup[gtap_codes]),
    ppm = as.numeric(ppm_lookup[gtap_codes]),
    ppd = as.numeric(ppd_lookup[gtap_codes]),
    baseline_import_share = as.numeric(baseline_share[gtap_codes]),
    postsim_import_share = as.numeric(postsim_share[gtap_codes])
  ) %>%
    mutate(
      import_price_component = baseline_import_share * ppm,
      domestic_price_component = (1 - baseline_import_share) * ppd,
      share_shift_component = (postsim_import_share - baseline_import_share) * (ppm - ppd),
      reconstructed_price = import_price_component + domestic_price_component + share_shift_component,
      residual_component = ppa - reconstructed_price
    )

  bea_commodity <- aggregate_gtap_to_bea(
    gtap_commodity, gtap_bea_crosswalk, viws_baseline,
    value_cols = c('ppa', 'ppm', 'ppd',
                   'import_price_component', 'domestic_price_component',
                   'share_shift_component', 'residual_component',
                   'baseline_import_share', 'postsim_import_share'),
    sum_cols = 'baseline_imports',
    include_n_gtap = TRUE
  )

  numerator_col <- resolve_markup_numerator_col(markup_assumption)

  decomp_cols <- c('import_price_component', 'domestic_price_component',
                   'share_shift_component', 'residual_component', 'ppa')
  bea_price_df <- bea_commodity %>%
    select(bea_code, all_of(decomp_cols))

  pce_category <- aggregate_bea_to_pce(
    bea_price_df, pce_bridge, numerator_col,
    value_cols = decomp_cols, na_handling = 'zero',
    include_pce_share = TRUE
  ) %>%
    rename(ge = ppa)

  if (!is.null(presub_pce_category_prices)) {
    pce_category <- pce_category %>%
      left_join(
        presub_pce_category_prices %>%
          select(nipa_line, pre_sub = sr_price_effect),
        by = 'nipa_line'
      ) %>%
      mutate(ge_minus_pre_sub = ge - pre_sub)
  }

  summary <- tibble(
        metric = c(
          'import_price_component',
          'domestic_price_component',
          'share_shift_component',
          'residual_component',
          'ge_price_increase'
        ),
        value = c(
      sum(pce_category$import_price_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$domestic_price_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$share_shift_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$residual_component * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value),
      sum(pce_category$ge * pce_category$purchasers_value) /
        sum(pce_category$purchasers_value)
    ),
    unit = 'pct'
  )

  return(list(
    gtap_commodity = gtap_commodity,
    bea_commodity = bea_commodity,
    pce_category = pce_category,
    summary = summary,
    markup_assumption = markup_assumption
  ))
}
