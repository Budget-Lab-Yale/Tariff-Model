# =============================================================================
# 00a_prepare_rate_inputs.R
#
# Pre-GTAP step: turn the upstream tariff-rate-tracker rate panel into the
# aggregated artifacts the downstream model expects. This absorbs the GTAP-sector
# rollup, BEA-commodity rollup, scenario-minus-baseline deltas, and shocks.txt
# generation that used to live in tariff-etrs.
#
# It MUST run before GTAP (Step 0b), because run_gtap() consumes shocks.txt.
# Pipeline order: read params -> 00a prepare rate inputs -> 0b GTAP -> 1 load inputs.
#
# Outputs (written to output/<scenario>/rate_inputs/, read by 01_load_inputs.R):
#   gtap_deltas_by_sector_country.csv   GTAP-sector x partner, deltas (pp)
#   gtap_levels_by_sector_country.csv   GTAP-sector x partner, levels (pp)
#   bea_deltas_by_commodity.csv         BEA commodity, delta (fraction)
#   shocks.txt                          GTAP shock commands at gtap_reference_date
# Plus output/<scenario>/baseline/gtap_levels_by_sector_country.csv (static).
#
# Aggregation logic ported from tariff-etrs:
#   calc_delta (etr_engine.R:104), aggregate_countries_to_partners (etr_engine.R:823),
#   write_bea_commodity_deltas (outputs.R:1185), write_shock_commands (outputs.R:146).
# =============================================================================

library(tidyverse)

source('src/read_rate_panel.R')

# ==== Output ordering constants (must match downstream expectations) =========
# Ported verbatim from tariff-etrs/src/outputs.R:25-35.
PARTNER_ORDER_CSV    <- c('china', 'canada', 'mexico', 'uk', 'japan', 'eu', 'row', 'ftrow')
PARTNER_ORDER_SHOCKS <- c('China', 'ROW', 'FTROW', 'Canada', 'Mexico', 'Japan', 'EU', 'UK')
SECTOR_ORDER <- c(
  'pdr', 'wht', 'gro', 'v_f', 'osd', 'c_b', 'pfb', 'ocr', 'ctl', 'oap',
  'rmk', 'wol', 'frs', 'fsh', 'coa', 'oil', 'gas', 'oxt', 'cmt', 'omt',
  'vol', 'mil', 'pcr', 'sgr', 'ofd', 'b_t', 'tex', 'wap', 'lea', 'lum',
  'ppp', 'p_c', 'chm', 'bph', 'rpp', 'nmm', 'i_s', 'nfm', 'fmp', 'ele',
  'eeq', 'ome', 'mvh', 'otn', 'omf', 'ely', 'gdt', 'wtr', 'cns'
)
PARTNER_SHOCK_LABEL <- c(
  'china' = 'China', 'canada' = 'Canada', 'mexico' = 'Mexico', 'row' = 'ROW',
  'ftrow' = 'FTROW', 'japan' = 'Japan', 'eu' = 'EU', 'uk' = 'UK'
)

RATE_AGG_RESOURCE_DIR <- 'resources/rate_aggregation/2024'

# =============================================================================
# Resource loading
# =============================================================================

#' Load the HS10xcountry import weights (with GTAP sector) and partner mapping
#'
#' @return List with $weights (hts10, cty_code, gtap_code, imports) and
#'   $partner_mapping (cty_code, partner) and $hs10_bea (hts10, bea_code)
load_aggregation_resources <- function() {

  # ---- import weights (hts10 x cty_code x gtap_code, 2024 Census flows) ----
  # Resource files use `hs10`; the canonical panel uses `hts10` — rename on load.
  weights_path <- file.path(RATE_AGG_RESOURCE_DIR, 'import_weights_hs10_country_2024.rds')
  if (!file.exists(weights_path)) {
    stop('Import weights not found: ', weights_path)
  }
  weights <- as_tibble(readRDS(weights_path)) %>%
    rename(hts10 = hs10) %>%
    mutate(hts10 = as.character(hts10), cty_code = as.character(cty_code)) %>%
    select(hts10, cty_code, gtap_code, imports)

  # Fail loud on degraded weights — these would silently corrupt every rollup.
  if (anyNA(weights$hts10) || anyNA(weights$cty_code)) {
    stop('Import weights have NA keys (hts10 / cty_code)')
  }
  if (anyNA(weights$gtap_code)) {
    stop(sum(is.na(weights$gtap_code)), ' import-weight rows have NA gtap_code')
  }
  if (anyNA(weights$imports)) {
    stop('Import weights have NA imports')
  }
  # Zero-weight pairs add nothing to an import-weighted average. Drop them EXPLICITLY
  # (not via a divide-by-zero guard) so every rollup denominator is strictly positive.
  n_zero <- sum(weights$imports == 0)
  if (n_zero > 0) {
    message(sprintf('  Dropping %d zero-weight (imports == 0) pairs from rollup universe', n_zero))
    weights <- weights %>% filter(imports > 0)
  }

  # ---- country -> partner mapping (a NAMED-partner list; ROW is the complement) ----
  # The mapping deliberately lists only named partners (china/canada/mexico/uk/japan/
  # eu/ftrow). Most countries are absent BY DESIGN and roll up to 'row'. An unmapped
  # country is therefore expected, not a bug — see build_pair_frame.
  partner_path <- file.path(RATE_AGG_RESOURCE_DIR, 'country_partner_mapping.csv')
  partner_mapping <- read_csv(partner_path, show_col_types = FALSE,
                              col_types = cols(cty_code = col_character())) %>%
    select(cty_code, partner) %>%
    distinct()

  # ---- HS10 -> BEA commodity crosswalk ----
  bea_path <- file.path(RATE_AGG_RESOURCE_DIR, 'hs10_bea_crosswalk.csv')
  hs10_bea <- read_csv(bea_path, show_col_types = FALSE,
                       col_types = cols(hs10 = col_character(), bea_code = col_character())) %>%
    rename(hts10 = hs10) %>%
    # crosswalk rows with an empty bea_code are explicit non-mappings; treat as absent
    filter(!is.na(bea_code), bea_code != '')

  return(list(weights = weights, partner_mapping = partner_mapping, hs10_bea = hs10_bea))
}

#' Stop if any import-weighted pair is missing a value in `col` (silent-drop guard)
#'
#' The rollup is driven off the weights universe; every weighted (hts10, cty_code)
#' MUST resolve to a rate in the panel. A missing value means a key mismatch or a
#' panel gap that would silently understate the result — fail loud with the size.
assert_full_coverage <- function(frame, col, label) {
  miss <- frame %>% filter(is.na(.data[[col]]))
  if (nrow(miss) > 0) {
    sample <- miss %>% slice_head(n = 5) %>% transmute(pair = paste(hts10, cty_code)) %>% pull(pair)
    stop(sprintf(paste0('%d of %d weighted (hts10, cty_code) pairs have no %s rate ',
                        '($%.2fB import value unmatched) — would silently understate the rollup. ',
                        'Check hts10/cty_code formats and panel coverage. Sample: %s'),
                 nrow(miss), nrow(frame), label, sum(miss$imports) / 1e9,
                 paste(sample, collapse = ', ')))
  }
  invisible(NULL)
}

# =============================================================================
# Core aggregation (ported from tariff-etrs)
# =============================================================================

#' Build the per-pair frame at one date: level, baseline level, delta, weights
#'
#' @param scenario_slice Static panel at the date (hts10, cty_code, rate_total)
#' @param baseline_slice Static baseline panel (hts10, cty_code, rate_total)
#' @param resources Output of load_aggregation_resources()
#'
#' @return Tibble: hts10, cty_code, gtap_code, partner, imports, level, delta
build_pair_frame <- function(scenario_slice, baseline_slice, resources) {

  scenario_levels <- scenario_slice %>% select(hts10, cty_code, level = rate_total)
  baseline_levels <- baseline_slice %>% select(hts10, cty_code, base_level = rate_total)

  # Drive the rollup off the import-weight universe (traded hts10 x cty_code pairs),
  # then REQUIRE every weighted pair to resolve to a rate in BOTH panels. A key
  # mismatch (hts10 format, vintage skew, missing country) surfaces as NA here and
  # stops loudly — never an inner_join that would silently drop traded value.
  frame <- resources$weights %>%
    left_join(scenario_levels, by = c('hts10', 'cty_code')) %>%
    left_join(baseline_levels, by = c('hts10', 'cty_code'))

  assert_full_coverage(frame, 'level', 'scenario')
  assert_full_coverage(frame, 'base_level', 'baseline')

  frame %>%
    mutate(delta = level - base_level) %>%
    left_join(resources$partner_mapping, by = 'cty_code') %>%
    # Unmapped countries are the ROW complement BY DESIGN (named-partner mapping);
    # this is the one deliberate default, not a silent drop — see load_aggregation_resources.
    mutate(partner = coalesce(partner, 'row')) %>%
    select(hts10, cty_code, gtap_code, partner, imports, level, delta)
}

#' Import-weighted rollup of a pair frame to partner x GTAP sector
#'
#' @param pair_frame Output of build_pair_frame()
#'
#' @return Tibble: partner, gtap_code, etr (weighted delta), level (weighted level)
rollup_gtap <- function(pair_frame) {
  # gtap_code is non-NA (asserted at load) and imports are strictly positive
  # (zero-weight pairs dropped at load), so no NA filter and no divide guard.
  pair_frame %>%
    group_by(partner, gtap_code) %>%
    summarise(
      total_imports = sum(imports),
      w_delta = sum(delta * imports),
      w_level = sum(level * imports),
      .groups = 'drop'
    ) %>%
    mutate(etr = w_delta / total_imports,
           level = w_level / total_imports) %>%
    select(partner, gtap_code, etr, level)
}

#' Import-weighted rollup of a pair frame to BEA commodity (deltas only)
#'
#' @param pair_frame Output of build_pair_frame()
#' @param hs10_bea HS10 -> BEA crosswalk (hts10, bea_code)
#'
#' @return Tibble: bea_code, etr_delta (fraction), total_imports
rollup_bea <- function(pair_frame, hs10_bea) {

  joined <- pair_frame %>% left_join(hs10_bea, by = 'hts10')

  # ~3% of import value has no BEA commodity. Dropping a pair with NO tariff change is
  # harmless; dropping one WITH a change silently understates tau_M — so fail loud
  # only in that case, and log the benign (zero-delta) drops.
  unmapped <- joined %>% filter(is.na(bea_code))
  unmapped_changed <- unmapped %>% filter(delta != 0)
  if (nrow(unmapped_changed) > 0) {
    stop(sprintf(paste0('%d (hts10, cty_code) pairs carry a tariff change but have no BEA ',
                        'commodity ($%.2fB) — these would be lost from the I-O price model. ',
                        'Extend hs10_bea_crosswalk.csv. Sample hts10: %s'),
                 nrow(unmapped_changed), sum(unmapped_changed$imports) / 1e9,
                 paste(head(unique(unmapped_changed$hts10), 5), collapse = ', ')))
  }
  if (nrow(unmapped) > 0) {
    message(sprintf('  BEA rollup: %d unmapped pairs with no rate change dropped ($%.2fB)',
                    nrow(unmapped), sum(unmapped$imports) / 1e9))
  }

  joined %>%
    filter(!is.na(bea_code)) %>%
    group_by(bea_code) %>%
    summarise(
      etr_delta = sum(delta * imports) / sum(imports),
      total_imports = sum(imports),
      .groups = 'drop'
    ) %>%
    arrange(bea_code)
}

#' Pivot a partner x sector long frame to the wide CSV matrix (sectors x partners, pp)
#'
#' @param long_df Tibble with partner, gtap_code, and `value_col`
#' @param value_col Name of the value column ('etr' or 'level')
#'
#' @return Wide tibble: gtap_code + one column per partner, in pp, ordered
to_sector_country_wide <- function(long_df, value_col) {
  wide <- long_df %>%
    select(partner, gtap_code, value = all_of(value_col)) %>%
    pivot_wider(names_from = partner, values_from = value, values_fill = 0) %>%
    select(gtap_code, any_of(PARTNER_ORDER_CSV)) %>%
    mutate(across(-gtap_code, ~ .x * 100))

  existing <- intersect(SECTOR_ORDER, wide$gtap_code)
  wide %>%
    filter(gtap_code %in% existing) %>%
    arrange(match(gtap_code, SECTOR_ORDER))
}

#' Write GTAP shock commands for the reference date (ported from write_shock_commands)
#'
#' @param partner_gtap Long frame for the reference date (partner, gtap_code, etr)
#' @param output_path File to write
write_shocks <- function(partner_gtap, output_path) {

  shock_commands <- partner_gtap %>%
    mutate(
      partner_fmt = PARTNER_SHOCK_LABEL[partner],
      etr_pct = round(etr * 100, 1)
    ) %>%
    filter(etr_pct != 0, !is.na(gtap_code)) %>%
    arrange(match(partner_fmt, PARTNER_ORDER_SHOCKS), gtap_code) %>%
    mutate(command = sprintf('Shock tms("%s","%s","USA") = %.1f;', gtap_code, partner_fmt, etr_pct))

  con <- file(output_path, 'w')
  for (p in PARTNER_ORDER_SHOCKS) {
    cmds <- shock_commands %>% filter(partner_fmt == p)
    if (nrow(cmds) > 0) {
      writeLines(cmds$command, con)
      writeLines('', con)
    }
  }
  close(con)

  message(sprintf('  Wrote %d shock commands to %s', nrow(shock_commands), output_path))
}

# =============================================================================
# Orchestrator
# =============================================================================

#' Prepare all rate-derived inputs for a scenario (the Step 0a entry point)
#'
#' @param scenario Scenario name (config/scenarios/<scenario>/)
#'
#' @return Invisibly, the list of written file paths
prepare_rate_inputs <- function(scenario) {

  scenario_dir <- file.path('config', 'scenarios', scenario)
  model_params <- yaml::read_yaml(file.path(scenario_dir, 'model_params.yaml'))

  rate_panel <- model_params$rate_panel
  if (is.null(rate_panel)) {
    stop('model_params.yaml must have a rate_panel block (root, vintage, tracker_scenario, baseline_date)')
  }
  gtap_reference_date <- as.Date(model_params$gtap_reference_date)

  out_dir <- file.path('output', scenario, 'rate_inputs')
  baseline_dir <- file.path('output', scenario, 'baseline')
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(baseline_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- inputs ----
  resources <- load_aggregation_resources()
  scenario_panel <- read_rate_panel(rate_panel, rate_panel$tracker_scenario)
  actual_panel   <- read_rate_panel(rate_panel, 'actual')
  baseline_slice <- slice_panel_at(actual_panel, rate_panel$baseline_date)

  # ---- snapshot dates = scenario interval starts; reference date must be one ----
  snapshot_dates <- sort(unique(scenario_panel$valid_from))
  time_varying <- length(snapshot_dates) > 1
  if (!gtap_reference_date %in% snapshot_dates) {
    stop('gtap_reference_date (', gtap_reference_date, ') is not an interval start in the ',
         'scenario panel. Interval starts: ', paste(snapshot_dates, collapse = ', '))
  }

  # ---- per-date rollups ----
  gtap_by_date <- list()
  bea_by_date  <- list()
  ref_partner_gtap <- NULL

  for (d in as.list(snapshot_dates)) {
    slice <- slice_panel_at(scenario_panel, d)
    pairs <- build_pair_frame(slice, baseline_slice, resources)

    gtap_long <- rollup_gtap(pairs)
    bea_long  <- rollup_bea(pairs, resources$hs10_bea)

    gtap_by_date[[as.character(d)]] <- gtap_long %>% mutate(date = d)
    bea_by_date[[as.character(d)]]  <- bea_long  %>% mutate(date = d)

    if (d == gtap_reference_date) {
      ref_partner_gtap <- gtap_long
    }
  }

  gtap_all <- bind_rows(gtap_by_date)
  bea_all  <- bind_rows(bea_by_date)

  # ---- write scenario matrices (deltas + levels), pp ----
  write_matrix <- function(value_col, file) {
    path <- file.path(out_dir, file)
    if (time_varying) {
      wide <- gtap_all %>%
        group_split(date) %>%
        map_dfr(~ to_sector_country_wide(.x, value_col) %>% mutate(date = .x$date[1])) %>%
        select(date, everything())
    } else {
      wide <- to_sector_country_wide(gtap_all, value_col)
    }
    write_csv(wide, path)
    message(sprintf('  Wrote %s (pp%s)', path, if (time_varying) ', stacked by date' else ''))
  }
  write_matrix('etr',   'gtap_deltas_by_sector_country.csv')
  write_matrix('level', 'gtap_levels_by_sector_country.csv')

  # ---- write BEA deltas (fraction) ----
  bea_path <- file.path(out_dir, 'bea_deltas_by_commodity.csv')
  bea_out <- if (time_varying) bea_all %>% select(date, bea_code, etr_delta, total_imports)
             else bea_all %>% select(bea_code, etr_delta, total_imports)
  write_csv(bea_out, bea_path)
  message(sprintf('  Wrote %s (%d commodity-dates)', bea_path, nrow(bea_out)))

  # ---- write static baseline levels matrix ----
  baseline_pairs <- build_pair_frame(baseline_slice, baseline_slice, resources)
  baseline_levels_wide <- to_sector_country_wide(rollup_gtap(baseline_pairs), 'level')
  baseline_path <- file.path(baseline_dir, 'gtap_levels_by_sector_country.csv')
  write_csv(baseline_levels_wide, baseline_path)
  message(sprintf('  Wrote %s (baseline levels, pp)', baseline_path))

  # ---- write shocks.txt for the reference date ----
  shocks_path <- file.path(out_dir, 'shocks.txt')
  write_shocks(ref_partner_gtap, shocks_path)

  invisible(c(out_dir, baseline_path))
}
