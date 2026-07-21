# =============================================================================
# 13_export_dashboard.R - Export model results as per-figure dashboard CSVs
# =============================================================================
#
# Successor to the retired 12_export_excel.R / update_state_of_tariffs.R Excel
# pipeline. The State of Tariffs report is now an interactive dashboard hosted in
# the budget-lab-interactives repo (GitHub Pages, ai-labor-market-tracker
# pattern: one folder per figure with config.md + long-format data.csv). This
# model builds ONLY the data side: per-figure long-format CSVs published to the
# interface tree, hand-copied into the interactives repo via PR.
#
# Platform contract:
#   line charts       -> columns: time, series, value (+ optional selectors)
#   bar/table charts  -> columns: category[, series], value + snake_case selectors
#
# Units: PERCENT (6.22 = 6.22%). Model result CSVs already carry percent; only
# tracker daily fractions get x100. Dollars stay dollars; revenue in $ billions.
# `time` is an ISO date; quarterly figures use a mid-quarter date (2025-03-15 =
# 2025Q1). Booleans are 0/1 integers.
#
# Three tabs:
#   statutory-rates      (Tab 2) : tracker daily pass-throughs, both series stacked
#   default-scenario     (Tab 3) : the default model scenario only
#   alternative-scenarios(Tab 4) : all scenarios stacked + deltas vs default
#
# Everything fails loudly: missing files, shape violations, NA in an output, a
# non-1:1 delta join, or an incoherent tracker vintage across scenarios all stop
# the run.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# interfaces.R  -> read_interface_config(), tariff_model_version(), git_commit()
# helpers.R     -> blend_usmm_gdp_deviation()
source('src/interfaces.R')
source('src/helpers.R')


# =============================================================================
# Constants (lifted from 12_export_excel.R)
# =============================================================================

# sector_effects.sector -> display name (10 rows incl. overall_gdp = 'Total')
SECTOR_NAMES <- tribble(
  ~sector,          ~display_name,
  'agriculture',    'Agriculture',
  'mining',         'Mining & Extraction',
  'manufacturing',  'Total Manufacturing',
  'durable',        'Durable Manufacturing',
  'nondurable',     'Nondurable Manufacturing',
  'advanced',       'Advanced Manufacturing',
  'utilities',      'Utilities',
  'construction',   'Construction',
  'services',       'Services',
  'overall_gdp',    'Total'
)

# foreign_gdp.region -> display name (11 rows incl. world = 'Total')
REGION_NAMES <- tribble(
  ~region,        ~display_name,
  'usa',          'USA',
  'china',        'China',
  'canada',       'Canada',
  'mexico',       'Mexico',
  'eu',           'EU',
  'uk',           'UK',
  'japan',        'Japan',
  'fta',          'Free-trade partners',
  'row',          'Rest of World',
  'world',        'Total',
  'world_ex_usa', 'World ex USA'
)

# goods_weighted_etrs.country_code -> display category (etr-by-country)
ETR_COUNTRY_NAMES <- tribble(
  ~country_code, ~category,
  'chn', 'China',
  'ca',  'Canada',
  'mx',  'Mexico',
  'eu',  'EU',
  'jp',  'Japan',
  'uk',  'UK',
  'row', 'Rest of World',
  'fta', 'Free-trade partners',
  'all', 'Total'
)

# daily_by_authority etr_* column -> series label. Multiple columns may map to the
# same series (they are summed): etr_s301br (a Section 301 sub-action) folds into
# section_301. etr_s338 surfaces Section 338 as its own bucket. Both were added to
# the tracker's daily_by_authority after the initial mapping was written; the
# by_authority builder now sums by series so the stacked buckets reconcile to the
# overall ETR.
AUTHORITY_SERIES <- tribble(
  ~col,              ~series,
  'etr_base',        'base',
  'etr_232',         'section_232',
  'etr_301',         'section_301',
  'etr_s301br',      'section_301',
  'etr_ieepa',       'ieepa',
  'etr_fentanyl',    'fentanyl',
  'etr_s122',        'section_122',
  'etr_s338',        'section_338',
  'etr_section_201', 'section_201',
  'etr_other',       'other'
)

# Thirteen curated summary-statistics metrics. `source` = 'key' (key_results.csv,
# looked up by `key`) or 'sector' (sector_effects.csv, overall_gdp). Values are
# already in the stated unit; nothing is rescaled.
SUMMARY_METRICS <- tribble(
  ~metric,                ~category,                 ~series,                          ~unit,    ~source, ~key,
  'etr_statutory_level',  'Tariff rate',             'Average statutory rate',         'pct',    'key',   'statutory_all_in_etr',
  'etr_presub_level',     'Tariff rate',             'Effective rate, pre-substitution',  'pct', 'key',   'pre_sub_all_in_etr',
  'etr_postsub_level',    'Tariff rate',             'Effective rate, post-substitution', 'pct', 'key',   'pe_postsub_all_in_etr',
  'revenue_conventional', 'Revenue (10-year)',       'Conventional',      'usd_bn', 'key',   'conventional_revenue_10yr',
  'revenue_dynamic',      'Revenue (10-year)',       'Dynamic',           'usd_bn', 'key',   'dynamic_revenue_10yr',
  'price_presub',         'Consumer price increase', 'Pre-substitution',  'pct',    'key',   'pre_sub_price_increase',
  'price_postsub',        'Consumer price increase', 'Post-substitution', 'pct',    'key',   'pe_postsub_price_increase',
  'hh_cost_presub',       'Household cost',          'Pre-substitution',  'usd',    'key',   'pre_sub_per_hh_cost',
  'hh_cost_postsub',      'Household cost',          'Post-substitution', 'usd',    'key',   'post_sub_per_hh_cost',
  'gdp_2025_q4q4',        'Real GDP',                '2025 Q4/Q4',        'pp',     'key',   'gdp_2025_q4q4',
  'gdp_2026_q4q4',        'Real GDP',                '2026 Q4/Q4',        'pp',     'key',   'gdp_2026_q4q4',
  'gdp_long_run',         'Real GDP',                'Long-run',          'pct',    'sector','overall_gdp',
  'unemployment_2026_q4', 'Unemployment rate',       '2026 Q4',           'pp',     'key',   'urate_2026_q4'
)

# Census cty_code partner label (country_partner_mapping) -> partner group code
# used by daily-rate-by-country (matches goods_weighted_etrs.country_code codes).
# Countries absent from the mapping fall through to 'row'.
PARTNER_GROUP_CODE <- c(
  china = 'chn', canada = 'ca', mexico = 'mx', eu = 'eu',
  japan = 'jp',  uk = 'uk',     ftrow = 'fta'
)
PARTNER_GROUP_LABEL <- c(
  chn = 'China', ca = 'Canada', mx = 'Mexico', eu = 'EU',
  jp = 'Japan',  uk = 'UK',     fta = 'Free-trade partners', row = 'Rest of World'
)

RESULT_FILES <- c(
  'key_results', 'goods_weighted_etrs', 'revenue_by_year', 'dynamic_revenue_by_year',
  'macro_quarterly', 'sector_effects', 'foreign_gdp', 'distribution',
  'distribution_postsub', 'pce_major_category_prices'
)

TABS <- c(statutory = 'statutory-rates',
          default   = 'default-scenario',
          alternative = 'alternative-scenarios')

GTAP_SECTORS_FILE  <- 'resources/mappings/gtap_sectors.csv'
COUNTRY_PARTNER_FILE <- 'resources/rate_aggregation/2024/country_partner_mapping.csv'


# =============================================================================
# Config
# =============================================================================

#' Read + validate config/dashboard.yaml
#'
#' @return list(scenarios = tibble(id, label, short_label, default), default_id)
read_dashboard_config <- function(path = 'config/dashboard.yaml') {
  if (!file.exists(path)) stop('Dashboard config not found: ', path)
  raw <- yaml::read_yaml(path)
  if (is.null(raw$scenarios) || length(raw$scenarios) == 0) {
    stop('config/dashboard.yaml has no scenarios')
  }

  scen <- map_dfr(raw$scenarios, function(s) {
    tibble(
      id          = s$id %||% NA_character_,
      label       = s$label %||% NA_character_,
      short_label = s$short_label %||% NA_character_,
      default     = isTRUE(s$default)
    )
  })

  if (any(is.na(scen$id)))    stop('Every dashboard scenario needs an id')
  if (any(duplicated(scen$id))) stop('Duplicate scenario ids in config/dashboard.yaml: ',
                                      paste(scen$id[duplicated(scen$id)], collapse = ', '))
  if (sum(scen$default) != 1L) stop('config/dashboard.yaml must set exactly one scenario default: true (found ',
                                     sum(scen$default), ')')

  list(scenarios = scen, default_id = scen$id[scen$default])
}


# =============================================================================
# Roots (model interface tree + tracker bundle)
# =============================================================================

#' Root of this model's published outputs for an interface vintage:
#'   {production|local}/model_data/Tariff-Model/v<version>/<vintage>
dashboard_read_root <- function(interface_vintage, write_local = FALSE,
                                cfg = read_interface_config()) {
  root <- if (write_local) cfg$roots$local else cfg$roots$production
  if (is.null(root)) stop('No output root configured in config/interfaces/output_roots.yaml')
  file.path(root, 'model_data', 'Tariff-Model',
            paste0('v', tariff_model_version(cfg)), interface_vintage)
}

#' Tracker bundle root. Tracker daily files are ALWAYS read from production
#' (per the dashboard contract), regardless of --write-local. An explicit
#' TARIFF_RATE_TRACKER_ROOT env var still wins, matching read_rate_panel.R.
tracker_root <- function(cfg = read_interface_config()) {
  env_root <- Sys.getenv('TARIFF_RATE_TRACKER_ROOT', unset = NA_character_)
  if (!is.na(env_root) && nzchar(env_root)) return(env_root)
  prod <- cfg$roots$production
  if (is.null(prod)) stop('No production root configured; cannot locate the tracker bundle')
  file.path(prod, 'model_data', 'Tariff-Rate-Tracker')
}

#' Daily-file directory for one tracker series. Mirrors resolve_series_dir()
#' in src/read_rate_panel.R:
#'   <root>/<vintage>/{actual | scenarios/<name>}/daily
tracker_daily_dir <- function(tracker_vintage, tracker_scenario, cfg = read_interface_config()) {
  vintage_dir <- file.path(tracker_root(cfg), tracker_vintage)
  series_dir <- if (tracker_scenario == 'actual') {
    file.path(vintage_dir, 'actual')
  } else {
    file.path(vintage_dir, 'scenarios', tracker_scenario)
  }
  file.path(series_dir, 'daily')
}

#' Vintage-level import-weight base (not series-specific). Mirrors
#' resolve_weights_path() in src/read_rate_panel.R.
tracker_weights_path <- function(tracker_vintage, cfg = read_interface_config()) {
  wdir <- file.path(tracker_root(cfg), tracker_vintage, 'weights')
  candidates <- file.path(wdir, c('import_weights_hs10_country.parquet',
                                  'import_weights_hs10_country.csv.gz'))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop('No import weights found for tracker vintage ', tracker_vintage, ' at ', wdir)
  }
  hit[1]
}


# =============================================================================
# Loading published scenario results + dependency stamps
# =============================================================================

#' Load a scenario's published result CSVs from the interface tree.
#'
#' Successor to load_model_outputs() (12_export_excel.R). Reads the published
#' scenario dir (same location build_bridge.R reads key_results from). Hard-stops
#' on any missing required file.
load_scenario_results <- function(scenario, read_root) {
  scen_dir <- file.path(read_root, scenario)
  if (!dir.exists(scen_dir)) {
    stop('Scenario dir not found in interface tree: ', scen_dir,
         '\n  (has the scenario been run + published at this vintage?)')
  }
  outputs <- list()
  for (name in RESULT_FILES) {
    path <- file.path(scen_dir, paste0(name, '.csv'))
    if (!file.exists(path)) stop('Required result file not found: ', path)
    outputs[[name]] <- read_csv(path, show_col_types = FALSE)
  }
  outputs
}

#' Read one scenario's tracker dependency stamp (dependencies.csv), returning the
#' single Tariff-Rate-Tracker row (tracker vintage + series consumed).
read_scenario_deps <- function(scenario, read_root) {
  path <- file.path(read_root, scenario, 'dependencies.csv')
  if (!file.exists(path)) stop('dependencies.csv not found for scenario: ', path)
  deps <- read_csv(path, show_col_types = FALSE)
  trk <- deps %>% filter(interface == 'Tariff-Rate-Tracker')
  if (nrow(trk) != 1L) {
    stop('Expected exactly one Tariff-Rate-Tracker dependency row for ', scenario,
         ', found ', nrow(trk))
  }
  tibble(scenario = scenario,
         tracker_version  = trk$version[1],
         tracker_vintage  = as.character(trk$vintage[1]),
         tracker_scenario = as.character(trk$scenario[1]))
}

#' Assert every scenario consumed the SAME tracker vintage; return it.
assert_common_tracker_vintage <- function(deps_tbl) {
  vints <- unique(deps_tbl$tracker_vintage)
  if (length(vints) != 1L) {
    stop('Scenarios consumed different tracker vintages: ',
         paste(sprintf('%s=%s', deps_tbl$scenario, deps_tbl$tracker_vintage), collapse = ', '),
         '\n  A single dashboard build requires one coherent tracker vintage.')
  }
  vints
}


# =============================================================================
# write_figure() — the single output choke point
# =============================================================================

#' Validate + write one figure's long-format data.csv.
#'
#' @param df tibble to write
#' @param tab tab dir name (e.g. 'default-scenario')
#' @param slug figure slug (folder name under the tab)
#' @param dashboard_root the .../dashboard output dir
#' @param kind 'line' (needs time/series/value) or 'categorical' (needs category/value)
#' @return invisibly, the data.csv path
write_figure <- function(df, tab, slug, dashboard_root, kind) {
  ctx <- sprintf('%s/%s', tab, slug)

  required <- switch(kind,
    line        = c('time', 'series', 'value'),
    categorical = c('category', 'value'),
    stop('write_figure: unknown kind "', kind, '" for ', ctx)
  )
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(ctx, ': missing required ', kind, ' columns: ', paste(missing_cols, collapse = ', '))
  }

  if (nrow(df) == 0) stop(ctx, ': figure has zero rows')

  # No NA anywhere — a missing value in a published figure is a build bug.
  na_cols <- names(df)[map_lgl(df, ~ any(is.na(.)))]
  if (length(na_cols) > 0) {
    stop(ctx, ': NA values in column(s): ', paste(na_cols, collapse = ', '))
  }

  # value must be finite numeric.
  if (!is.numeric(df$value)) stop(ctx, ': `value` must be numeric')
  if (any(!is.finite(df$value))) stop(ctx, ': `value` has non-finite entries')

  # time must parse as a date (line figures).
  if (kind == 'line') {
    parsed <- suppressWarnings(as.Date(as.character(df$time)))
    if (any(is.na(parsed))) stop(ctx, ': `time` does not parse as ISO date')
  }

  out_dir <- file.path(dashboard_root, 'data', tab, slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, 'data.csv')
  write_csv(df, out_path)
  message(sprintf('    wrote %-24s %4d rows  (%s)', slug, nrow(df), ctx))
  invisible(out_path)
}


# =============================================================================
# Tab 3 / Tab 4 per-figure builders
#   Each is PURE: outputs (one scenario) -> level tibble (measure = 'level').
#   Selector columns are snake_case; values are already in percent/$ units.
# =============================================================================

build_summary_stats <- function(outputs) {
  kr <- outputs$key_results %>% select(metric, value) %>% deframe()
  lr_gdp <- outputs$sector_effects %>%
    filter(sector == 'overall_gdp') %>% pull(output_change_pct)

  # Base mapply (no data-masking) so the `key` COLUMN of SUMMARY_METRICS doesn't
  # shadow the `kr` key_results lookup vector.
  lookup <- function(src, k) {
    if (src == 'key') {
      if (!k %in% names(kr)) NA_real_ else unname(kr[[k]])
    } else {
      lr_gdp
    }
  }
  res <- SUMMARY_METRICS
  res$value <- mapply(lookup, res$source, res$key, USE.NAMES = FALSE)

  missing <- res$metric[is.na(res$value)]
  if (length(missing) > 0) {
    stop('summary-statistics: missing metric(s): ', paste(missing, collapse = ', '))
  }
  if (nrow(res) != 13L) stop('summary-statistics: expected 13 metrics, got ', nrow(res))

  res %>% transmute(category, series, value, unit, metric)
}


build_etr_by_country <- function(outputs) {
  etrs <- outputs$goods_weighted_etrs
  # Levels (baseline + increase), not increases; stacked pre / post.
  long <- etrs %>%
    select(country_code, presub = presub_level, postsub = pe_postsub_level) %>%
    pivot_longer(c(presub, postsub), names_to = 'substitution', values_to = 'value') %>%
    inner_join(ETR_COUNTRY_NAMES, by = 'country_code')

  n_cat <- n_distinct(long$category)
  if (n_cat != 9L) stop('etr-by-country: expected 9 categories, got ', n_cat)

  long %>%
    mutate(country_code = if_else(country_code == 'all', 'total', country_code)) %>%
    transmute(category, value, country_code, substitution)
}


build_consumer_prices <- function(outputs) {
  prices <- outputs$pce_major_category_prices
  cat_rows <- prices %>%
    arrange(desc(pre_sub)) %>%
    transmute(major_category, group = top_level,
              presub = pre_sub, postsub = pe_postsub) %>%
    pivot_longer(c(presub, postsub), names_to = 'substitution', values_to = 'value') %>%
    transmute(category = major_category, value, group, substitution)

  # Overall PCE price increase as a 'Total' bar (plan: "total bars at very top").
  # Sourced from key_results so it equals the headline shown on summary-statistics;
  # group = 'Total' lets the front-end delineate it from the category bars.
  kr <- outputs$key_results %>% select(metric, value) %>% deframe()
  need <- c('pre_sub_price_increase', 'pe_postsub_price_increase')
  miss <- setdiff(need, names(kr))
  if (length(miss) > 0) stop('consumer-prices total: missing key_results metric(s): ',
                             paste(miss, collapse = ', '))
  total_rows <- tibble(
    category     = 'Total',
    group        = 'Total',
    substitution = c('presub', 'postsub'),
    value        = c(kr[['pre_sub_price_increase']], kr[['pe_postsub_price_increase']])
  )

  bind_rows(total_rows, cat_rows)
}


#' @param concept 'pct' (pct_of_income) or 'dollars' (cost_per_hh)
build_distribution <- function(outputs, concept) {
  col <- switch(concept, pct = 'pct_of_income', dollars = 'cost_per_hh',
                stop('build_distribution: unknown concept "', concept, '"'))

  # All-household 'Total' burden, computed from the same deciles shown (plan:
  # toggleable total). Deciles are equal-population, so the population figure is
  # the aggregate over deciles: dollars = mean per-hh cost (equals the headline
  # per_hh_cost); pct = aggregate cost / aggregate income.
  overall <- function(df) {
    switch(concept,
      dollars = mean(df$cost_per_hh),
      pct     = sum(df$cost_per_hh) / sum(df$income) * 100)
  }

  one <- function(df, sub) {
    if (nrow(df) != 10L) stop('distribution (', sub, '): expected 10 deciles, got ', nrow(df))
    deciles <- df %>%
      arrange(decile) %>%
      # Burden is reported as a negative magnitude (sign convention normalised
      # with -abs(), matching the retired build_f5).
      transmute(category = as.character(decile),
                value = -abs(.data[[col]]),
                substitution = sub)
    total <- tibble(category = 'Total', value = -abs(overall(df)), substitution = sub)
    bind_rows(deciles, total)
  }

  bind_rows(
    one(outputs$distribution, 'presub'),
    one(outputs$distribution_postsub, 'postsub')
  )
}


build_real_gdp_path <- function(outputs) {
  macro <- outputs$macro_quarterly
  gtap_lr_gdp <- outputs$foreign_gdp %>% filter(region == 'usa') %>% pull(gdp_change_pct)
  if (length(gtap_lr_gdp) != 1L) stop('real-gdp: could not find foreign_gdp usa row')

  res <- macro %>%
    filter(year >= 2025) %>%
    arrange(year, quarter) %>%
    mutate(
      time = as.Date(sprintf('%d-%02d-15', year, quarter * 3)),
      raw_deviation = (gdp_tariff - gdp_baseline) / gdp_baseline * 100,
      value = blend_usmm_gdp_deviation(
        year = year, quarter = quarter,
        raw_deviation = raw_deviation, gtap_long_run_gdp = gtap_lr_gdp
      ),
      series = 'gdp_level'
    )

  n_q <- nrow(res)
  if (n_q != 44L) stop('real-gdp: expected 44 quarters (2025Q1-2035Q4), got ', n_q)

  res %>% transmute(time, series, value)
}


build_gdp_by_category <- function(outputs) {
  sectors <- outputs$sector_effects %>%
    left_join(SECTOR_NAMES, by = 'sector')
  if (any(is.na(sectors$display_name))) {
    stop('gdp-by-category: unmapped sector(s): ',
         paste(sectors$sector[is.na(sectors$display_name)], collapse = ', '))
  }
  if (nrow(sectors) != 10L) stop('gdp-by-category: expected 10 sectors, got ', nrow(sectors))
  sector_rows <- sectors %>%
    transmute(category = display_name, value = output_change_pct,
              dimension = 'sector', category_code = sector)

  regions <- outputs$foreign_gdp %>%
    left_join(REGION_NAMES, by = 'region')
  if (any(is.na(regions$display_name))) {
    stop('gdp-by-category: unmapped region(s): ',
         paste(regions$region[is.na(regions$display_name)], collapse = ', '))
  }
  if (nrow(regions) != 11L) stop('gdp-by-category: expected 11 regions, got ', nrow(regions))
  region_rows <- regions %>%
    transmute(category = display_name, value = gdp_change_pct,
              dimension = 'country', category_code = region)

  bind_rows(sector_rows, region_rows)
}


build_revenue_by_year <- function(outputs) {
  conv <- outputs$revenue_by_year %>%
    filter(fiscal_year >= 2026) %>%
    transmute(fiscal_year, conventional = net_revenue)
  dyn <- outputs$dynamic_revenue_by_year %>%
    filter(fiscal_year >= 2026) %>%
    transmute(fiscal_year, dynamic = dynamic_revenue)

  years <- sort(unique(conv$fiscal_year))
  if (!identical(as.integer(years), 2026:2035)) {
    stop('revenue-by-year: fiscal years not gapless 2026-2035: ', paste(years, collapse = ', '))
  }

  merged <- conv %>% inner_join(dyn, by = 'fiscal_year')
  if (nrow(merged) != 10L) stop('revenue-by-year: expected 10 fiscal years after join, got ', nrow(merged))

  totals <- tibble(fiscal_year = NA_integer_,
                   conventional = sum(merged$conventional),
                   dynamic = sum(merged$dynamic))

  bind_rows(
    merged %>% mutate(category = as.character(fiscal_year)),
    totals %>% mutate(category = 'Total')
  ) %>%
    select(category, conventional, dynamic) %>%
    pivot_longer(c(conventional, dynamic), names_to = 'series', values_to = 'value')
}


# Registry of the 8 scenario figures: slug -> (builder, kind).
SCENARIO_FIGURES <- list(
  list(slug = 'summary-statistics',      kind = 'categorical', build = function(o) build_summary_stats(o)),
  list(slug = 'etr-by-country',          kind = 'categorical', build = function(o) build_etr_by_country(o)),
  list(slug = 'consumer-prices',         kind = 'categorical', build = function(o) build_consumer_prices(o)),
  list(slug = 'distribution-pct-income', kind = 'categorical', build = function(o) build_distribution(o, 'pct')),
  list(slug = 'distribution-dollars',    kind = 'categorical', build = function(o) build_distribution(o, 'dollars')),
  list(slug = 'real-gdp',                kind = 'line',        build = function(o) build_real_gdp_path(o)),
  list(slug = 'gdp-by-category',         kind = 'categorical', build = function(o) build_gdp_by_category(o)),
  list(slug = 'revenue-by-year',         kind = 'categorical', build = function(o) build_revenue_by_year(o))
)


# =============================================================================
# Tab 4 delta machinery
# =============================================================================

#' Given a figure stacked across scenarios (with a `scenario` column and
#' measure == 'level'), append measure == 'delta_vs_default' rows for every
#' non-default scenario (non-default level - default level). Asserts a clean
#' 1:1 join on the figure's id columns (anti-join both ways empty).
add_scenario_deltas <- function(df, default_id) {
  keys <- setdiff(names(df), c('value', 'scenario', 'measure'))

  default_rows <- df %>% filter(scenario == default_id) %>%
    select(all_of(keys), default_value = value)
  nondefault <- df %>% filter(scenario != default_id)

  for (sc in unique(nondefault$scenario)) {
    nd <- nondefault %>% filter(scenario == sc) %>% select(all_of(keys))
    dk <- default_rows %>% select(all_of(keys))
    if (nrow(anti_join(nd, dk, by = keys)) > 0 ||
        nrow(anti_join(dk, nd, by = keys)) > 0) {
      stop('add_scenario_deltas: non-1:1 join between scenario "', sc,
           '" and default on keys (', paste(keys, collapse = ', '), ')')
    }
  }

  deltas <- nondefault %>%
    left_join(default_rows, by = keys) %>%
    mutate(value = value - default_value, measure = 'delta_vs_default') %>%
    select(-default_value)

  bind_rows(df, deltas)
}


# =============================================================================
# Tab 2 tracker daily pass-throughs
# =============================================================================

#' 0/1 projected flag from a tracker `revision` label (bnd_* / 2026_* = projected)
flag_projected <- function(revision) as.integer(grepl('^(bnd_|2026_)', revision))

#' GTAP code -> series description, joined on tolower(gtap_code); 'unmapped' ->
#' 'Unmapped'. Fails loudly on any other unmatched code.
gtap_sector_labels <- function() {
  gs <- read_csv(GTAP_SECTORS_FILE, show_col_types = FALSE) %>%
    transmute(gtap_code = tolower(gtap_code), description)
  if (any(duplicated(gs$gtap_code))) stop('Duplicate gtap_code in ', GTAP_SECTORS_FILE)
  gs
}

#' Build a long tracker-daily figure for overall / by_authority / by_category /
#' by_hs (HS-based product breakdown).
#' Returns time, series, value(+gtap_code for category), projected. `scenario`
#' is added by the caller.
build_daily_rates <- function(daily_dir, which) {
  if (which == 'overall') {
    path <- file.path(daily_dir, 'daily_overall.csv')
    if (!file.exists(path)) stop('tracker daily file not found: ', path)
    # `new_tariffs` = weighted_etr_new, the tariffs imposed since the 2025-01-01
    # baseline (0 on the baseline date). This replaces weighted_etr_additional
    # (above-MFN), which the front-end mislabeled as "above baseline"; requires
    # tracker vintage >= 2026-07-16-09.
    read_csv(path, show_col_types = FALSE) %>%
      transmute(time = as.Date(date),
                total = weighted_etr * 100,
                new_tariffs = weighted_etr_new * 100,
                projected = flag_projected(revision)) %>%
      pivot_longer(c(total, new_tariffs), names_to = 'series', values_to = 'value') %>%
      select(time, series, value, projected)

  } else if (which == 'by_authority') {
    path <- file.path(daily_dir, 'daily_by_authority.csv')
    if (!file.exists(path)) stop('tracker daily file not found: ', path)
    raw <- read_csv(path, show_col_types = FALSE)
    missing <- setdiff(AUTHORITY_SERIES$col, names(raw))
    if (length(missing) > 0) stop('daily_by_authority missing etr columns: ',
                                  paste(missing, collapse = ', '))
    raw %>%
      transmute(time = as.Date(date), projected = flag_projected(revision),
                across(all_of(AUTHORITY_SERIES$col), ~ .x * 100)) %>%
      pivot_longer(all_of(AUTHORITY_SERIES$col), names_to = 'col', values_to = 'value') %>%
      left_join(AUTHORITY_SERIES, by = 'col') %>%
      group_by(time, series, projected) %>%
      summarise(value = sum(value), .groups = 'drop') %>%
      select(time, series, value, projected)

  } else if (which == 'by_category') {
    path <- file.path(daily_dir, 'daily_by_category.csv')
    if (!file.exists(path)) stop('tracker daily file not found: ', path)
    labels <- gtap_sector_labels()
    # The 'unmapped' residual bucket carries NA weighted_etr on every date (the
    # tracker computes no rate for it), so it can only ever be an all-NA series.
    # Drop NA-rate rows; in this vintage that is exactly the unmapped bucket.
    # (Plan intended to keep 'Unmapped', but the source has no value to show.)
    raw <- read_csv(path, show_col_types = FALSE) %>%
      filter(!is.na(weighted_etr)) %>%
      mutate(gtap_code = tolower(gtap_code))

    # The tracker emits some GTAP concepts as BOTH an upper- and lower-case row
    # on the same date (e.g. OXT + oxt), split across product subsets, with no
    # per-category import $ to merge them exactly. Collapse each lower-cased
    # code with an n_products_present-weighted mean of weighted_etr (documented
    # approximation; see the async-mapping-cat plan review).
    merged <- raw %>%
      group_by(date, gtap_code, revision) %>%
      summarise(
        weighted_etr = if (sum(n_products_present) > 0)
          sum(weighted_etr * n_products_present) / sum(n_products_present)
          else mean(weighted_etr),
        .groups = 'drop'
      )

    labelled <- merged %>%
      mutate(series = if_else(gtap_code == 'unmapped', 'Unmapped',
                              labels$description[match(gtap_code, labels$gtap_code)]))
    unmatched <- labelled %>% filter(is.na(series)) %>% distinct(gtap_code) %>% pull(gtap_code)
    if (length(unmatched) > 0) stop('daily_by_category: unmapped gtap_code(s): ',
                                    paste(unmatched, collapse = ', '))

    labelled %>%
      transmute(time = as.Date(date), series, value = weighted_etr * 100,
                gtap_code, projected = flag_projected(revision))

  } else if (which == 'by_hs') {
    path <- file.path(daily_dir, 'daily_by_hs.csv')
    if (!file.exists(path)) stop('tracker daily file not found: ', path)
    # HS-based product breakdown (Budget Lab Tariff HS Aggregation). Unlike the
    # GTAP by_category file, the tracker keys this on a stable category_code +
    # category_label and emits one clean row per (date, category) -- no
    # upper/lower-case dup to collapse and no external label join. The
    # 'unclassified' residual (HS ch 98-99) carries essentially no weight; it is
    # kept as-is. Defensive NA-rate drop mirrors by_category.
    read_csv(path, show_col_types = FALSE) %>%
      filter(!is.na(weighted_etr)) %>%
      transmute(time = as.Date(date), series = category_label,
                value = weighted_etr * 100, category_code,
                projected = flag_projected(revision))

  } else {
    stop('build_daily_rates: unknown which "', which, '"')
  }
}


#' Per-country import totals (census cty_code -> total imports) for one tracker
#' vintage, from the vintage import-weight base.
country_import_totals <- function(weights_path) {
  w <- if (grepl('\\.parquet$', weights_path)) {
    arrow::read_parquet(weights_path, col_select = c('country', 'imports'))
  } else {
    read_csv(weights_path, show_col_types = FALSE) %>% select(country, imports)
  }
  w %>%
    mutate(cty_code = as.numeric(country)) %>%
    group_by(cty_code) %>%
    summarise(imports = sum(imports, na.rm = TRUE), .groups = 'drop')
}

#' daily_by_country -> the 8 partner groups (chn, ca, mx, eu, jp, uk, fta, row),
#' import-weighted mean of country weighted_etr. Returns time, series, value,
#' country_code, projected.
#'
#' NA weighted_etr rows are dropped BEFORE weighting. In this vintage that is the
#' US self-row (cty 1000) plus zero-trade jurisdictions (North Korea + US
#' territories); they carry no rate and would otherwise poison the 'row' group.
#' (Plan sanctioned only cty 1000; this drops all NA weighted_etr — see review.)
build_daily_by_country <- function(daily_dir, weights_path) {
  path <- file.path(daily_dir, 'daily_by_country.csv')
  if (!file.exists(path)) stop('tracker daily file not found: ', path)

  partner_map <- read_csv(COUNTRY_PARTNER_FILE, show_col_types = FALSE) %>%
    transmute(cty_code = as.numeric(cty_code),
              group = unname(PARTNER_GROUP_CODE[partner]))
  if (any(is.na(partner_map$group))) {
    stop('country_partner_mapping has partner value(s) not in PARTNER_GROUP_CODE: ',
         paste(unique(read_csv(COUNTRY_PARTNER_FILE, show_col_types = FALSE)$partner), collapse = ', '))
  }

  totals <- country_import_totals(weights_path)

  raw <- read_csv(path, show_col_types = FALSE) %>%
    filter(!is.na(weighted_etr)) %>%
    mutate(cty_code = as.numeric(country))

  grouped <- raw %>%
    left_join(partner_map, by = 'cty_code') %>%
    mutate(group = coalesce(group, 'row')) %>%
    left_join(totals, by = 'cty_code') %>%
    mutate(imports = coalesce(imports, 0)) %>%
    group_by(time = as.Date(date), group, revision) %>%
    summarise(
      value = if (sum(imports) > 0)
        sum(weighted_etr * imports) / sum(imports) * 100
        else weighted.mean(weighted_etr, w = rep(1, n())) * 100,
      .groups = 'drop'
    ) %>%
    mutate(series = unname(PARTNER_GROUP_LABEL[group]),
           projected = flag_projected(revision))

  if (any(is.na(grouped$series))) stop('daily-rate-by-country: unmapped partner group')

  grouped %>%
    transmute(time, series, value, country_code = group, projected)
}


# =============================================================================
# Tab exporters
# =============================================================================

export_default_tab <- function(default_outputs, dashboard_root) {
  message('  Tab 3: default-scenario')
  tab <- TABS[['default']]
  for (fig in SCENARIO_FIGURES) {
    df <- fig$build(default_outputs)
    write_figure(df, tab, fig$slug, dashboard_root, fig$kind)
  }
}

export_alternatives_tab <- function(scenario_outputs, default_id, dashboard_root) {
  message('  Tab 4: alternative-scenarios')
  tab <- TABS[['alternative']]
  for (fig in SCENARIO_FIGURES) {
    stacked <- imap_dfr(scenario_outputs, function(outs, scen) {
      fig$build(outs) %>% mutate(scenario = scen, measure = 'level')
    })
    with_deltas <- add_scenario_deltas(stacked, default_id)
    write_figure(with_deltas, tab, fig$slug, dashboard_root, fig$kind)
  }
}

export_statutory_tab <- function(deps_tbl, tracker_vintage, dashboard_root,
                                  cfg = read_interface_config()) {
  message('  Tab 2: statutory-rates')
  tab <- TABS[['statutory']]
  weights_path <- tracker_weights_path(tracker_vintage, cfg)

  # scenario -> tracker series daily dir (both series stacked with `scenario`)
  gather_series <- function(builder) {
    imap_dfr(setNames(deps_tbl$tracker_scenario, deps_tbl$scenario),
             function(trk_scen, model_scen) {
               daily_dir <- tracker_daily_dir(tracker_vintage, trk_scen, cfg)
               builder(daily_dir) %>% mutate(scenario = model_scen)
             })
  }

  write_figure(gather_series(function(d) build_daily_rates(d, 'overall')),
               tab, 'daily-rate-overall', dashboard_root, 'line')
  write_figure(gather_series(function(d) build_daily_rates(d, 'by_authority')),
               tab, 'daily-rate-by-authority', dashboard_root, 'line')
  write_figure(gather_series(function(d) build_daily_by_country(d, weights_path)),
               tab, 'daily-rate-by-country', dashboard_root, 'line')
  write_figure(gather_series(function(d) build_daily_rates(d, 'by_category')),
               tab, 'daily-rate-by-category', dashboard_root, 'line')
  write_figure(gather_series(function(d) build_daily_rates(d, 'by_hs')),
               tab, 'daily-rate-by-hs', dashboard_root, 'line')
}


# =============================================================================
# Manifest + dependency stamp
# =============================================================================

write_dashboard_manifest <- function(dashboard_root, interface_vintage, config,
                                     deps_tbl, tracker_vintage, figure_inventory,
                                     write_local, cfg = read_interface_config()) {
  # dependencies.csv: one Tariff-Model row per scenario (the model outputs read)
  # + one Tariff-Rate-Tracker row per scenario (the tracker series consumed).
  model_deps <- config$scenarios %>%
    transmute(ID = id, interface = 'Tariff-Model',
              version = tariff_model_version(cfg), vintage = interface_vintage,
              scenario = id)
  tracker_deps <- deps_tbl %>%
    transmute(ID = scenario, interface = 'Tariff-Rate-Tracker',
              version = tracker_version, vintage = tracker_vintage,
              scenario = tracker_scenario)
  deps_out <- bind_rows(model_deps, tracker_deps)
  write_csv(deps_out, file.path(dashboard_root, 'dependencies.csv'))

  manifest <- list(
    model            = 'Tariff-Model',
    artifact         = 'dashboard',
    version          = tariff_model_version(cfg),
    interface_vintage = interface_vintage,
    published_at     = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z'),
    git_commit       = git_commit(),
    write_local      = write_local,
    tracker_vintage  = tracker_vintage,
    scenarios        = config$scenarios,
    figures          = figure_inventory,
    dependencies     = deps_out
  )
  jsonlite::write_json(manifest, file.path(dashboard_root, 'manifest.json'),
                       auto_unbox = TRUE, pretty = TRUE)
  message(sprintf('  Wrote manifest.json + dependencies.csv (%d dep rows)', nrow(deps_out)))
}


# =============================================================================
# Orchestrator
# =============================================================================

#' Build every dashboard figure for the scenarios in config/dashboard.yaml at one
#' interface vintage, and stamp a manifest + dependencies.
#'
#' @param interface_vintage Vintage id the scenarios were published under
#' @param write_local Read model outputs from / write dashboard under the local
#'   scratch root instead of production (tracker daily always read from production)
#' @param config_path Path to dashboard.yaml
#' @return invisibly, the dashboard output dir
export_dashboard <- function(interface_vintage, write_local = FALSE,
                             config_path = 'config/dashboard.yaml') {
  cfg <- read_interface_config()
  config <- read_dashboard_config(config_path)
  message(sprintf('Dashboard export: vintage %s, %d scenario(s), default = %s',
                  interface_vintage, nrow(config$scenarios), config$default_id))

  read_root <- dashboard_read_root(interface_vintage, write_local, cfg)
  if (!dir.exists(read_root)) stop('Interface vintage dir not found: ', read_root)

  # Validate scenario dirs + coherent tracker vintage up front (fail loudly).
  deps_tbl <- map_dfr(config$scenarios$id, ~ read_scenario_deps(.x, read_root))
  tracker_vintage <- assert_common_tracker_vintage(deps_tbl)
  message(sprintf('  Tracker vintage (coherent across scenarios): %s', tracker_vintage))

  scenario_outputs <- setNames(
    map(config$scenarios$id, ~ load_scenario_results(.x, read_root)),
    config$scenarios$id
  )
  default_outputs <- scenario_outputs[[config$default_id]]

  dashboard_root <- file.path(read_root, 'dashboard')
  dir.create(file.path(dashboard_root, 'data'), recursive = TRUE, showWarnings = FALSE)

  export_statutory_tab(deps_tbl, tracker_vintage, dashboard_root, cfg)
  export_default_tab(default_outputs, dashboard_root)
  export_alternatives_tab(scenario_outputs, config$default_id, dashboard_root)

  figure_inventory <- list(
    statutory = c('daily-rate-overall', 'daily-rate-by-authority',
                  'daily-rate-by-country', 'daily-rate-by-category'),
    default   = map_chr(SCENARIO_FIGURES, 'slug'),
    alternative = map_chr(SCENARIO_FIGURES, 'slug')
  )
  write_dashboard_manifest(dashboard_root, interface_vintage, config, deps_tbl,
                           tracker_vintage, figure_inventory, write_local, cfg)

  message(sprintf('Dashboard export complete -> %s', dashboard_root))
  invisible(dashboard_root)
}
