#!/usr/bin/env Rscript
# =============================================================================
# Tariff headline decomposition bridge
# =============================================================================
# Stitches the per-run key_results.csv outputs into the additive 3-step
# decomposition that explains how the April 8 published headline moved to the
# current model. One variable changes per step; the increments sum to the total.
#
#   Run 0  April-8 rates, pre-MFN-fix code (published)   ]- Residual (code)
#   Run 1  April-8 rates, current code                   ]
#   Run 2  June rates,   eta/alpha OFF (legacy haircut)  ]- Policy update (rates)
#   Run 3  June rates,   full model (calibrated eta/alpha)]- Eta/alpha update (model)
#
#   Residual       = Run 1 - Run 0   (methodology, same policy)
#   Policy update  = Run 2 - Run 1   (new tariff rates, model held fixed)
#   Eta/alpha upd. = Run 3 - Run 2   (turn on calibrated eta + alpha)
#   Total          = Run 3 - Run 0
#
# Runs 0 & 1 are April-8-rate runs that CANNOT be regenerated on current code
# (the old Tariff-ETRs rate path is gone; no April tracker vintage exists), so
# they are pinned here as constants transcribed from the 2026-06-23 build's
# key_results.csv. Runs 2 & 3 (June rates) are read from the shared interface tree
# at <root>/model_data/Tariff-Model/v<ver>/<interface-vintage>/<scenario>/. Passing
# vintage 2026-06-23 pulls runs 2 & 3 from the pinned appendix too, reproducing the
# published table as a correctness check.
#
# Usage:
#   Rscript build_bridge.R [label] --interface-vintage <V> [--local]
#       label (default 2026-07-01) is the scenario-name suffix + report label;
#       <V> is the value passed to run.R --vintage for runs 2 & 3.
#   Rscript build_bridge.R 2026-06-23       # reproduce the published table from the appendix
#
# Writes reports/tariff_decomposition_bridge_<vintage>.{html,csv} and prints a
# console summary. Revenue holds refunds fixed at $166B in every scenario.
# =============================================================================

# `vintage` is the SCENARIO-NAME suffix + report label (e.g. 2026-07-01), NOT the
# interface-tree publish vintage. Runs 2 & 3 are read from the shared interface
# tree at --interface-vintage (the value passed to run.R --vintage); that path
# differs from the scenario label, so the two are separate arguments.
args <- commandArgs(trailingOnly = TRUE)
vintage <- '2026-07-01'
iface_vintage <- NA_character_
write_local <- FALSE
got_positional <- FALSE
i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a == '--interface-vintage' && i < length(args)) {
    iface_vintage <- args[i + 1]; i <- i + 2
  } else if (a == '--local') {
    write_local <- TRUE; i <- i + 1
  } else if (!startsWith(a, '--')) {
    if (!got_positional) { vintage <- a; got_positional <- TRUE }
    i <- i + 1
  } else {
    i <- i + 1
  }
}

if (is.na(iface_vintage) && vintage != '2026-06-23') {
  stop('--interface-vintage <V> is required to read runs 2 & 3 from the interface tree ',
       '(the value you passed to run.R --vintage). Only vintage 2026-06-23 reproduces ',
       'from the pinned appendix without reading disk.')
}

# Resolve the interface-tree root where runs 2 & 3 were published. Mirrors
# tariff_output_root() in src/interfaces.R, read directly from config so this stays
# a dependency-free base-R script. NULL for the 2026-06-23 appendix reproduction.
read_root <- NULL
if (!is.na(iface_vintage)) {
  roots <- yaml::read_yaml('config/interfaces/output_roots.yaml')
  versions <- yaml::read_yaml('config/interfaces/interface_versions.yaml')
  root <- if (write_local) roots$local else roots$production
  if (is.null(root)) stop('No output root configured in config/interfaces/output_roots.yaml')
  ver <- versions[['Tariff-Model']]$version
  if (is.null(ver)) ver <- 1L
  read_root <- file.path(root, 'model_data', 'Tariff-Model', paste0('v', ver), iface_vintage)
}

# --- display metrics: key in key_results.csv -> label + kind (pct|usd|bil) ----
metrics <- data.frame(
  key = c('pre_sub_all_in_etr', 'pe_postsub_all_in_etr',
          'pre_sub_price_increase', 'pe_postsub_price_increase',
          'pre_sub_per_hh_cost', 'post_sub_per_hh_cost',
          'conventional_revenue_10yr', 'dynamic_revenue_10yr'),
  label = c('Pre-sub all-in ETR', 'Post-sub all-in ETR',
            'Pre-sub price effect', 'Post-sub price effect',
            'Pre-sub cost / household', 'Post-sub cost / household',
            'Conventional revenue (10yr)', 'Dynamic revenue (10yr)'),
  kind = c('pct', 'pct', 'pct', 'pct', 'usd', 'usd', 'bil', 'bil'),
  stringsAsFactors = FALSE
)

# --- pinned April-8 anchors + 2026-06-23 reference (from the run appendix) -----
# Only the metrics displayed above are pinned; transcribed from the 2026-06-23
# build's results/key_results.csv (those output dirs are gitignored and predate
# this machine, so the values are recorded here rather than read).
appendix <- list(
  actual = list(
    run0 = c(pre_sub_all_in_etr = 9.657873175834032, pe_postsub_all_in_etr = 8.212036253754134,
             pre_sub_price_increase = 0.67505152383348, pe_postsub_price_increase = 0.5470837992974382,
             pre_sub_per_hh_cost = 938.6537399735269, post_sub_per_hh_cost = 760.7156434123407,
             conventional_revenue_10yr = 1320.728510049375, dynamic_revenue_10yr = 1214.696510049375),
    run1 = c(pre_sub_all_in_etr = 9.404782939104182, pe_postsub_all_in_etr = 7.871634286263469,
             pre_sub_price_increase = 0.47639234394936025, pe_postsub_price_increase = 0.39945385055185306,
             pre_sub_per_hh_cost = 645.2118608412729, post_sub_per_hh_cost = 541.0086150800303,
             conventional_revenue_10yr = 1130.8460607295103, dynamic_revenue_10yr = 1049.2790607295103),
    run2 = c(pre_sub_all_in_etr = 9.08629337724557, pe_postsub_all_in_etr = 7.243167490361853,
             pre_sub_price_increase = 0.48572742679894293, pe_postsub_price_increase = 0.40682258526259524,
             pre_sub_per_hh_cost = 657.8550240931302, post_sub_per_hh_cost = 550.9886139090396,
             conventional_revenue_10yr = 1082.2864211121448, dynamic_revenue_10yr = 990.8984211121448),
    run3 = c(pre_sub_all_in_etr = 9.08629337724557, pe_postsub_all_in_etr = 6.217489871817597,
             pre_sub_price_increase = 0.48572742679894293, pe_postsub_price_increase = 0.26354434324015036,
             pre_sub_per_hh_cost = 657.8550240931302, post_sub_per_hh_cost = 356.9367523971874,
             conventional_revenue_10yr = 1117.8264147331793, dynamic_revenue_10yr = 1048.7614147331792)
  ),
  new_301 = list(
    run0 = c(pre_sub_all_in_etr = 12.237902042799764, pe_postsub_all_in_etr = 10.529989267200538,
             pre_sub_price_increase = 1.0626386122642024, pe_postsub_price_increase = 0.8686474360961214,
             pre_sub_per_hh_cost = 1477.590483727462, post_sub_per_hh_cost = 1207.847306202319,
             conventional_revenue_10yr = 1874.3204761509182, dynamic_revenue_10yr = 1715.7004761509183),
    run1 = c(pre_sub_all_in_etr = 11.984811806069914, pe_postsub_all_in_etr = 10.216236344315757,
             pre_sub_price_increase = 0.8639794323800823, pe_postsub_price_increase = 0.735997452889778,
             pre_sub_per_hh_cost = 1201.356484403432, post_sub_per_hh_cost = 1023.3985664424573,
             conventional_revenue_10yr = 1709.2938102514768, dynamic_revenue_10yr = 1577.1928102514767),
    run2 = c(pre_sub_all_in_etr = 11.241424646727648, pe_postsub_all_in_etr = 8.69410382040324,
             pre_sub_price_increase = 0.8297804553149383, pe_postsub_price_increase = 0.6269944774685876,
             pre_sub_per_hh_cost = 1123.830385738484, post_sub_per_hh_cost = 849.1829868443716,
             conventional_revenue_10yr = 1469.941028384413, dynamic_revenue_10yr = 1316.7620283844128),
    run3 = c(pre_sub_all_in_etr = 11.241424646727648, pe_postsub_all_in_etr = 7.509773487011335,
             pre_sub_price_increase = 0.8297804553149383, pe_postsub_price_increase = 0.4933280744816636,
             pre_sub_per_hh_cost = 1123.830385738484, post_sub_per_hh_cost = 668.1491190702073,
             conventional_revenue_10yr = 1551.8041487384144, dynamic_revenue_10yr = 1422.3681487384144)
  )
)

paths <- list(actual = 'Current-law path (Section 122 expires)',
              new_301 = 'Higher-tariff path (Section 122 extended, new_301)')

# --- fetch one run's metric vector: pinned anchors, or read from disk ----------
read_key_results <- function(scenario) {
  # Runs publish key_results.csv straight into the scenario dir in the interface
  # tree (11_write_outputs.R), NOT a repo-local output/.../results/ path.
  f <- file.path(read_root, scenario, 'key_results.csv')
  if (!file.exists(f)) {
    stop('missing key_results for run: ', f,
         '\n  (run the scenario at --interface-vintage ', iface_vintage,
         ', or use vintage 2026-06-23 to build from the pinned appendix)')
  }
  d <- read.csv(f, stringsAsFactors = FALSE)
  stats::setNames(as.numeric(d$value), d$metric)
}

get_run <- function(path, run_idx, vintage) {
  # Runs 0 & 1 are always pinned; runs 2 & 3 come from the appendix only for the
  # 2026-06-23 reproduction, otherwise from that vintage's run outputs on disk.
  if (run_idx %in% c(0L, 1L) || vintage == '2026-06-23') {
    all_vals <- appendix[[path]][[paste0('run', run_idx)]]
  } else {
    scenario <- if (run_idx == 2L) sprintf('tracker_%s_%s_no_eta_alpha', path, vintage)
                else                sprintf('tracker_%s_%s', path, vintage)
    all_vals <- read_key_results(scenario)
  }
  missing <- setdiff(metrics$key, names(all_vals))
  if (length(missing)) stop('run ', run_idx, ' (', path, ') missing metrics: ',
                            paste(missing, collapse = ', '))
  all_vals[metrics$key]
}

# --- transcription gate: pinned 06-23 run 3 must match the published cells -----
check_golden <- function() {
  r3 <- appendix$actual$run3
  gold <- list(pe_postsub_all_in_etr = 6.217, pe_postsub_price_increase = 0.264,
               post_sub_per_hh_cost = 356.94)
  ok <- TRUE
  for (k in names(gold)) {
    got <- round(r3[[k]], if (k == 'post_sub_per_hh_cost') 2 else 3)
    hit <- isTRUE(all.equal(got, gold[[k]], tolerance = 1e-6))
    ok <- ok && hit
    cat(sprintf('  %-28s pinned=%-10s published=%-8s %s\n',
                k, format(got), format(gold[[k]]), if (hit) 'OK' else 'MISMATCH'))
  }
  if (!ok) stop('golden transcription check failed — pinned appendix values drifted from the published table')
  invisible(TRUE)
}

# --- number formatting ---------------------------------------------------------
fmt_level <- function(v, kind) {
  if (kind == 'pct') sprintf('%.3f%%', v)
  else if (kind == 'usd') paste0('$', formatC(v, format = 'f', digits = 0, big.mark = ','))
  else paste0('$', formatC(v, format = 'f', digits = 1, big.mark = ','), 'B')
}
fmt_delta <- function(v, kind) {
  sign <- if (v >= 0) '+' else '-'
  a <- abs(v)
  if (kind == 'pct') sprintf('%s%.3f pp', sign, a)
  else if (kind == 'usd') paste0(sign, '$', formatC(a, format = 'f', digits = 0, big.mark = ','))
  else paste0(sign, '$', formatC(a, format = 'f', digits = 1, big.mark = ','), 'B')
}

# --- build the per-path table: levels + interleaved increments -----------------
# Returns a list with $levels (4 x metrics matrix) and $rows (ordered display).
build_panel <- function(path, vintage) {
  L <- sapply(0:3, function(i) get_run(path, i, vintage))  # metrics x 4 (run0..run3)
  rownames(L) <- metrics$key
  colnames(L) <- paste0('run', 0:3)
  deltas <- list(
    residual  = L[, 'run1'] - L[, 'run0'],
    policy    = L[, 'run2'] - L[, 'run1'],
    eta_alpha = L[, 'run3'] - L[, 'run2'],
    total     = L[, 'run3'] - L[, 'run0']
  )
  list(levels = L, deltas = deltas)
}

# --- HTML rendering ------------------------------------------------------------
row_defs <- list(
  list(type = 'level', src = 'run0',      label = 'Run 0 &middot; April 8 published (pre-MFN-fix code)'),
  list(type = 'delta', src = 'residual',  label = '&nbsp;&nbsp;&#8627; Residual (bug fix + methodology drift)'),
  list(type = 'level', src = 'run1',      label = 'Run 1 &middot; April 8, current code'),
  list(type = 'delta', src = 'policy',    label = '&nbsp;&nbsp;&#8627; Policy update (April&#8594;June rates)'),
  list(type = 'level', src = 'run2',      label = 'Run 2 &middot; June rates, eta/alpha off'),
  list(type = 'delta', src = 'eta_alpha', label = '&nbsp;&nbsp;&#8627; Eta/alpha update (calibrated model)'),
  list(type = 'level', src = 'run3',      label = 'Run 3 &middot; June rates, full model (current)'),
  list(type = 'delta', src = 'total',     label = 'Total change (Run 3 &minus; Run 0)')
)

render_panel_html <- function(path, panel) {
  L <- panel$levels; D <- panel$deltas
  ncol_metrics <- nrow(metrics)
  th <- paste0('<th>', metrics$label, '</th>', collapse = '')
  body <- ''
  for (rd in row_defs) {
    cls <- if (rd$type == 'level') 'lvl' else 'dlt'
    if (rd$src == 'total') cls <- 'tot'
    cells <- ''
    for (j in seq_len(ncol_metrics)) {
      kind <- metrics$kind[j]
      if (rd$type == 'level') {
        v <- L[metrics$key[j], rd$src]
        cells <- paste0(cells, '<td>', fmt_level(v, kind), '</td>')
      } else {
        v <- D[[rd$src]][metrics$key[j]]
        col <- if (v < 0) 'neg' else if (v > 0) 'pos' else ''
        cells <- paste0(cells, '<td class="', col, '">', fmt_delta(v, kind), '</td>')
      }
    }
    body <- paste0(body, '<tr class="', cls, '"><td class="rl">', rd$label, '</td>', cells, '</tr>\n')
  }
  paste0('<h2>', paths[[path]], '</h2>\n',
         '<table>\n<thead><tr><th class="rl">Step</th>', th, '</tr></thead>\n<tbody>\n',
         body, '</tbody>\n</table>\n')
}

render_html <- function(vintage) {
  src_note <- if (vintage == '2026-06-23')
    'All four runs from the pinned 2026-06-23 build appendix (reproduction / correctness check).'
  else
    sprintf('Runs 0 &amp; 1 pinned from the April-8 appendix (cannot be regenerated on current code); runs 2 &amp; 3 read from the interface tree at v-vintage %s, scenarios tracker_*_%s (2026-06-25-14 tracker vintage).', iface_vintage, vintage)
  panels <- paste0(sapply(names(paths), function(p) render_panel_html(p, build_panel(p, vintage))),
                   collapse = '\n')
  css <- '
    :root{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#1a1a1a}
    body{max-width:1100px;margin:2rem auto;padding:0 1rem;line-height:1.45}
    h1{font-size:1.5rem;margin-bottom:.2rem} h2{font-size:1.1rem;margin:1.6rem 0 .5rem}
    .note{background:#f4f6f8;border:1px solid #dde3e8;border-radius:6px;padding:.7rem .9rem;font-size:.86rem;color:#3a4650}
    table{border-collapse:collapse;width:100%;font-size:.85rem;margin-bottom:.5rem}
    th,td{padding:.35rem .55rem;text-align:right;border-bottom:1px solid #eceff2}
    th{background:#2f3e4e;color:#fff;font-weight:600} th.rl,td.rl{text-align:left}
    tr.lvl td{font-weight:600} tr.dlt td{color:#5a6672;font-style:italic}
    tr.dlt td.rl{font-weight:400} tr.tot td{border-top:2px solid #2f3e4e;font-weight:700;background:#f4f6f8}
    td.neg{color:#0a7d55} td.pos{color:#c0392b}
    .foot{font-size:.78rem;color:#6b7680;margin-top:1rem}'
  paste0('<!doctype html>\n<html><head><meta charset="utf-8">',
         '<title>Tariff decomposition bridge ', vintage, '</title><style>', css, '</style></head><body>\n',
         '<h1>Tariff headline decomposition bridge &middot; ', vintage, '</h1>\n',
         '<div class="note">', src_note,
         ' Each step changes exactly one variable; increments sum to the total. Levels are bold; increments are the italic &#8627; rows.</div>\n',
         panels,
         '<p class="foot">Revenue holds refunds fixed at $166B in every scenario. ',
         'Post-sub = post-substitution. ETR/price deltas in percentage points; cost in dollars; revenue in billions (10-year).</p>\n',
         '</body></html>\n')
}

# --- long CSV export -----------------------------------------------------------
build_csv <- function(vintage) {
  out <- list()
  for (p in names(paths)) {
    panel <- build_panel(p, vintage)
    for (j in seq_len(nrow(metrics))) {
      k <- metrics$key[j]
      for (r in 0:3) out[[length(out) + 1]] <- data.frame(
        path = p, stage = paste0('run', r), kind = 'level',
        metric = k, value = panel$levels[k, paste0('run', r)], stringsAsFactors = FALSE)
      for (d in names(panel$deltas)) out[[length(out) + 1]] <- data.frame(
        path = p, stage = d, kind = 'increment',
        metric = k, value = panel$deltas[[d]][k], stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# --- console summary (post-sub headline story) ---------------------------------
print_summary <- function(vintage) {
  for (p in names(paths)) {
    panel <- build_panel(p, vintage)
    cat('\n', paths[[p]], '\n', sep = '')
    for (k in c('pe_postsub_all_in_etr', 'post_sub_per_hh_cost', 'dynamic_revenue_10yr')) {
      j <- match(k, metrics$key); kind <- metrics$kind[j]
      cat(sprintf('  %-27s  %s (published/run0) -> %s (current/run3)   residual %s | policy %s | eta/alpha %s\n',
                  metrics$label[j],
                  fmt_level(panel$levels[k, 'run0'], kind),
                  fmt_level(panel$levels[k, 'run3'], kind),
                  fmt_delta(panel$deltas$residual[k], kind),
                  fmt_delta(panel$deltas$policy[k], kind),
                  fmt_delta(panel$deltas$eta_alpha[k], kind)))
    }
  }
}

# --- main ----------------------------------------------------------------------
cat('=== decomposition bridge — vintage', vintage, '===\n')
cat('Golden transcription check (pinned 06-23 run 3 vs published cells):\n')
check_golden()

if (!dir.exists('reports')) dir.create('reports')
html_path <- file.path('reports', sprintf('tariff_decomposition_bridge_%s.html', vintage))
csv_path  <- file.path('reports', sprintf('tariff_decomposition_bridge_%s.csv', vintage))
writeLines(render_html(vintage), html_path)
write.csv(build_csv(vintage), csv_path, row.names = FALSE)

print_summary(vintage)
cat('\nWrote:\n  ', html_path, '\n  ', csv_path, '\n', sep = '')
