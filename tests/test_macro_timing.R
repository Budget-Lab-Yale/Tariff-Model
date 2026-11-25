# Run from the repository root with Rscript tests/test_macro_timing.R.
suppressPackageStartupMessages(library(tidyverse))
source('src/helpers.R')
source('src/04_calculate_revenue.R')
source('src/05a_usmm_surrogate.R')
source('src/05_calculate_macro.R')
source('src/07_calculate_dynamic_revenue.R')
source('src/13_export_dashboard.R')

same <- function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-10, check.attributes = FALSE))
irfs <- load_usmm_irfs()
assumptions <- yaml::read_yaml('config/global_assumptions.yaml')
dates <- as.Date(c('2025-04-01', '2026-01-01', '2027-04-01'))
policy <- tibble(date = dates, etr_increase = c(.05, .07, .07))
proposal <- mutate(policy, etr_increase = c(.05, .07, .11))
before <- with(irfs$baseline, year * 4 + quarter < 2027 * 4 + 2)
cbo <- read_csv('resources/baselines/cbo.csv', show_col_types = FALSE)

run_case <- function(schedule, gdp, imports, postsub) {
  inputs <- list(usmm_irfs = irfs, model_params = list(refund_2026 = 166),
                 baselines = list(cbo = cbo), assumptions = assumptions,
                 qgdp = c(usa = gdp), qmwreg = imports, etr_increase = postsub)
  etr <- list(presub_etr_increase_by_date = schedule, short_run_etr_by_date = schedule,
              etr_increase_by_date = mutate(schedule, etr_increase = etr_increase * postsub / .1))
  usmm <- run_usmm_surrogate(etr, inputs)
  inputs$macro_quarterly <- usmm$macro_quarterly
  macro <- calculate_macro(inputs)$quarterly
  revenue <- calculate_revenue(inputs, etr)
  dynamic <- calculate_dynamic_revenue(inputs, revenue)
  list(macro = macro, raw = usmm$macro_quarterly, revenue = revenue$by_year,
       dynamic = dynamic$by_year, chart = build_real_gdp_path(list(macro_quarterly = macro)))
}
ref <- suppressMessages(run_case(policy, -.5, -5, .08))
alt <- suppressMessages(run_case(proposal, -1.5, -12, .06))
stopifnot(identical(ref$macro[before, ], alt$macro[before, ]),
          same(ref$macro$gdp_tariff[before], ref$raw$gdp_tariff[before]),
          identical(filter(ref$revenue, fiscal_year < 2027), filter(alt$revenue, fiscal_year < 2027)),
          identical(filter(ref$dynamic, fiscal_year < 2027), filter(alt$dynamic, fiscal_year < 2027)))
start <- which(!before)[1]
for (col in c('gdp_tariff', 'urate_tariff', 'employment_tariff')) {
  stopifnot(alt$macro[[col]][start] != ref$macro[[col]][start])
}
stopifnot(with(alt$revenue, net_revenue[fiscal_year == 2027]) !=
          with(ref$revenue, net_revenue[fiscal_year == 2027]))

# Exact endpoints, midpoint and a different configuration.
w <- long_run_weight(irfs$baseline$year, irfs$baseline$quarter)
stopifnot(all(w[before] == 0), w[start] == 0, w[start + 6] == .5,
          all(tail(w, length(w) - start - 11) == 1),
          same(long_run_weight(2025, 1:4, list(start_year = 2025, start_quarter = 2, quarters = 2)),
               c(0, 0, .5, 1)))
dev <- with(alt$macro, (gdp_tariff / gdp_baseline - 1) * 100)
raw <- with(alt$raw, (gdp_tariff / gdp_baseline - 1) * 100)
stopifnot(same(dev, (1-w)*raw + w*(-1.5)), same(alt$chart$value, dev[alt$macro$year >= 2025]))
# Only Jul-Sep has positive GTAP weight in FY2027. FY2031 is fully phased in.
stopifnot(same(alt$revenue$phase_in[alt$revenue$fiscal_year == 2027], (92/365)/12),
          all(alt$revenue$phase_in[alt$revenue$fiscal_year >= 2031] == 1),
          same(revenue_rate_on(as.Date('2025-03-31'), policy, NULL), 0))

# A single 2025Q2 change still reproduces the original response, including refunds.
one <- construct_usmm_response(decompose_shocks(policy[1, ], refund_2026 = 84), irfs)
for (var in USMM_VARS) {
  stopifnot(same(one[[var]], irfs$perm[['5']][[var]] + .5 * irfs$refund[[var]]))
}
static <- construct_usmm_response(decompose_shocks(etr_increase = .05, refund_2026 = 84), irfs)
stopifnot(identical(one, static))

# An explicitly temporary 5-point tariff starts in Q1. A revision inside the
# announced window has no effect before the revision, including in the all-permanent case.
window <- c('2027-01-01', '2027-07-01')
tmp <- tibble(date = as.Date(c(window[1], '2027-04-01', window[2])), etr_increase = c(.05, .05, 0))
revised <- mutate(tmp, etr_increase = c(.05, .12, .03))
for (use_temp in c(TRUE, FALSE)) {
  a <- construct_usmm_response(decompose_shocks(tmp, temp_component_dates = window), irfs, use_temp)
  b <- construct_usmm_response(decompose_shocks(revised, temp_component_dates = window), irfs, use_temp)
  stopifnot(identical(a[before, ], b[before, ]), a$gdpr[start] != b$gdpr[start])
  if (use_temp) stopifnot(same(a$gdpr, shift_irf(irfs$temp[['5']]$gdpr,
                                      date_to_quarter_index(tmp$date[1]) - USMM_TEMP_SHOCK_QUARTER)))
}
# Below/above the calibrated sizes and a negative change each use their own bracket.
mixed <- tibble(date = dates, etr_increase = c(.02, .14, .11))
response <- construct_usmm_response(decompose_shocks(mixed), irfs)
expected <- irfs$perm[['2']]$gdpr +
  shift_irf(interpolate_irf(irfs$perm, 12)$gdpr, date_to_quarter_index(dates[2]) - 6L) +
  shift_irf(interpolate_irf(irfs$perm, -3)$gdpr, date_to_quarter_index(dates[3]) - 6L)
stopifnot(same(response$gdpr, expected))
message('PASS: policy dates, single-step equivalence, temporary changes, blend, dashboard and fiscal years')
