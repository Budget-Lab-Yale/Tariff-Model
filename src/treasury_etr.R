# =============================================================================
# treasury_etr.R - Treasury-derived effective tariff rates (customs duties / imports)
# =============================================================================
# Builds the two "Treasury-Derived Rates" figures for the State of Tariffs tool:
#
#   Annual (1790-latest full year): spliced long-run series
#     1790-1981  HSUS Table Ee424-430, series Ee429 (Irwin): duties calculated as a
#                percent of imports (for consumption from 1822; see SOURCE.md)
#     1982-      calendar-year Treasury customs receipts (net) / BEA NIPA Table 4.1
#                line 19, imports of goods
#     plus the HSUS series over 1982-2000 as an overlap line.
#
#   Monthly (2015-03-latest): MTS Table 4 customs duties (gross or net) / Census
#     imports for consumption, same month.
#
# Inputs are committed snapshots under resources/treasury_etr/ (refreshed by
# build_treasury_etr.R --refresh). Consumed by build_treasury_etr.R.
# =============================================================================

library(dplyr)
library(readr)
library(jsonlite)

SNAPSHOT_DIR <- 'resources/treasury_etr'
HSUS_RAW <- 'resources/historical_etr/raw/Ee424-430.xls'

SPLICE_YEAR <- 1982           # first year on the Treasury/BEA basis
HSUS_LAST_YEAR <- 2000        # last year in HSUS Ee424-430
ANNUAL_STARTS <- c(1790, 1822, 1930, 1982)
MONTHLY_STARTS <- c('2015', '2024', '2025')
MONTHLY_FIRST <- as.Date('2015-03-01')   # fiscaldata MTS Table 4 begins 2015-03

MTS_URL <- 'https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/mts/mts_table_4'
CENSUS_URL <- 'https://api.census.gov/data/timeseries/intltrade/imports/hs'
BEA_URL <- 'https://apps.bea.gov/api/data'

snapshot_path <- function(name) file.path(SNAPSHOT_DIR, name)

require_key <- function(var) {
  key <- Sys.getenv(var)
  if (!nzchar(key)) stop(var, ' is not set (expected in ~/.Renviron).')
  key
}

# Never let a keyed URL reach an error message or log.
fetch_json <- function(url, label) {
  tryCatch(fromJSON(url, simplifyVector = TRUE),
           error = function(e) stop('Request failed: ', label, ' (', conditionMessage(e), ')', call. = FALSE))
}


# --- Pulls (used by --refresh) -----------------------------------------------

pull_mts_customs <- function() {
  fields <- 'record_date,classification_desc,current_month_gross_rcpt_amt,current_month_refund_amt,current_month_net_rcpt_amt'
  pages <- list()
  page <- 1
  repeat {
    url <- sprintf('%s?fields=%s&filter=classification_desc:eq:Customs%%20Duties&sort=record_date&page%%5Bsize%%5D=1000&page%%5Bnumber%%5D=%d',
                   MTS_URL, fields, page)
    res <- fetch_json(url, 'fiscaldata MTS Table 4')
    pages[[page]] <- res$data
    if (page >= res$meta$`total-pages`) break
    page <- page + 1
  }
  mts <- bind_rows(pages) %>%
    transmute(
      month = as.Date(format(as.Date(record_date), '%Y-%m-01')),
      gross_usd_m = as.numeric(current_month_gross_rcpt_amt) / 1e6,
      refund_usd_m = as.numeric(current_month_refund_amt) / 1e6,
      net_usd_m = as.numeric(current_month_net_rcpt_amt) / 1e6
    )
  if (anyDuplicated(mts$month)) stop('MTS: more than one Customs Duties row per month.')
  if (anyNA(mts)) stop('MTS: missing values in customs duties.')
  mts
}

pull_census_imports <- function() {
  key <- require_key('CENSUS_API_KEY')
  url <- sprintf('%s?get=CON_VAL_MO,GEN_VAL_MO&time=from+%s&key=%s',
                 CENSUS_URL, format(MONTHLY_FIRST, '%Y-%m'), key)
  res <- fetch_json(url, 'Census international trade API')
  colnames(res) <- res[1, ]
  as_tibble(res[-1, , drop = FALSE]) %>%
    transmute(
      month = as.Date(paste0(time, '-01')),
      imports_consumption_usd_m = as.numeric(CON_VAL_MO) / 1e6,
      imports_general_usd_m = as.numeric(GEN_VAL_MO) / 1e6
    ) %>%
    arrange(month)
}

pull_bea_goods_imports <- function() {
  key <- require_key('BEA_API_KEY')
  url <- sprintf('%s?UserID=%s&method=GetData&datasetname=NIPA&TableName=T40100&Frequency=A&Year=ALL&ResultFormat=JSON',
                 BEA_URL, key)
  res <- fetch_json(url, 'BEA NIPA Table 4.1')
  rows <- res$BEAAPI$Results$Data
  if (is.null(rows)) stop('BEA: no data returned for NIPA Table 4.1.')
  goods <- rows %>% filter(LineNumber == '19')
  if (!all(trimws(goods$LineDescription) == 'Goods')) {
    stop('BEA: NIPA Table 4.1 line 19 is no longer "Goods" (imports); check the table layout.')
  }
  goods %>%
    transmute(year = as.integer(TimePeriod),
              goods_imports_usd_m = as.numeric(gsub(',', '', DataValue))) %>%
    arrange(year)
}

# Ee429 only: the raw HSUS workbook is licensed for internal use and stays gitignored.
extract_hsus_ee429 <- function() {
  raw <- readxl::read_excel(HSUS_RAW, col_types = 'text')
  names(raw)[1] <- 'year'
  ee429_col <- grep('Ee429', names(raw), value = TRUE)
  if (length(ee429_col) != 1) stop('HSUS: could not find the Ee429 column in ', HSUS_RAW)
  raw %>%
    transmute(year = as.integer(year), ee429_pct = as.numeric(.data[[ee429_col]])) %>%
    filter(!is.na(year))
}

refresh_snapshots <- function() {
  retrieved <- format(Sys.Date())
  write_csv(pull_mts_customs(), snapshot_path('mts_customs_duties_monthly.csv'))
  write_csv(pull_census_imports(), snapshot_path('census_imports_monthly.csv'))
  write_csv(pull_bea_goods_imports(), snapshot_path('bea_nipa_goods_imports_annual.csv'))
  if (file.exists(HSUS_RAW)) {
    write_csv(extract_hsus_ee429(), snapshot_path('hsus_ee429.csv'))
  } else {
    message('  HSUS raw workbook not present; keeping the committed hsus_ee429.csv')
  }
  writeLines(retrieved, snapshot_path('RETRIEVED'))
  message('  Snapshots refreshed (', retrieved, ')')
}


# --- Builders ----------------------------------------------------------------

load_snapshots <- function() {
  read <- function(name) read_csv(snapshot_path(name), show_col_types = FALSE)
  list(
    mts = read('mts_customs_duties_monthly.csv'),
    census = read('census_imports_monthly.csv'),
    bea = read('bea_nipa_goods_imports_annual.csv'),
    hsus = read('hsus_ee429.csv'),
    # Haver FTRU@GOVFIN (net customs receipts) for the years before the MTS API.
    haver = read_csv('resources/calibration/treasury_revenue.csv', show_col_types = FALSE)
  )
}

# Calendar-year net customs receipts ($M): Haver before 2015, MTS from 2015.
# Haver and MTS agree to the dollar over their overlap, so the join is seamless.
annual_customs_net <- function(haver, mts) {
  mts_years <- mts %>%
    mutate(year = as.integer(format(month, '%Y'))) %>%
    group_by(year) %>%
    summarise(n = n(), customs_usd_m = sum(net_usd_m), .groups = 'drop')
  haver_years <- haver %>%
    group_by(year) %>%
    summarise(n = n(), customs_usd_m = sum(customs_duties), .groups = 'drop')
  bind_rows(
    haver_years %>% filter(year >= SPLICE_YEAR, year < 2015),
    mts_years %>% filter(year >= 2015) %>%
      # 2015 Jan-Feb predate the MTS API; take them from Haver.
      left_join(haver %>% filter(year == 2015, month <= 2) %>%
                  summarise(year = 2015L, pre_api = sum(customs_duties), n_pre = n()),
                by = 'year') %>%
      mutate(customs_usd_m = customs_usd_m + coalesce(pre_api, 0),
             n = n + coalesce(n_pre, 0L)) %>%
      select(year, n, customs_usd_m)
  ) %>%
    filter(n == 12) %>%
    select(year, customs_usd_m)
}

build_annual <- function(s) {
  treasury <- annual_customs_net(s$haver, s$mts) %>%
    inner_join(s$bea, by = 'year') %>%
    transmute(year, value = 100 * customs_usd_m / goods_imports_usd_m)
  if (min(treasury$year) != SPLICE_YEAR) stop('Annual: Treasury/BEA series does not start in ', SPLICE_YEAR)
  if (any(diff(treasury$year) != 1)) stop('Annual: gap in the Treasury/BEA years.')

  hsus <- s$hsus %>% filter(!is.na(ee429_pct))
  if (!all(1790:HSUS_LAST_YEAR %in% hsus$year)) stop('Annual: HSUS Ee429 is missing years.')

  series <- bind_rows(
    hsus %>% filter(year < SPLICE_YEAR) %>% transmute(year, series = 'effective_rate', value = ee429_pct),
    treasury %>% mutate(series = 'effective_rate'),
    hsus %>% filter(year >= SPLICE_YEAR) %>% transmute(year, series = 'hsus_overlap', value = ee429_pct)
  )

  bind_rows(lapply(ANNUAL_STARTS, function(st) {
    series %>% filter(year >= st) %>% mutate(annual_start = as.character(st))
  })) %>%
    arrange(annual_start, series, year) %>%
    transmute(time = year, series, value, annual_start)
}

build_monthly <- function(s) {
  joined <- s$mts %>%
    inner_join(s$census, by = 'month') %>%
    filter(month >= MONTHLY_FIRST)
  if (nrow(joined) == 0) stop('Monthly: no overlapping MTS/Census months.')
  long <- bind_rows(
    joined %>% transmute(time = month, measure = 'gross', value = 100 * gross_usd_m / imports_consumption_usd_m),
    joined %>% transmute(time = month, measure = 'net', value = 100 * net_usd_m / imports_consumption_usd_m)
  ) %>%
    mutate(series = 'treasury_etr')

  bind_rows(lapply(MONTHLY_STARTS, function(st) {
    long %>% filter(time >= as.Date(paste0(st, '-01-01'))) %>% mutate(monthly_start = st)
  })) %>%
    transmute(time = format(time, '%Y-%m-%d'), series, value, measure, monthly_start) %>%
    arrange(monthly_start, measure, time)
}
