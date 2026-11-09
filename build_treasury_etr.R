#!/usr/bin/env Rscript
# =============================================================================
# build_treasury_etr.R - Build the Treasury-derived effective tariff rate figures
# =============================================================================
# Thin CLI over src/treasury_etr.R. Writes two long-format data.csv files for the
# State of Tariffs tool's "Treasury-Derived Rates" section:
#   <out-dir>/treasury-etr-annual/data.csv    time (year), series, value, annual_start
#   <out-dir>/treasury-etr-monthly/data.csv   time, series, value, measure, monthly_start
# and checks the 1982-2024 annual values against the series published through
# April 2026.
#
# Usage:
#   Rscript build_treasury_etr.R [--refresh] [--out-dir <dir>]
#
#   --refresh        re-pull MTS, Census and BEA (and re-extract HSUS Ee429 if the
#                    raw workbook is present) into resources/treasury_etr/
#   --out-dir <dir>  output directory (default: output/treasury_etr)
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

refresh <- FALSE
out_dir <- 'output/treasury_etr'

i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a == '--refresh') {
    refresh <- TRUE; i <- i + 1
  } else if (a == '--out-dir' && i < length(args)) {
    out_dir <- args[i + 1]; i <- i + 2
  } else {
    stop('Unknown or malformed argument: ', a,
         '\n  Usage: Rscript build_treasury_etr.R [--refresh] [--out-dir <dir>]')
  }
}

suppressPackageStartupMessages(source('src/treasury_etr.R'))

# Published 1982-2024 values (old src/12_export_excel.R HISTORICAL_ETR, through
# the April 2026 report). The rebuild should reproduce them up to BEA revisions.
PUBLISHED_CHECK_TOL <- 0.05   # percentage points

if (refresh) refresh_snapshots()

snapshots <- load_snapshots()
annual <- build_annual(snapshots)
monthly <- build_monthly(snapshots)

write_figure <- function(df, fig_id) {
  dir.create(file.path(out_dir, fig_id), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, file.path(out_dir, fig_id, 'data.csv'))
  message('  wrote ', file.path(out_dir, fig_id, 'data.csv'), ' (', nrow(df), ' rows)')
}
write_figure(annual, 'treasury-etr-annual')
write_figure(monthly, 'treasury-etr-monthly')

published <- read_csv(snapshot_path('published_etr_1982_2024.csv'), show_col_types = FALSE)
check <- annual %>%
  filter(annual_start == '1982', series == 'effective_rate') %>%
  rename(year = time) %>%
  inner_join(published, by = 'year') %>%
  mutate(diff = value - published_pct)
message(sprintf('  1982-2024 vs published: max |diff| %.3f pp; %d of %d years within %.2f pp',
                max(abs(check$diff)), sum(abs(check$diff) <= PUBLISHED_CHECK_TOL), nrow(check),
                PUBLISHED_CHECK_TOL))
off <- check %>% filter(abs(diff) > PUBLISHED_CHECK_TOL)
if (nrow(off) > 0) print(as.data.frame(off), row.names = FALSE)

last_annual <- max(annual$time)
last_month <- max(monthly$time)
message('  annual through ', last_annual, '; monthly through ', last_month)
