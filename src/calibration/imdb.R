# =============================================================================
# imdb.R
# Census IMDB (merchandise import) bulk download / parse / aggregate helpers.
#
# FAITHFUL PORT of tariff-etr-adj/code/imdb.R. Preserved verbatim on the load-
# bearing gotchas (fixed-width positions, latin1/lazy=FALSE, NO readBin byte-
# sanitize) so the migrated panel reproduces the golden eta/alpha numbers. The
# only changes from upstream are the TM cache layout (output/calibration/_imdb/)
# and dropping the tariff-etr-eval ZIP-cache fallback (no eval repo in TM).
#
# Sourcing this file has no side effects beyond creating the ZIP cache dir.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringi)
})

# Shared (scenario-independent) IMDB cache: the ZIPs are immutable Census bulk
# files, so one cache serves every scenario.
IMDB_CACHE_DIR <- imdb_cache_dir()
IMDB_RAW       <- file.path(IMDB_CACHE_DIR, 'zips')
dir.create(IMDB_RAW, showWarnings = FALSE, recursive = TRUE)

IMDB_URL_TEMPLATE <- 'https://www.census.gov/trade/downloads/%s/Merch/im_m/IMDB%s.ZIP'

# Fixed-width spec for IMP_DETL.TXT (positions from tariff-etr-eval; only the
# fields the calibration needs -- preference/district codes dropped)
imdb_fwf <- fwf_positions(
  start     = c( 1, 11, 23, 27,  74, 104),
  end       = c(10, 14, 26, 28,  88, 118),
  col_names = c('commodity', 'cty_code', 'year', 'month',
                'con_val_mo', 'cal_dut_mo')
)

#' Locate one IMDB ZIP: local cache -> census.gov download.
get_imdb_zip <- function(year_month) {
  yymm     <- paste0(substr(year_month, 3, 4), substr(year_month, 6, 7))
  zip_name <- paste0('IMDB', yymm, '.ZIP')
  zip_path <- file.path(IMDB_RAW, zip_name)
  if (file.exists(zip_path) && file.size(zip_path) > 1000) return(zip_path)

  url <- sprintf(IMDB_URL_TEMPLATE, substr(year_month, 1, 4), yymm)
  for (attempt in 1:3) {
    ok <- tryCatch({
      download.file(url, zip_path, mode = 'wb', quiet = TRUE)
      file.exists(zip_path) && file.size(zip_path) > 1000
    }, error = function(e) FALSE)
    if (ok) { msg('    %s: downloaded', year_month); return(zip_path) }
    Sys.sleep(2 * attempt)
  }
  if (file.exists(zip_path)) file.remove(zip_path)
  NULL
}

#' Parse one IMDB ZIP to HS10 x country rows (entry level).
parse_imdb <- function(zip_path) {
  detl <- grep('IMP_DETL\\.TXT$', unzip(zip_path, list = TRUE)$Name,
               value = TRUE, ignore.case = TRUE)
  if (length(detl) == 0) return(NULL)

  tmp_dir <- tempfile('imdb_'); dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  unzip(zip_path, files = detl[1], exdir = tmp_dir)
  detl_path <- file.path(tmp_dir, detl[1])

  # latin1 tolerates any byte (the fields we keep are pure ASCII), avoiding
  # the 16GB-peak readBin byte-sanitize the eval repo used. lazy = FALSE
  # materializes before the temp file is deleted on exit.
  read_fwf(detl_path, col_positions = imdb_fwf,
           col_types = cols(.default = col_character()),
           locale = locale(encoding = 'latin1'),
           lazy = FALSE, progress = FALSE) %>%
    mutate(hs10       = stri_pad_left(trimws(commodity), 10, '0'),
           cty_code   = trimws(cty_code),
           year       = as.integer(trimws(year)),
           month      = as.integer(trimws(month)),
           con_val_mo = as.numeric(trimws(con_val_mo)),
           cal_dut_mo = as.numeric(trimws(cal_dut_mo))) %>%
    filter(grepl('^[0-9]+$', hs10), !is.na(year),
           coalesce(con_val_mo, 0) != 0) %>%
    mutate(year_month = sprintf('%04d-%02d', year, month)) %>%
    select(hs10, cty_code, year_month, con_val_mo, cal_dut_mo)
}

#' Download + parse + aggregate a set of months to HS10 x country x month.
build_imdb_agg <- function(yms) {
  chunks <- vector('list', length(yms)); names(chunks) <- yms
  for (ym in yms) {
    zip_path <- get_imdb_zip(ym)
    if (is.null(zip_path)) { msg('    %s: NOT AVAILABLE', ym); next }
    d <- tryCatch(parse_imdb(zip_path), error = function(e) {
      msg('    %s: ERROR %s', ym, conditionMessage(e)); NULL
    })
    if (is.null(d) || nrow(d) == 0) next
    chunks[[ym]] <- d %>%
      summarise(con_val_mo = sum(con_val_mo, na.rm = TRUE),
                cal_dut_mo = sum(cal_dut_mo, na.rm = TRUE),
                .by = c(hs10, cty_code, year_month))
    msg('    %s: %s cells, $%.0fB', ym,
        format(nrow(chunks[[ym]]), big.mark = ','),
        sum(chunks[[ym]]$con_val_mo) / 1e9)
    rm(d); gc(verbose = FALSE)
  }
  bind_rows(chunks)
}
