# =============================================================================
# extract_usmm_irfs.R - One-time extraction of USMM IRFs from Excel
# =============================================================================
#
# Extracts impulse response functions (diffs from baseline) from the USMM
# output workbook into CSV files for use by the tariff model.
#
# Source: TARIFF IRFS OUTPUT 20260302.xlsx
#
# Usage:
#   Rscript src/util/extract_usmm_irfs.R
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
})

EXCEL_PATH <- file.path(
  Sys.getenv('USERPROFILE'),
  'Downloads',
  '2026-03-04 12_30 tariffs',
  'TARIFF IRFS OUTPUT 20260302.xlsx'
)

OUTPUT_DIR <- 'resources/usmm'

# Row mapping for diffs-from-baseline section (rows 32-49)
# Row numbers are 1-indexed in the Excel sheet
DIFF_ROWS <- list(
  gdpr = 34,    # Real GDP (% diff)
  jpc  = 36,    # PCE price index (% diff)
  ruc  = 38,    # Unemployment rate (%pt diff)
  ehhc = 39,    # Nonfarm payroll employment (% diff)
  rmff = 49     # Federal funds rate (%pt diff)
)

# Baseline levels section (rows 7-20 in baseforce)
LEVEL_ROWS <- list(
  gdpr = 7,     # Real GDP (bil chained 2017 $)
  jpc  = 10,    # PCE price index (2017=100)
  ruc  = 9,     # Unemployment rate (%)
  ehhc = 11,    # Nonfarm payroll employment (mil)
  rmff = 20     # Federal funds rate (%)
)

# Sheets to extract diffs from
DIFF_SHEETS <- c(
  'perm2_fed', 'perm5_fed', 'perm10_fed',
  'temp2_fed', 'temp5_fed', 'temp10_fed',
  'refund_FED'
)

# Output filenames (without .csv)
SHEET_TO_FILE <- c(
  'perm2_fed'  = 'perm2_fed',
  'perm5_fed'  = 'perm5_fed',
  'perm10_fed' = 'perm10_fed',
  'temp2_fed'  = 'temp2_fed',
  'temp5_fed'  = 'temp5_fed',
  'temp10_fed' = 'temp10_fed',
  'refund_FED' = 'refund_fed'
)

# Data columns start at column 5 (2024Q1) through column 52 (2035Q4)
DATA_COL_START <- 5
DATA_COL_END <- 52
N_QUARTERS <- DATA_COL_END - DATA_COL_START + 1  # 48 quarters

#' Build year/quarter sequence from Excel header
#' @param ws Worksheet object
#' @return Tibble with year and quarter columns
build_time_index <- function(ws) {
  years <- sapply(DATA_COL_START:DATA_COL_END, function(col) {
    ws[[1]][[col - 1]]  # Row 2 has years (0-indexed in list)
  })
  # Parse quarter labels like "2024Q1" -> extract quarter number
  q_labels <- sapply(DATA_COL_START:DATA_COL_END, function(col) {
    ws[[2]][[col - 1]]
  })

  tibble(
    year = as.integer(years),
    quarter = as.integer(str_extract(q_labels, '\\d$'))
  )
}


#' Extract a row of data from a worksheet
#' @param ws Worksheet object (read as list via read.xlsx)
#' @param wb Workbook object
#' @param sheet_name Sheet name
#' @param row_num Row number (1-indexed)
#' @return Numeric vector of values
extract_row <- function(wb, sheet_name, row_num) {
  # Read entire sheet as data frame, then extract the row
  # openxlsx read.xlsx is 1-indexed, rows start at 1
  vals <- read.xlsx(
    wb,
    sheet = sheet_name,
    rows = row_num,
    cols = DATA_COL_START:DATA_COL_END,
    colNames = FALSE
  )
  as.numeric(vals[1, ])
}


#' Extract diffs-from-baseline for one sheet
#' @param wb Workbook object
#' @param sheet_name Sheet name
#' @return Tibble with year, quarter, and variable columns
extract_diffs <- function(wb, sheet_name) {
  message(sprintf('  Extracting diffs from %s...', sheet_name))

  # Get time index from row 2-3
  time_idx <- read.xlsx(
    wb,
    sheet = sheet_name,
    rows = 2:3,
    cols = DATA_COL_START:DATA_COL_END,
    colNames = FALSE
  )

  years <- as.integer(time_idx[1, ])
  q_labels <- as.character(time_idx[2, ])
  quarters <- as.integer(str_extract(q_labels, '\\d$'))

  result <- tibble(year = years, quarter = quarters)

  for (var_name in names(DIFF_ROWS)) {
    row_num <- DIFF_ROWS[[var_name]]
    vals <- extract_row(wb, sheet_name, row_num)
    result[[var_name]] <- vals
  }

  return(result)
}


#' Extract baseline levels from baseforce sheet
#' @param wb Workbook object
#' @return Tibble with year, quarter, and variable columns
extract_baseline <- function(wb) {
  sheet_name <- 'baseforce'
  message(sprintf('  Extracting baseline levels from %s...', sheet_name))

  # Get time index
  time_idx <- read.xlsx(
    wb,
    sheet = sheet_name,
    rows = 2:3,
    cols = DATA_COL_START:DATA_COL_END,
    colNames = FALSE
  )

  years <- as.integer(time_idx[1, ])
  q_labels <- as.character(time_idx[2, ])
  quarters <- as.integer(str_extract(q_labels, '\\d$'))

  result <- tibble(year = years, quarter = quarters)

  for (var_name in names(LEVEL_ROWS)) {
    row_num <- LEVEL_ROWS[[var_name]]
    vals <- extract_row(wb, sheet_name, row_num)
    result[[var_name]] <- vals
  }

  return(result)
}


# =============================================================================
# Main extraction
# =============================================================================

main <- function() {
  if (!file.exists(EXCEL_PATH)) {
    stop('Excel file not found: ', EXCEL_PATH)
  }

  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  message('Loading workbook...')
  wb <- loadWorkbook(EXCEL_PATH)

  # Extract diffs from each IRF sheet
  for (sheet_name in DIFF_SHEETS) {
    diffs <- extract_diffs(wb, sheet_name)
    out_file <- file.path(OUTPUT_DIR, paste0(SHEET_TO_FILE[sheet_name], '.csv'))
    write_csv(diffs, out_file)
    message(sprintf('    Wrote %s (%d quarters)', out_file, nrow(diffs)))
  }

  # Extract baseline levels
  baseline <- extract_baseline(wb)
  out_file <- file.path(OUTPUT_DIR, 'baseline.csv')
  write_csv(baseline, out_file)
  message(sprintf('    Wrote %s (%d quarters)', out_file, nrow(baseline)))

  message('\nDone! All IRF CSVs written to ', OUTPUT_DIR)
}

main()
