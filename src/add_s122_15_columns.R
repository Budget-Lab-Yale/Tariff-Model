# =============================================================================
# add_s122_15_columns.R - Add 15% Section 122 columns to data_download.xlsx
# =============================================================================
#
# Adds two new columns (F and G) to the T1 sheet of the existing combined
# data_download.xlsx for the 15% Section 122 scenarios.
#
# Usage:
#   Rscript src/add_s122_15_columns.R
#
# =============================================================================

source('src/12_export_excel.R')

INPUT_PATH <- 'C:/Users/jar335/Downloads/OneDrive_2026-03-08/2026 03 March 09 State of Tariffs/data_download.xlsx'
OUTPUT_PATH <- INPUT_PATH  # Overwrite in place

suppress_openxlsx_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl('externalLink', msg, fixed = TRUE) ||
          grepl('one argument not used by format', msg, fixed = TRUE)) {
        invokeRestart('muffleWarning')
      }
    }
  )
}

write_block <- function(wb, sheet, data, start_row, start_col) {
  if (is.null(dim(data))) {
    data <- as.data.frame(data)
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(invisible(NULL))
  }

  deleteData(
    wb,
    sheet = sheet,
    cols = start_col:(start_col + ncol(data) - 1),
    rows = start_row:(start_row + nrow(data) - 1),
    gridExpand = TRUE
  )

  writeData(
    wb,
    sheet = sheet,
    x = data,
    startCol = start_col,
    startRow = start_row,
    colNames = FALSE
  )
}


# =============================================================================
# Load workbook and scenario outputs
# =============================================================================

message('Loading workbook...')
wb <- suppress_openxlsx_warnings(loadWorkbook(INPUT_PATH))

message('Loading 15% Section 122 scenario outputs...')
out_temp_15 <- load_model_outputs('2-21_temp_15')
out_perm_15 <- load_model_outputs('2-21_perm_15')


# =============================================================================
# T1: Add columns F(6) and G(7) — matching existing pattern in cols B-E
# =============================================================================

message('  Adding T1 columns...')

# Column headers (rows 5-6) — match existing pattern
# Row 5: "15% Section 122" spanning F-G
# Row 6: "Expires in 150 Days" (F) and "Extended After 150 Days" (G)
writeData(wb, 'T1', '15% Section 122', startCol = 6, startRow = 5, colNames = FALSE)
writeData(wb, 'T1', 'Expires in 150 Days', startCol = 6, startRow = 6, colNames = FALSE)
writeData(wb, 'T1', 'Extended After 150 Days', startCol = 7, startRow = 6, colNames = FALSE)

# Build T1 data for both scenarios
t1_temp_15 <- build_t1(out_temp_15)
t1_perm_15 <- build_t1(out_perm_15)

# Write data columns (rows 7-22, same as existing cols)
write_block(wb, 'T1', t1_temp_15, start_row = 7, start_col = 6)
write_block(wb, 'T1', t1_perm_15, start_row = 7, start_col = 7)

# Apply number formatting to new columns (match existing cols 2-5)
t1_pct_style <- createStyle(numFmt = '0.0%')
t1_pct2_style <- createStyle(numFmt = '0.00%')
t1_currency1_style <- createStyle(numFmt = '"$"#,##0.0')
t1_currency0_style <- createStyle(numFmt = '"$"#,##0')
t1_decimal1_style <- createStyle(numFmt = '0.0')

for (col in 6:7) {
  addStyle(wb, 'T1', t1_pct_style, rows = c(8, 9), cols = col, stack = TRUE)
  addStyle(wb, 'T1', t1_currency1_style, rows = c(11, 12), cols = col, stack = TRUE)
  addStyle(wb, 'T1', t1_pct_style, rows = c(14, 15), cols = col, stack = TRUE)
  addStyle(wb, 'T1', t1_currency0_style, rows = c(16, 17), cols = col, stack = TRUE)
  addStyle(wb, 'T1', t1_decimal1_style, rows = c(19, 20), cols = col, stack = TRUE)
  addStyle(wb, 'T1', t1_pct2_style, rows = 21, cols = col, stack = TRUE)
  addStyle(wb, 'T1', t1_decimal1_style, rows = 22, cols = col, stack = TRUE)
}

# White background for new columns
white_style <- createStyle(fgFill = 'white')
addStyle(wb, 'T1', white_style, rows = 1:500, cols = 6:7, gridExpand = TRUE, stack = TRUE)


# =============================================================================
# Save
# =============================================================================

message('Saving...')
suppress_openxlsx_warnings(saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE))
message(sprintf('Done! Saved to: %s', OUTPUT_PATH))
