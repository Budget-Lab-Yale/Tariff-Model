# =============================================================================
# update_state_of_tariffs.R - Update State of Tariffs Excel data download
# =============================================================================
#
# Updates the data_download.xlsx file with fresh model results from all
# 4 scenarios (2-19, 2-20, 2-21_perm, 2-21_temp) for the March 9, 2026 report.
#
# Usage:
#   source('src/update_state_of_tariffs.R')
#
# =============================================================================

source('src/12_export_excel.R')

TEMPLATE_PATH <- 'C:/Users/jar335/Downloads/OneDrive_2026-03-08/2026 02 February 27 State of Tariffs/data_download.xlsx'
OUTPUT_PATH <- 'C:/Users/jar335/Downloads/OneDrive_2026-03-08/2026 03 March 09 State of Tariffs/data_download.xlsx'
REPORT_DATE <- 'March 9, 2026'


# =============================================================================
# Load template and all scenario outputs
# =============================================================================

message('Loading template workbook...')

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

wb <- suppress_openxlsx_warnings(loadWorkbook(TEMPLATE_PATH))

message('Loading model outputs...')
out_219 <- load_model_outputs('2-19')
out_220 <- load_model_outputs('2-20')
out_temp <- load_model_outputs('2-21_temp')
out_perm <- load_model_outputs('2-21_perm')


# =============================================================================
# Helper: write a block of data into the workbook
# =============================================================================

write_block <- function(sheet, data, start_row, start_col) {
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
# Data TOC (Sheet 1)
# =============================================================================

message('  Updating Data TOC...')
report_date_obj <- as.Date(REPORT_DATE, format = '%B %d, %Y')
date_md <- format(report_date_obj, '%B %d')
date_my <- format(report_date_obj, '%B %Y')

writeData(wb, 'Data TOC', sprintf('State of U.S. Tariffs: %s', REPORT_DATE),
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, 'Data TOC', date_my,
          startCol = 1, startRow = 2, colNames = FALSE)

# TOC rows 6-14 are hyperlinks with generic titles (no dates) — leave them as-is
# from the template to preserve cached display values


# =============================================================================
# T1: Summary Table (4 columns: 2-19, 2-20, 2-21_temp, 2-21_perm)
# =============================================================================

message('  Updating T1 (Summary)...')

t1_219 <- build_t1(out_219)
t1_220 <- build_t1(out_220)
t1_temp <- build_t1(out_temp)
t1_perm <- build_t1(out_perm)

# Write each scenario as a column (B=2, C=3, D=4, E=5), starting at row 7
write_block('T1', t1_219, start_row = 7, start_col = 2)
write_block('T1', t1_220, start_row = 7, start_col = 3)
write_block('T1', t1_temp, start_row = 7, start_col = 4)
write_block('T1', t1_perm, start_row = 7, start_col = 5)

# Number formatting for T1 (all 4 data columns)
t1_pct_style <- createStyle(numFmt = '0.0%')
t1_pct2_style <- createStyle(numFmt = '0.00%')
t1_currency1_style <- createStyle(numFmt = '"$"#,##0.0')
t1_currency0_style <- createStyle(numFmt = '"$"#,##0')
t1_decimal1_style <- createStyle(numFmt = '0.0')

for (col in 2:5) {
  # ETR levels (rows 8-9): percent format
  addStyle(wb, 'T1', t1_pct_style, rows = c(8, 9), cols = col, stack = TRUE)
  # Revenue (rows 11-12): dollar trillions
  addStyle(wb, 'T1', t1_currency1_style, rows = c(11, 12), cols = col, stack = TRUE)
  # PCE price change (rows 14-15): percent format
  addStyle(wb, 'T1', t1_pct_style, rows = c(14, 15), cols = col, stack = TRUE)
  # HH cost (rows 16-17): dollar format
  addStyle(wb, 'T1', t1_currency0_style, rows = c(16, 17), cols = col, stack = TRUE)
  # GDP growth (rows 19-20): decimal
  addStyle(wb, 'T1', t1_decimal1_style, rows = c(19, 20), cols = col, stack = TRUE)
  # Long-run GDP (row 21): percent format (small, use 0.00%)
  addStyle(wb, 'T1', t1_pct2_style, rows = 21, cols = col, stack = TRUE)
  # Unemployment (row 22): decimal
  addStyle(wb, 'T1', t1_decimal1_style, rows = 22, cols = col, stack = TRUE)
}

# Update row labels in column A for the new layout (now has 2025 and 2026 GDP rows)
writeData(wb, 'T1', 'Effective Tariff Rates at the End of 2026',
          startCol = 1, startRow = 7, colNames = FALSE)
writeData(wb, 'T1', 'Overall, Pre-Substitution',
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, 'T1', 'Overall, Post-Substitution',
          startCol = 1, startRow = 9, colNames = FALSE)
writeData(wb, 'T1', 'Fiscal',
          startCol = 1, startRow = 10, colNames = FALSE)
writeData(wb, 'T1', 'Conventional Revenue, 2026-2035 (Trillions)',
          startCol = 1, startRow = 11, colNames = FALSE)
writeData(wb, 'T1', 'Dynamic Revenue, 2026-2035 (Trillions)',
          startCol = 1, startRow = 12, colNames = FALSE)
writeData(wb, 'T1', 'Prices in the Medium Run',
          startCol = 1, startRow = 13, colNames = FALSE)
writeData(wb, 'T1', 'Percent Change in PCE Price Level, pre-substitution',
          startCol = 1, startRow = 14, colNames = FALSE)
writeData(wb, 'T1', 'Percent Change in PCE Price Level, post-substitution',
          startCol = 1, startRow = 15, colNames = FALSE)
writeData(wb, 'T1', 'Average Household Real Income Loss, Pre-Substitution (2025$)',
          startCol = 1, startRow = 16, colNames = FALSE)
writeData(wb, 'T1', 'Average Household Real Income Loss, Post-Substitution (2025$)',
          startCol = 1, startRow = 17, colNames = FALSE)
writeData(wb, 'T1', 'Output and Employment',
          startCol = 1, startRow = 18, colNames = FALSE)
writeData(wb, 'T1', 'Percentage Point Change in Q4-Q4 GDP Growth, 2025',
          startCol = 1, startRow = 19, colNames = FALSE)
writeData(wb, 'T1', 'Percentage Point Change in Q4-Q4 GDP Growth, 2026',
          startCol = 1, startRow = 20, colNames = FALSE)
writeData(wb, 'T1', 'Percent change in long-run GDP',
          startCol = 1, startRow = 21, colNames = FALSE)
writeData(wb, 'T1', 'Percentage Point Change in the Unemployment Rate, End of 2026',
          startCol = 1, startRow = 22, colNames = FALSE)


# =============================================================================
# T2: ETR by Region (2 sections: temp rows 8-12, perm rows 17-21)
# =============================================================================

message('  Updating T2 (ETR by Region)...')

t2_temp <- build_t2(out_temp)
t2_perm <- build_t2(out_perm)

# Section 1: "Section 122 Expires" = 2-21_temp (rows 8-12, cols A-G)
write_block('T2', t2_temp, start_row = 8, start_col = 1)

# Section 2: "Section 122 Extended" = 2-21_perm (rows 17-21, cols A-G)
write_block('T2', t2_perm, start_row = 17, start_col = 1)


# =============================================================================
# F1: Historical ETR (columns D and F = current post-sub and pre-sub rates)
# =============================================================================

message('  Updating F1 (Historical ETR)...')

f1 <- build_f1(out_temp)  # Use 2-21_temp for current rates
write_block('F1', f1, start_row = 6, start_col = 1)


# =============================================================================
# F2: GDP Level Effects (2 data columns: temp=B, perm=C)
# =============================================================================

message('  Updating F2 (GDP Effects)...')

f2_temp <- build_f2(out_temp)
f2_perm <- build_f2(out_perm)

# Write dates (column A) and temp data (column B) from f2_temp
write_block('F2', f2_temp, start_row = 6, start_col = 1)

# Overwrite column C with perm data
write_block('F2', f2_perm %>% select(`All 2025 Tariffs to Date`),
            start_row = 6, start_col = 3)


# =============================================================================
# F3: Sector GDP by Industry (2 data columns: temp=B, perm=C)
# =============================================================================

message('  Updating F3 (Sectors)...')

f3_temp <- build_f3(out_temp)
f3_perm <- build_f3(out_perm)

# Write sector names + temp data
write_block('F3', f3_temp, start_row = 7, start_col = 1)

# Overwrite column C with perm data
write_block('F3', f3_perm %>% select(`All 2025 Tariffs to Date`),
            start_row = 7, start_col = 3)


# =============================================================================
# F4: International GDP (2 data columns: temp=B, perm=C)
# =============================================================================

message('  Updating F4 (International GDP)...')

f4_temp <- build_f4(out_temp)
f4_perm <- build_f4(out_perm)

# Write region names + temp data
write_block('F4', f4_temp, start_row = 7, start_col = 1)

# Overwrite column C with perm data
write_block('F4', f4_perm %>% select(`All 2025 Tariffs to Date`),
            start_row = 7, start_col = 3)


# =============================================================================
# T3: Revenue (2 sections: temp rows 7-9, perm rows 13-15)
# =============================================================================

message('  Updating T3 (Revenue)...')

t3_temp <- build_t3(out_temp)
t3_perm <- build_t3(out_perm)

# Section 1: "Section 122 Expires" = temp (rows 7-9, cols A-L)
write_block('T3', t3_temp, start_row = 7, start_col = 1)

# Section 2: "Section 122 Extended" = perm (rows 13-15, cols A-L)
write_block('T3', t3_perm, start_row = 13, start_col = 1)


# =============================================================================
# F5: Distributional Effects (2 sections)
# =============================================================================

message('  Updating F5 (Distribution)...')

f5_temp <- build_f5(out_temp)
f5_perm <- build_f5(out_perm)

# Section 1: "Section 122 Expires" = temp
# pct at row 9, cols B-K; cost at row 13, cols B-K
pct_temp <- as.data.frame(as.list(f5_temp$pct))
cost_temp <- as.data.frame(as.list(f5_temp$cost))
write_block('F5', pct_temp, start_row = 9, start_col = 2)
write_block('F5', cost_temp, start_row = 13, start_col = 2)

# Section 2: "Section 122 Extended" = perm
# pct at row 19, cols B-K; cost at row 23, cols B-K
pct_perm <- as.data.frame(as.list(f5_perm$pct))
cost_perm <- as.data.frame(as.list(f5_perm$cost))
write_block('F5', pct_perm, start_row = 19, start_col = 2)
write_block('F5', cost_perm, start_row = 23, start_col = 2)


# =============================================================================
# F6: Commodity Price Effects (4 data columns: temp SR/LR = B/C, perm SR/LR = D/E)
# =============================================================================

message('  Updating F6 (Commodities)...')

f6_temp <- build_f6(out_temp)
f6_perm <- build_f6(out_perm)

# Write names + temp SR/LR (cols A-C)
write_block('F6', f6_temp, start_row = 8, start_col = 1)

# Align perm data to temp's commodity sort order before writing cols D-E
f6_perm_aligned <- f6_temp %>%
  select(Name) %>%
  left_join(f6_perm, by = 'Name') %>%
  select(`Short-Run`, `Long-Run`)

write_block('F6', f6_perm_aligned, start_row = 8, start_col = 4)


# =============================================================================
# Apply white background and save
# =============================================================================

message('  Applying formatting and saving...')

apply_white_background <- function(sheet, max_rows = 500, max_cols = 50) {
  white_style <- createStyle(fgFill = 'white')
  addStyle(
    wb,
    sheet = sheet,
    style = white_style,
    rows = 1:max_rows,
    cols = 1:max_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
}

for (sheet_name in c('Data TOC', 'T1', 'T2', 'F1', 'F2', 'F3', 'F4', 'T3', 'F5', 'F6')) {
  apply_white_background(sheet_name)
}

suppress_openxlsx_warnings(saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE))

message(sprintf('Done! Output saved to: %s', OUTPUT_PATH))
