suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(yaml)
  library(scales)
})
source('src/helpers.R')
source('src/12_export_excel.R')

scenarios <- c('2-19', '2-20', '2-21_perm', '2-21_temp')
for (s in scenarios) {
  cat(sprintf('\n=== Exporting %s ===\n', s))
  tryCatch(
    export_excel_tables(s, 'March 09, 2026'),
    error = function(e) cat(sprintf('ERROR: %s\n', e$message))
  )
}
