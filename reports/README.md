# Excel Data Download

This directory contains the Excel template used for the State of Tariffs data
download export.

## Files

| File | Purpose |
|------|---------|
| `data_download_template.xlsx` | Workbook template used by `export_excel_tables()` |
| `tariff_report_template.docx` | Legacy reference file kept for archival context |

## Usage

Export the data download workbook from R:

```r
source('src/helpers.R')
source('src/12_export_excel.R')

export_excel_tables(
  scenario = '2-21_temp',
  report_date = 'March 09, 2026'
)
```

The workbook is written to `output/{scenario}/report/data_download.xlsx`.
