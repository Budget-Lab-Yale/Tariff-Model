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

To build the combined four-scenario workbook used for the report:

```bash
Rscript src/update_state_of_tariffs.R \
  --template reports/data_download_template.xlsx \
  --output output/combined/state_of_tariffs_data_download.xlsx \
  --report-date "March 09, 2026"
```

To append the 15% Section 122 columns to that combined workbook:

```bash
Rscript src/add_s122_15_columns.R \
  --input output/combined/state_of_tariffs_data_download.xlsx \
  --output output/combined/state_of_tariffs_data_download_15.xlsx
```
