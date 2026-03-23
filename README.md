# The Budget Lab Tariff Model

An R-based pipeline for estimating the economic effects of U.S. tariff policy,
including effective tariff rates, consumer price effects, revenue, macro
effects, sector output effects, foreign GDP effects, and distributional impacts.

## Overview

The top-level entrypoint is [run.R](/C:/Users/jar335/Documents/Repositories/Tariff-Model/run.R).
It runs the full scenario pipeline orchestrated by
[src/run_model.R](/C:/Users/jar335/Documents/Repositories/Tariff-Model/src/run_model.R).

Main stages:

| Step | Module | Description |
|------|--------|-------------|
| 0 | `00_run_tariff_etrs.R` | Runs Tariff-ETRs and writes tariff matrices |
| 0b | `00b_run_gtap.R` | Runs the GTAP trade model |
| 1 | `01_load_inputs.R` | Loads baselines, mappings, assumptions, and model inputs |
| 2 | `02_calculate_etr.R` | Calculates weighted effective tariff rates |
| 3 | `run_model.R` + `io_price_model.R` | Computes pre-sub, post-sub, and GE price effects |
| 3a | `05a_usmm_surrogate.R` | Constructs the USMM surrogate macro response |
| 4 | `04_calculate_revenue.R` | Calculates conventional revenue |
| 5 | `05_calculate_macro.R` | Calculates macro effects |
| 6 | `06_calculate_sectors.R` | Calculates sector output effects |
| 7 | `07_calculate_dynamic_revenue.R` | Calculates dynamic revenue effects |
| 8 | `08_calculate_foreign_gdp.R` | Calculates foreign GDP effects |
| 9 | `09_calculate_distribution.R` | Calculates pre-sub and post-sub household burdens |
| 10 | `11_write_outputs.R` | Writes result CSVs |
| 11 | `12_export_excel.R` | Builds report workbook tables from saved outputs |

## Requirements

Required software:

| Software | Purpose |
|----------|---------|
| R 4.0+ | Main runtime |
| Tariff-ETRs repo | Tariff matrix generation |
| GTAP + GEMPACK | Trade model simulation and extraction |

Required R packages:

```r
install.packages(c('tidyverse', 'yaml', 'HARr', 'openxlsx'))
```

External dependencies:

1. Clone `Tariff-ETRs` as a sibling repo at `../Tariff-ETRs`.
2. Install GTAP and GEMPACK.
3. Create a local GTAP config from the example file.

## Installation

```bash
git clone https://github.com/Budget-Lab-Yale/Tariff-Model.git
cd Tariff-Model

cd ..
git clone https://github.com/Budget-Lab-Yale/Tariff-ETRs.git
cd Tariff-Model

Rscript -e "install.packages(c('tidyverse', 'yaml', 'HARr', 'openxlsx'))"
```

## GTAP Setup

Copy the example config and edit it for your machine:

```bash
cp config/gtap_config.yaml.example config/gtap_config.yaml
```

```powershell
Copy-Item config/gtap_config.yaml.example config/gtap_config.yaml
```

Example contents:

```yaml
gtap_executable: 'C:/runGTAP375/gtapv7.exe'
gtap_work_dir: 'C:/runGTAP375/work'
gtap_data_dir: 'C:/runGTAP375/tbltarv7'
gtap_aux_dir: 'C:/runGTAP375'
sltoht_executable: 'C:/GP/sltoht.exe'
```

The model validates these paths on startup and reports missing executables,
directories, or GTAP data files.

## Command Line Usage

Basic usage:

```bash
Rscript run.R <scenario_name> [options]
```

Options:

| Option | Description |
|--------|-------------|
| `--markup average` | Average of upper and lower markup bounds (default) |
| `--markup constant_percentage` | Upper-bound markup assumption |
| `--markup constant_dollar` | Lower-bound markup assumption |
| `--bea-io-level summary` | Use summary BEA I-O tables (default) |
| `--bea-io-level detail` | Use detail BEA I-O tables |

Examples:

```bash
Rscript run.R 2-21_perm
Rscript run.R 2-21_perm --markup constant_dollar
Rscript run.R 2-21_perm --bea-io-level detail
Rscript run.R
```

From an R session:

```r
source('src/run_model.R')
run_scenario('2-21_perm')
```

## Scenarios

Current scenario directories:

```text
2-19
2-20
2-21_perm
2-21_perm_15
2-21_temp
2-21_temp_15
```

To create a new scenario:

```bash
cp -r config/scenarios/2-21_perm config/scenarios/my-scenario
```

Then edit:

- `config/scenarios/my-scenario/model_params.yaml`
- baseline tariff config files under `baseline/`
- date-specific tariff config files under `tariff_etrs/`
- optional retaliation shocks in `retaliation/shocks.txt`

## Outputs

Scenario results are written to `output/{scenario}/results/`.

Key files:

| File | Description |
|------|-------------|
| `key_results.csv` | Headline results |
| `goods_weighted_etrs.csv` | Weighted ETRs by goods category |
| `revenue_by_year.csv` | Conventional annual revenue |
| `dynamic_revenue_by_year.csv` | Revenue with GDP feedback |
| `macro_quarterly.csv` | Quarterly macro series |
| `sector_effects.csv` | Sector output effects |
| `foreign_gdp.csv` | Foreign GDP effects |
| `distribution.csv` | Pre-substitution household burden by decile |
| `distribution_postsub.csv` | Post-substitution household burden by decile |
| `pce_category_prices.csv` | Consumer-category price effects |
| `bea_commodity_prices.csv` | BEA commodity price effects |

## Excel Export

Export a single-scenario report workbook:

```r
source('src/helpers.R')
source('src/12_export_excel.R')

export_excel_tables(
  scenario = '2-21_temp',
  report_date = 'March 09, 2026'
)
```

This writes to `output/{scenario}/report/data_download.xlsx`.

Build the combined four-scenario workbook used for the report:

```bash
Rscript src/update_state_of_tariffs.R \
  --template reports/data_download_template.xlsx \
  --output output/combined/state_of_tariffs_data_download.xlsx \
  --report-date "March 09, 2026"
```

Append the 15% Section 122 columns:

```bash
Rscript src/add_s122_15_columns.R \
  --input output/combined/state_of_tariffs_data_download.xlsx \
  --output output/combined/state_of_tariffs_data_download_15.xlsx
```

## Repo Layout

```text
Tariff-Model/
|-- run.R
|-- config/
|   |-- global_assumptions.yaml
|   |-- gtap_config.yaml.example
|   `-- scenarios/
|-- reports/
|   |-- README.md
|   `-- data_download_template.xlsx
|-- resources/
|-- src/
|   |-- run_model.R
|   |-- helpers.R
|   |-- read_gtap.R
|   |-- io_price_model.R
|   `-- util/
`-- output/
```
