# The Budget Lab Tariff Model

An R pipeline for estimating the economic effects of U.S. tariff policy,
including effective tariff rates, consumer price effects, revenue, macro effects,
sector output effects, foreign GDP effects, and distributional impacts.

## Overview

The top-level entrypoint is `run.R`. It runs the full scenario pipeline
orchestrated by `src/run_model.R`.

Main stages:

| Step | Module | Description |
|------|--------|-------------|
| 0a | `00a_prepare_rate_inputs.R` | Reads a published tariff-rate-tracker bundle and writes model-ready rate matrices |
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
| tariff-rate-tracker output bundle | HS10-country rate panel and import weights |
| GTAP + GEMPACK | Trade model simulation and extraction |

Required R packages:

```r
install.packages(c('tidyverse', 'yaml', 'HARr', 'openxlsx'))
```

Create a local GTAP config from the example file:

```bash
cp config/gtap_config.yaml.example config/gtap_config.yaml
```

```powershell
Copy-Item config/gtap_config.yaml.example config/gtap_config.yaml
```

Each scenario must have a `rate_panel:` block in
`config/scenarios/<scenario>/model_params.yaml` pointing at a published tracker
bundle.

## Usage

```bash
Rscript run.R tracker_actual_2026-06-13
Rscript run.R tracker_actual_2026-06-13 --markup constant_dollar
Rscript run.R tracker_actual_2026-06-13 --bea-io-level detail
```

From an R session:

```r
source('src/run_model.R')
run_scenario('tracker_actual_2026-06-13')
```

Options:

| Option | Description |
|--------|-------------|
| `--markup average` | Average of upper and lower markup bounds |
| `--markup constant_percentage` | Upper-bound markup assumption |
| `--markup constant_dollar` | Lower-bound markup assumption, the default |
| `--bea-io-level summary` | Use summary BEA I-O tables |
| `--bea-io-level detail` | Use detail BEA I-O tables |

## Scenario Config

Rate inputs are read from the tracker bundle declared in the scenario config:

```yaml
rate_panel:
  root: 'C:/Users/jar335/Documents/Interfaces/model_data/tariff-rate-tracker'
  vintage: 'latest_2026_06_13'
  weights_vintage: '2026-06-04_2'
  tracker_scenario: 'actual'
  baseline_date: '2025-01-01'
  interval_end: 'inclusive'
```

Step 0a writes model-ready artifacts to `output/<scenario>/rate_inputs/`; later
stages read only that directory.

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
