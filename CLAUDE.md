# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is the Yale Budget Lab Tariff Model - an R-based pipeline that automates tariff impact analysis. It calculates effective tariff rates (ETRs), consumer price effects, revenue estimates, and macroeconomic impacts.

## Architecture

The model orchestrates multiple components:

1. **Tariff-ETRs** (external repo): Calculates ETR matrices from statutory tariff parameters
2. **GTAP results** (manual input for now): Trade model outputs for sector/GDP effects
3. **MAUS results** (manual input for now): Macro model outputs for short-run GDP/unemployment
4. **This model**: Combines all inputs to produce final estimates

```
Statutory Params → Tariff-ETRs → ETR Matrix ──┐
                                               ├──→ This Model → Key Results
GTAP/MAUS outputs (manual CSVs) ──────────────┘
```

## Directory Structure

```
config/
  scenarios/{scenario_name}/
    tariff_etrs/          # Config passed to Tariff-ETRs (232.yaml, ieepa_*.yaml, etc.)
    other_models/         # GTAP and MAUS output CSVs (manual for now)
    model_params.yaml     # Price passthrough, shares, elasticities

resources/
  cbo_baselines/          # CBO import/duty projections
  mappings/               # GTAP sector mappings, crosswalks

src/
  run_model.R             # Main orchestrator
  00_run_tariff_etrs.R    # Calls Tariff-ETRs via CLI
  01_load_inputs.R        # Load all inputs
  02_calculate_etr.R      # Weighted ETR calculations
  03_calculate_prices.R   # Price effects, per-household cost
  04_calculate_revenue.R  # Conventional + dynamic revenue
  05_calculate_macro.R    # GDP, unemployment effects
  06_calculate_sectors.R  # Sector output effects

output/                   # Generated outputs (gitignored)
excel_model/              # Reference Excel model and documentation
```

## Running the Model

```r
# From project root:
source('src/run_model.R')

# Or with a specific scenario:
run_scenario('scenario_name')
```

## Key Calculations

Based on the Excel model dependency map in `excel_model/tariff_model_dependency_map.md`:

1. **Weighted ETR**: `sum(country_etr * import_weights) / sum(import_weights)`

2. **Price Effect**:
   ```
   price_increase = (etr * -usd_offset * goods_share * import_share) +
                    (etr * import_share * goods_share * (1 + passthrough))
   ```

3. **Revenue**:
   ```
   gross_revenue = new_duties - baseline_duties
   net_revenue = gross_revenue * (1 - compliance_effect - income_effect)
   ```

4. **GDP Impact (Q4-Q4)**:
   ```
   gdp_impact = ((tariff_gdp_q4 / tariff_gdp_q4_prior) /
                 (baseline_gdp_q4 / baseline_gdp_q4_prior) - 1) * 100
   ```

## External Dependencies

- **Tariff-ETRs repo**: Located at `../Tariff-ETRs`, called via `Rscript`
- R packages: tidyverse, yaml

## Style Guidelines

- Use single quotes for strings
- Prefer tabular data structures over loops
- Never use `na.rm = TRUE` - missing values should cause errors to surface bugs
- 2 spaces for indentation
