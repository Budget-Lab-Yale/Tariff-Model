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

## Reading Excel Files

Excel files (.xlsx, .xlsm) are ZIP archives containing XML files. To read them:

```bash
# Extract the Excel file structure
unzip -l excel_model/ricco_tariffs_11-17.xlsm

# Extract specific sheet XML (sheets are in xl/worksheets/)
unzip -p excel_model/ricco_tariffs_11-17.xlsm xl/worksheets/sheet1.xml

# Get sheet names from workbook.xml
unzip -p excel_model/ricco_tariffs_11-17.xlsm xl/workbook.xml

# Shared strings (cell text values) are in sharedStrings.xml
unzip -p excel_model/ricco_tariffs_11-17.xlsm xl/sharedStrings.xml
```

**Important:** The Excel model is ground truth. When replicating calculations, always examine the actual Excel formulas in the XML, don't guess.

## Coding Style Guidelines

Based on Tariff-ETRs codebase patterns:

### General
- 2-space indentation
- Single quotes for all strings: `'like this'`
- Explicit `return()` at end of functions
- Roxygen-style docstrings with `#'` for functions

### Tidyverse-First
- Heavy use of dplyr/tidyr — no data.table
- Pipe chains with `%>%`
- `mutate()`, `select()`, `filter()` for transformations
- `group_by() + summarise()` for aggregations (always use `.groups = 'drop'`)
- `left_join()` for combining data (prefer joins over loops)
- `pivot_longer()` / `pivot_wider()` for reshaping

### Conditionals
- `case_when()` for complex multi-condition logic
- `if_else()` (not base R `ifelse()`) for simple conditionals
- `coalesce()` for fallback values

### Error Philosophy
- **Never use `na.rm = TRUE`** — missing values indicate bugs, let them surface
- **Never use preemptive `replace_na()`** — same reason
- Let operations fail loudly so we catch data issues early

### Naming
- snake_case for variables and functions
- Descriptive names: `weighted_etr` not `w_etr`
- Constants in UPPER_SNAKE_CASE: `CTY_CHINA <- '5700'`

### Structure
- Section separators: `# ====` blocks for major sections
- Comments explain *why*, not *what*
- Prefer tabular data over lists — convert early, join instead of loop

### Example Pattern
```r
result <- input_data %>%
  left_join(rates, by = 'hs10') %>%
  mutate(
    final_rate = case_when(
      rate_232 > 0 ~ rate_232,
      TRUE ~ ieepa_rate
    ),
    weighted = final_rate * imports
  ) %>%
  group_by(partner) %>%
  summarise(
    etr = sum(weighted) / sum(imports),
    .groups = 'drop'
  )

return(result)
```
