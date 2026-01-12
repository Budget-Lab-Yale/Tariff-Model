# Distribution Analysis Implementation Plan

## Overview

This document outlines the implementation plan for adding distributional analysis (per-household costs by income decile) to the R tariff model pipeline.

## Current State

The Excel model (`ricco_tariffs_11-17.xlsm`) calculates distribution in sheet `F6 Distribution (C)`:
- **Output**: Per-household cost by income decile (columns B-K = deciles 1-10)
- **Summary**: Cell N23 = `AVERAGE(B23:K23)` feeds back to Key Results as average per-household cost ($1,671)
- **R pipeline gap**: The current `03_calculate_prices.R` uses a rough scaling factor (`HH_SCALING_FACTOR = -135000`) as a placeholder

## Required Data Sources

### 1. Household Consumption Data by Income Decile

**What it contains:**
- Consumption expenditure by product category for each income decile
- Source: Bureau of Labor Statistics Consumer Expenditure Survey (CES) or BEA PCE data

**Structure needed:**
```
decile, category, expenditure, expenditure_share
1,      food,     5000,        0.25
1,      apparel,  1200,        0.06
...
10,     food,     8000,        0.10
10,     apparel,  3500,        0.04
```

### 2. Product-to-Tariff Mapping

**What it contains:**
- Links consumer expenditure categories to tariffed product groups
- Maps to GTAP sectors or HS code groupings

**Structure needed:**
```
category,    gtap_sector, import_share
food,        ofd,         0.15
apparel,     wap,         0.85
electronics, ele,         0.90
...
```

### 3. Tariff Rates by Product (Already Available)

From existing ETR calculations - weighted rates by sector/country

## Calculation Methodology

### Core Formula

For each income decile `d`, the tariff cost is:

```
Cost_d = Sum over categories c of:
  (ETR_c * Import_Share_c * Consumption_c,d)
```

Where:
- `ETR_c` = effective tariff rate for category c
- `Import_Share_c` = fraction of category c that's imported
- `Consumption_c,d` = expenditure by decile d on category c

### As Percentage of Income

```
Cost_Pct_d = Cost_d / Income_d * 100
```

### Regressivity Measure

Lower-income deciles typically spend higher fractions on:
- Food (higher import share due to produce)
- Apparel (high import penetration, ~85% from abroad)
- Basic electronics/appliances

## Implementation Steps

### Step 1: Extract Data from Excel

Run `scripts/extract_distribution_data.R` to:
- Extract the F6 Distribution sheet structure
- Identify what consumption/expenditure data is embedded
- Save as CSVs in `resources/distribution/`

### Step 2: Create Resource Files

Create/populate the following CSV files:

**`resources/distribution/household_consumption.csv`**
- 10 decile rows (or can be wider format)
- Columns for each consumption category
- Source: Extract from Excel or pull from BLS/BEA

**`resources/distribution/category_tariff_mapping.csv`**
- Map consumption categories to GTAP sectors
- Include import penetration rates per category

**`resources/distribution/decile_income.csv`**
- Average income by decile (for percentage calculations)
- Source: Census/BLS

### Step 3: Implement `09_calculate_distribution.R`

```r
# Pseudocode structure:

calculate_distribution <- function(etr_results, inputs) {

  # Load distribution inputs
  consumption <- inputs$household_consumption
  category_map <- inputs$category_tariff_mapping
  decile_income <- inputs$decile_income

  # Get sector-level ETRs from etr_results
  sector_etrs <- etr_results$sector_etrs

  # Join category mapping to get ETR per consumption category
  category_etrs <- category_map %>%
    left_join(sector_etrs, by = 'gtap_sector')

  # Calculate cost per decile
  decile_costs <- consumption %>%
    left_join(category_etrs, by = 'category') %>%
    mutate(
      tariff_cost = expenditure * import_share * etr
    ) %>%
    group_by(decile) %>%
    summarise(
      total_cost = sum(tariff_cost),
      .groups = 'drop'
    ) %>%
    left_join(decile_income, by = 'decile') %>%
    mutate(
      cost_pct_income = total_cost / income * 100
    )

  # Summary statistics
  avg_cost <- mean(decile_costs$total_cost)

  return(list(
    decile_costs = decile_costs,
    avg_per_hh_cost = avg_cost,
    cost_by_decile = decile_costs$total_cost
  ))
}
```

### Step 4: Update Pipeline Integration

**In `01_load_inputs.R`:**
```r
# Add distribution data loading
inputs$household_consumption <- read_csv('resources/distribution/household_consumption.csv')
inputs$category_tariff_mapping <- read_csv('resources/distribution/category_tariff_mapping.csv')
inputs$decile_income <- read_csv('resources/distribution/decile_income.csv')
```

**In `run_model.R`:**
```r
# Add step 9
source('src/09_calculate_distribution.R')
...
message('Step 9: Calculating distribution...')
distribution_results <- calculate_distribution(etr_results, inputs)
```

**In `03_calculate_prices.R`:**
```r
# Replace the HH_SCALING_FACTOR approximation with actual distribution results
# OR: Move per-HH cost calculation entirely to 09_calculate_distribution.R
```

### Step 5: Output Format

Add to final results:

```yaml
distribution:
  pre_substitution:
    avg_per_hh_cost: 1671
    by_decile:
      - decile: 1
        cost: 2500
        pct_income: 2.5
      - decile: 2
        cost: 2100
        pct_income: 1.8
      ...
  post_substitution:
    avg_per_hh_cost: 1257
    by_decile: ...
```

## Data Source Options

### Option A: Extract from Excel (Quickest)

Run the extraction script to pull the exact consumption/expenditure matrix from the F6 Distribution sheet. This ensures exact replication.

### Option B: Build from Primary Sources

1. **BLS Consumer Expenditure Survey** - Official household spending by income quintile
2. **BEA Personal Consumption Expenditures** - National accounts consumption
3. **Census Bureau Current Population Survey** - Income distribution

## Dependencies and Risks

| Risk | Mitigation |
|------|------------|
| Excel data structure unclear | Run extraction script first to examine |
| Consumption categories don't map to GTAP | Create crosswalk mapping table |
| Data only available by quintile (5 groups) | Interpolate to 10 deciles or use quintiles |
| Import shares vary by year | Use latest available baseline year |

## Timeline Estimate

1. **Data extraction and examination**: Run script, analyze output
2. **Resource file creation**: Depends on data availability
3. **R function implementation**: ~2-4 hours
4. **Integration and testing**: ~1-2 hours

## Next Steps

1. Run `scripts/extract_distribution_data.R` in R/RStudio
2. Review the extracted CSV files to understand data structure
3. Identify any gaps in consumption/expenditure data
4. Proceed with implementation based on findings
