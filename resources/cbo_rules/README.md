# CBO Dynamic Scoring Parameters

## Source

Congressional Budget Office (CBO) Rules of Thumb Workbook:
- **Public download**: https://www.cbo.gov/publication/61183
- **Local copy**: `excel_model/61183-Rules-of-Thumb-2025.xlsx`

The workbook contains linearized coefficients for converting productivity growth shocks
to revenue effects. These coefficients are stored in hidden rows (108-118) on the
"1. Productivity" tab but are accessible via XML extraction from the xlsx file.

The formulas in row 24 explicitly reference these hidden coefficients:
```
D24 = SUMPRODUCT($C13:$M13, TRANSPOSE(D108:D118)) - D120*(D13*0.75+C13*0.25)
```

## The CBO Model Structure

The full CBO model maps **productivity shocks** to **revenue changes** through a distributed lag system
with impulse response convolution. The R implementation in `src/07_calculate_dynamic_revenue.R` matches
this methodology exactly, producing dynamic revenue estimates within 0.1% of Excel.

### Full Methodology

```
1. Input: FY Growth Rate Deviations (percentage points)
   These are the "shocks" to productivity growth relative to baseline.
   Example: FY2025 = -0.187%, FY2026 = -0.626%, FY2027 = +0.076%

2. Apply to CBO Baseline Productivity Growth
   scenario_growth[t] = baseline_growth[t] + growth_deviation[t]

   Row 17 = Row 16 + Row 13 in CBO workbook

3. Cumulate into Level Indices
   baseline_index[t] = baseline_index[t-1] × (1 + baseline_growth[t]/100)
   scenario_index[t] = scenario_index[t-1] × (1 + scenario_growth[t]/100)

   Starting from base = 100 (rows 113-114)

4. Compute Level Deviation for Convolution
   level_dev[t] = scenario_index[t] / baseline_index[t] - 1

   Row 115 in CBO workbook

5. Apply Impulse Response Convolution to Real GDP
   scenario_real_gdp[t] = baseline_real_gdp[t] × (1 + Σ impulse[s] × level_dev[t-s+1])

   Where impulse coefficients are:
   - impulse[1] = 1.096  (first-year: >100% elasticity)
   - impulse[2] = 0.041  (second-year lagged effect)
   - impulse[3] = 0.038  (third-year lagged effect)
   - etc. (decaying but persistent)

   Row 64 formula: D64 = D63*(1+$D108*C115+$C108*D115)

6. Scale to Nominal GDP
   scenario_nominal_gdp[t] = baseline_nominal_gdp[t] × (scenario_real_gdp[t] / baseline_real_gdp[t])

   Row 67 in CBO workbook

7. Apply Fiscal Year Weighting
   fy_scenario_gdp[t] = baseline_nominal_gdp[t] + nominal_diff[t]*0.75 + nominal_diff[t-1]*0.25
   fy_gdp_change[t] = fy_scenario_gdp[t] - baseline_nominal_gdp[t]

   Row 70 formula: D70 = D69+(D67-D66)*0.75+(C67-C66)*0.25

8. Apply CBO Sensitivity
   sensitivity[t] = cbo_revenue_change[t] / (cbo_gdp_scenario[t] - cbo_gdp_baseline[t])
   dynamic_effect[t] = sensitivity[t] × fy_gdp_change[t]

   Row 24 formula: D24 = D11/(D57-D56) × (D70-D69)
```

## Computing Growth Deviations from MAUS

The R model calculates FY growth deviations from MAUS quarterly GDP data:

### Step 1: Map Calendar Quarters to Fiscal Years
```
CY Q4 → FY Q1 of next year (Oct-Dec)
CY Q1 → FY Q2 (Jan-Mar)
CY Q2 → FY Q3 (Apr-Jun)
CY Q3 → FY Q4 (Jul-Sep)
```

### Step 2: Compute Quarterly Level Deviations
```
level_deviation[q] = 100 × (GDP_tariff[q] / GDP_baseline[q] - 1)
```

### Step 3: Average by Fiscal Year
```
avg_level_deviation[FY] = mean(level_deviation for FY quarters)
```

### Step 4: Compute Incremental Growth Deviations
```
growth_deviation[FY] = avg_level_deviation[FY] - avg_level_deviation[FY-1]
```

This produces values matching Excel column AI:
- FY2025: -0.187%
- FY2026: -0.626%
- FY2027: +0.076%

## Files

### cbo_convolution_params.csv

Contains all parameters needed for the impulse response convolution:

| Column | Description |
|--------|-------------|
| fiscal_year | Fiscal year (2025-2035) |
| baseline_productivity_growth | CBO baseline productivity growth rate (%) |
| impulse_response | Impulse response coefficient for that lag |
| baseline_real_gdp | CBO baseline real GDP (billions) |
| baseline_nominal_gdp | CBO baseline nominal GDP (billions) |
| cbo_revenue_change | Revenue change in CBO's 1% shock scenario |
| cbo_gdp_scenario | GDP in CBO's 1% shock scenario |
| cbo_gdp_baseline | GDP baseline for computing sensitivity |

### revenue_sensitivity.csv (legacy)

Simpler sensitivity parameters (superceded by cbo_convolution_params.csv).

## Validation Results

The R implementation matches Excel within 0.1%:

| FY | R Model | Excel | Diff |
|----|---------|-------|------|
| 2025 | -$7.91B | -$7.91B | 0.06% |
| 2026 | -$41.12B | -$41.12B | 0.00% |
| 2027 | -$52.09B | -$52.09B | 0.00% |
| 2028 | -$44.15B | -$44.13B | 0.03% |
| 2029 | -$35.11B | -$35.10B | 0.03% |
