# CBO Dynamic Scoring Parameters

## Source
Extracted from CBO Rules of Thumb Workbook:
`excel_model/61183-Rules-of-Thumb-2025_unlocked.xlsx`

## The CBO Model Structure

The full CBO model maps **productivity shocks** to **revenue changes** through a distributed lag system:

```
1. Productivity Deviation
   deviation[t] = prod_scenario[t] / prod_baseline[t] - 1

2. Real GDP (impulse response convolution)
   GDP_real[t] = GDP_base[t] × (1 + Σ β[t,s] × deviation[s])

   Where β is a triangular matrix:
   - β[t,t] ≈ 1.096  (first-year effect: >100% elasticity)
   - β[t,s] ≈ 0.03-0.04 for s < t  (persistent but decaying)

3. Nominal GDP
   GDP_nom[t] = (GDP_real[t] / GDP_real_base[t]) × GDP_nom_base[t]

4. Fiscal Year Conversion
   FY_GDP[t] = 0.75 × CY_GDP[t] + 0.25 × CY_GDP[t-1]

5. Revenue Change (linear approximation)
   ΔRevenue[t] = sensitivity[t] × ΔGDP_nominal_FY[t]
```

## For the Tariff Model

Since MAUS already provides GDP changes (tariff scenario vs baseline), we only need step 5:

```
dynamic_effect[t] = revenue_per_gdp[t] × nominal_gdp_change[t]
```

Where:
- `nominal_gdp_change` = MAUS GDP (tariff) - MAUS GDP (baseline), converted to nominal and fiscal year
- `revenue_per_gdp` = CBO's pre-computed sensitivity (varies by year)

## The Sensitivity Formula

From the CBO workbook (sheet4, cell C24):

```
sensitivity[t] = CBO_revenue_change[t] / CBO_GDP_change[t]
```

This is derived from CBO's own illustrative scenario in the rules of thumb workbook:
- CBO_revenue_change: Row 11 in sheet8 (5. Productivity_CBO)
- CBO_GDP_change: Row 57 - Row 56 in sheet8

The sensitivity increases from ~0.17 to ~0.20 because CBO's internal models show:
- Revenue elasticity to GDP is not constant
- It rises slightly over time due to bracket creep and other factors

## File: revenue_sensitivity.csv

| Column | Description |
|--------|-------------|
| fiscal_year | Fiscal year (2025-2035) |
| revenue_per_gdp | Sensitivity: revenue change per $1B GDP change |
| cbo_gdp_base | CBO baseline nominal GDP (billions) |
| cbo_gdp_scenario | CBO scenario nominal GDP (billions) |
| cbo_gdp_change | GDP change in CBO scenario (billions) |
| cbo_revenue_change | Revenue change in CBO scenario (billions) |
