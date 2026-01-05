# Yale Budget Lab Tariff Model - Dependency Map

## Overview

This document maps the calculation dependencies in `ricco_tariffs_11-17.xlsm`. Each result is traced back to its inputs and intermediate calculations.

---

## 1. EFFECTIVE TARIFF RATES

### 1.1 Pre-Substitution ETR (Increase)
**Output:** `Key Results!B3` = 14.4%

```
Key Results!B3
  └── 'F1 Historical'!F239
        └── = F236 - E236
              ├── F236 = E237 (All-In Pre-Sub ETR)
              └── E236 = B236 (Baseline ETR = 2.418%)
```

**Calculation:** Pre-Sub ETR Increase = All-In ETR - Baseline ETR

### 1.2 Pre-Substitution ETR (All-In)
**Output:** `Key Results!B4` = 16.8%

```
Key Results!B4
  └── 'F1 Historical'!F237
        └── = E237
              └── = B236 + ricco_price_effects_and_etr!D25 * 100
                    ├── B236 = 2.418 (Baseline ETR, hard-coded)
                    └── D25 = BW55/100 (Post-sub weighted ETR)
                          └── BW55 = SUMPRODUCT(BO55:BV55, BE55:BL55) / SUM(BE55:BL55)
                                     [Weighted average of country ETRs by import shares]
```

### 1.3 Post-Substitution ETR (Increase & All-In)
**Output:** `Key Results!B7` = 11.9%, `Key Results!B8` = 14.4%

```
Key Results!B7 (Increase)
  └── 'F1 Historical'!D239 = D236 - C236

Key Results!B8 (All-In)
  └── 'F1 Historical'!D237
        └── = B236 + ricco_price_effects_and_etr!D25 * 100
```

### ETR Input Sources

| Input | Location | Description |
|-------|----------|-------------|
| Baseline ETR | `F1 Historical!B236` | 2.418% (2024 baseline) |
| Country ETRs | `Weighted US Tariff!Z83:AG127` | Matrix of tariff rates by GTAP sector × country |
| Import weights | `ricco_price_effects_and_etr!BE:BL` | Country import shares from GTAP |
| Country mapping | `Weighted US Tariff!Z82:AG82` | china, canada, mexico, uk, japan, eu, row, ftrow |

### ETR Calculation Logic
```
Weighted ETR = Σ(Country_ETR × Country_Import_Share) / Σ(Country_Import_Share)
```

Where Country ETRs are built from:
- IEEPA rates (Canada: 35%, Mexico: 25%, China: 20%, EU: 15%, Vietnam: 20%)
- 232 rates (Autos: 25%, Steel/Aluminum: 50%)
- Product-specific adjustments

---

## 2. PRICE EFFECTS

### 2.1 Consumer Price Increase (Pre-Sub)
**Output:** `Key Results!B12` = 1.2%

```
Key Results!B12
  └── ricco_price_effects_and_etr!E24
        └── = (D24 * -B13 * F24 * H24) + (D24*100) * H24 * F24 * (1+B14) / 100
              ├── D24 = BW125/100 (Pre-sub weighted ETR)
              ├── B13 = 0.17 (USD offset = 0.3 * 0.58)
              ├── B14 = 0.50 (Domestic price passthrough)
              ├── F24 = B15 = 0.31 (Goods share of PCE)
              └── H24 = B16 = 0.21 (Import share of goods PCE)
```

### 2.2 Per-Household Cost (Pre-Sub)
**Output:** `Key Results!B13` = $1,671

```
Key Results!B13
  └── = -ricco_price_effects_and_etr!I24
        └── I24 = 'F6 Distribution (C)'!N23
              └── N23 = AVERAGE(B23:K23)
                        [Average cost across income deciles]
```

### 2.3 Post-Substitution Price Effects
**Output:** `Key Results!B14` = 0.9%, `Key Results!B15` = $1,257

Similar structure, using row 25 instead of row 24.

### Price Effect Parameters

| Parameter | Cell | Value | Description |
|-----------|------|-------|-------------|
| USD Offset | `B13` | 0.17 | = 0.3 × 0.58 (exchange rate adjustment) |
| Price Passthrough | `B14` | 0.50 | Domestic price passthrough rate |
| Goods Share of PCE | `B15` | 0.31 | Share of consumption on goods |
| Import Share | `B16` | 0.21 | Import share of goods consumption |

### Price Effect Formula
```
Price_Increase = (ETR × -USD_Offset × Goods_Share × Import_Share)
               + (ETR × 100 × Import_Share × Goods_Share × (1 + Passthrough)) / 100
```

---

## 3. REVENUE ESTIMATES

### 3.1 Conventional Revenue (10-year)
**Output:** `Key Results!B18` = $2,728B

```
Key Results!B18
  └── 'T3 Fiscal'!L6
        └── = SUM(B6:K6) [Sum of FY2026-FY2035]
              └── B6 = 'Fiscal Summary'!C28
                    └── C28 = C25 + C27 + C26
                          ├── C25 = C22 - C17 (Gross revenue - Baseline duties)
                          │     ├── C22 = C23 * C21 (New duties)
                          │     │     └── C21 = C16*(1-C42) + C42*C16*(1+Q11/100)
                          │     │           [Adjusted imports with elasticity]
                          │     └── C17 = 84 (Baseline duties, CBO)
                          ├── C26 = C25 * -0.10 (Compliance effect: -10%)
                          └── C27 = -0.23 * (C22-C17) (Income effect: -23%)
```

### 3.2 Dynamic Revenue (10-year)
**Output:** `Key Results!B19` = $2,342B

```
Key Results!B19
  └── 'T3 Fiscal'!L7
        └── = SUM(B7:K7)
              └── B7 = B6 + B8
                    └── B8 = -41 (Dynamic effect for FY2026)
                          [Hard-coded dynamic scoring adjustments by year]
```

### Revenue Inputs

| Input | Source | Description |
|-------|--------|-------------|
| Baseline Imports | `Fiscal Summary!C16` | CBO January baseline ($4,283B for FY2026) |
| Baseline Duties | `Fiscal Summary!C17` | CBO baseline duties ($84B) |
| Import Elasticity | `Fiscal Summary!C42` | Response of imports to tariffs |
| Compliance Effect | Hard-coded | -10% of gross revenue |
| Income Effect | Hard-coded | -23% of gross revenue |
| Dynamic Effects | `T3 Fiscal!B8:K8` | Year-by-year dynamic scoring adjustments |

---

## 4. MACRO EFFECTS

### 4.1 GDP Impact (2025 Q4-Q4)
**Output:** `Key Results!B22` = -0.50%

```
Key Results!B22
  └── 'F3 GDP'!AF8
        └── = 100 * ((Y8/Y4) - (H8/H4))
              ├── Y8 = H8 * (1 + B8/100) [GDP with tariffs, Q4 2025]
              ├── Y4 = H4 * (1 + B4/100) [GDP with tariffs, Q4 2024]
              ├── H8 = 21203.3 (Baseline GDP Q4 2025, hard-coded)
              ├── H4 = 20805.1 (Baseline GDP Q4 2024, hard-coded)
              └── B8 = 100 * (T8/H8 - 1) [GDP deviation %]
                    └── T8 = 21099.8 (MAUS GDP projection with tariffs)
```

### 4.2 GDP Impact (2026 Q4-Q4)
**Output:** `Key Results!B23` = -0.39%

Same structure using row 12 cells.

### 4.3 Unemployment Rate Change
**Output:** `Key Results!B24` (2025) = +0.28pp, `Key Results!B25` (2026) = +0.62pp

```
Key Results!B24
  └── = 'F3 GDP'!W8 - 'F3 GDP'!K8
        ├── W8 = 4.7 (U-rate with tariffs, hard-coded from MAUS)
        └── K8 = 4.4 (Baseline U-rate, hard-coded)
```

### 4.4 Payroll Change
**Output:** `Key Results!B26` (2025) = -463K, `Key Results!B27` (2026) = -1,230K

```
Key Results!B26
  └── = 1000 * ('F3 GDP'!V8 - 'F3 GDP'!J8)
        ├── V8 = 133.2 (Employment with tariffs, millions)
        └── J8 = 133.7 (Baseline employment, millions)
```

### 4.5 Long-Run GDP
**Output:** `Key Results!B28` = -0.31%

```
Key Results!B28
  └── 'F4 US Sector Output'!B10
        └── = 'F5 Foreign GDP'!B2
              └── = H2 = -0.31 (GTAP simulation result, hard-coded)
```

### Macro Input Sources

| Input | Source | Description |
|-------|--------|-------------|
| Baseline GDP (quarterly) | `F3 GDP!H3:H92` | CBO/MAUS baseline projections |
| Tariff GDP (quarterly) | `F3 GDP!T3:T92` | MAUS projections with tariffs |
| Baseline Unemployment | `F3 GDP!K` column | CBO baseline |
| Tariff Unemployment | `F3 GDP!W` column | MAUS with tariffs |
| Baseline Employment | `F3 GDP!J` column | CBO baseline (millions) |
| Tariff Employment | `F3 GDP!V` column | MAUS with tariffs |
| Long-run GDP effects | `F5 Foreign GDP!H2:H10` | GTAP simulation results |

**Note:** MAUS = Macro model outputs (appears to be pasted in from external model runs)

---

## 5. SECTOR EFFECTS

### 5.1 Sector Output Changes
**Output:** `Key Results!B32:B40`

```
Key Results!B32 (Agriculture)
  └── 'F4 US Sector Output'!B1
        └── = SUMPRODUCT(L11:L24, X11:X24) / SUM(L11:L24)
              ├── L11:L24 = Sector output weights (from GTAP)
              └── X11:X24 = Sector output changes (GTAP simulation, hard-coded)

Key Results!B33 (Mining)
  └── 'F4 US Sector Output'!B2
        └── = SUMPRODUCT(L25:L28, X25:X28) / SUM(L25:L28)

Key Results!B34 (Manufacturing)
  └── 'F4 US Sector Output'!B3
        └── = SUMPRODUCT(L29:L55, X29:X55, S29:S55) / SUMPRODUCT(L29:L55, S29:S55)
              └── S29:S55 = Additional weights (durable/nondurable flags)
```

### Sector Mapping

| Output Sector | GTAP Rows | Description |
|---------------|-----------|-------------|
| Agriculture | 11-24 | pdr, wht, gro, v_f, osd, c_b, pfb, ocr, ctl, oap, rmk, wol, frs, fsh |
| Mining & Extraction | 25-28 | coa, oil, gas, oxt |
| Total Manufacturing | 29-55 | All manufacturing sectors |
| Durable Manufacturing | Subset | Metal products, machinery, electronics, vehicles |
| Advanced Manufacturing | Subset | Electronics, precision instruments |
| Nondurable Manufacturing | Subset | Food, textiles, paper, chemicals |
| Utilities | - | ely, gdt, wtr |
| Construction | - | cns |
| Services | - | trd, otp, wtp, atp, cmn, ofi, isr, obs, ros, osg, dwe |

---

## 6. FOREIGN GDP EFFECTS

**Output:** `Key Results!B45:B53`

```
Key Results!B45 (USA)
  └── 'F5 Foreign GDP'!B2 = H2 = -0.31%

Key Results!B46 (China)
  └── 'F5 Foreign GDP'!B3 = H3 = -0.23%

Key Results!B47 (ROW)
  └── 'F5 Foreign GDP'!B4 = H4 = +0.01%

[etc. for Canada, Mexico, FTROW, Japan, EU, UK]
```

**Source:** Column H values are GTAP simulation results (`qgdp` variable), pasted from RunGTAP.

---

## 7. FOOD PRICE EFFECTS

### 7.1 Short-Run Food Price
**Output:** `Key Results!B58`

```
Key Results!B58
  └── 'Weighted US Tariff'!B11
        └── = SUMPRODUCT(K12:K75, N12:N75, T12:T75) / SUMPRODUCT(K12:K75, T12:T75)
              ├── K12:K75 = Product inclusion flags
              ├── N12:N75 = B10 * M / M10 (Scaled price impacts)
              │     └── B10 = ricco_price_effects_and_etr!E24 * 100
              └── T12:T75 = Price effect from ricco_price_effects_and_etr
```

### 7.2 Long-Run Food Price
**Output:** `Key Results!B59`

```
Key Results!B59
  └── ' LR Products'!AM88
        └── = SUMPRODUCT(X11:X75, AS11:AS75, AH11:AH75) / SUMPRODUCT(AH11:AH75, AS11:AS75)
              [Weighted average of GTAP price effects for food sectors]
```

---

## 8. T1 TABLE (Trading Partner Breakdown)

### ETR by Trading Partner
**Output:** `T1!B6:C10`

```
T1!B6 (China Pre-Sub ETR)
  └── = ricco_price_effects_and_etr!B[row]
        [Country-specific weighted ETR from GTAP import data]

T1!D6 (China Pre-Sub Import Share)
  └── = ricco_price_effects_and_etr!B[row]
        [Country import share]

T1!F6 (Contribution)
  └── = B6 * D6 (ETR × Share)
```

---

## INPUT DATA SOURCES

### External Model Outputs (Pasted In)
| Data | Sheet | Description |
|------|-------|-------------|
| GTAP qgdp | `F5 Foreign GDP!H` | GDP effects by region |
| GTAP sector output | `F4 US Sector Output!X` | Output changes by GTAP sector |
| GTAP price effects | `ricco_price_effects_and_etr` cols AS-BB | Price/quantity effects |
| MAUS GDP | `F3 GDP!T` | Quarterly GDP with tariffs |
| MAUS Unemployment | `F3 GDP!W` | Quarterly U-rate with tariffs |
| MAUS Employment | `F3 GDP!V` | Quarterly payrolls with tariffs |

### Parameter Inputs (Hard-Coded)
| Parameter | Location | Current Value |
|-----------|----------|---------------|
| IEEPA Canada | `Weighted US Tariff!B26` | 35% |
| IEEPA Canada energy | `Weighted US Tariff!B27` | 10% |
| IEEPA Mexico | `Weighted US Tariff!B28` | 25% |
| IEEPA Vietnam | `Weighted US Tariff!B30` | 20% |
| IEEPA China Broad | `Weighted US Tariff!B31` | 20% |
| IEEPA China Reciprocal | `Weighted US Tariff!B32` | 10% |
| IEEPA EU Reciprocal | `Weighted US Tariff!B33` | 15% |
| IEEPA Reciprocal | `Weighted US Tariff!B34` | 10% |
| 232 Autos | `Weighted US Tariff!B37` | 25% |
| 232 Autos EU | `Weighted US Tariff!B38` | 15% |
| 232 Steel & Aluminum | `Weighted US Tariff!B40` | 50% |
| USD Offset | `ricco_price_effects_and_etr!B13` | 0.17 |
| Price Passthrough | `ricco_price_effects_and_etr!B14` | 0.50 |
| Goods Share PCE | `ricco_price_effects_and_etr!B15` | 0.31 |
| Import Share | `ricco_price_effects_and_etr!B16` | 0.21 |
| US-assembled foreign autos | `Weighted US Tariff!B13` | 0.33 |
| Rebate year 1 | `Weighted US Tariff!B14` | 3.75 |

### Data Tables (From Census/GTAP)
| Data | Sheet | Description |
|------|-------|-------------|
| Monthly imports by country/NAICS | `Full Imports` | Census Bureau customs data |
| USMCA-qualifying imports | `USMCA Imports` | Subset qualifying for USMCA treatment |
| GTAP import matrix | `GTAP Imports` | GDP expenditure by component |
| Country tariff assignment | `April 2 Worksheet` | Country × tariff rate matrix |
| CBO baseline imports | `Fiscal Summary!C16:L16` | Multi-year import projections |
| CBO baseline duties | `Fiscal Summary!C17:L17` | Multi-year duty projections |

---

## CALCULATION FLOW DIAGRAM

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              INPUT LAYER                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│  Tariff Rates          Import Data           Model Outputs      Parameters  │
│  (Weighted US)         (Full Imports,        (GTAP, MAUS)       (Various)   │
│                         USMCA, Census)                                       │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           CALCULATION LAYER                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│   ┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐     │
│   │ ricco_price_     │    │ Fiscal Summary   │    │ F3 GDP           │     │
│   │ effects_and_etr  │    │                  │    │                  │     │
│   │                  │    │ Revenue =        │    │ GDP_impact =     │     │
│   │ ETR = Σ(rate×wt) │    │ (New_duties -    │    │ (Tariff_GDP /    │     │
│   │ Price = f(ETR,   │    │  Baseline) ×     │    │  Baseline_GDP) - │     │
│   │   passthrough)   │    │ (1-compliance-   │    │  1               │     │
│   │                  │    │  income_effect)  │    │                  │     │
│   └────────┬─────────┘    └────────┬─────────┘    └────────┬─────────┘     │
│            │                       │                       │                │
│   ┌────────┴─────────┐    ┌────────┴─────────┐    ┌────────┴─────────┐     │
│   │ F6 Distribution  │    │ T3 Fiscal        │    │ F4 Sector Output │     │
│   │                  │    │                  │    │                  │     │
│   │ Cost_per_HH =    │    │ 10yr_revenue =   │    │ Sector_effect =  │     │
│   │ AVERAGE(deciles) │    │ SUM(annual)      │    │ SUMPRODUCT(GTAP) │     │
│   └────────┬─────────┘    └────────┬─────────┘    └────────┬─────────┘     │
│            │                       │                       │                │
└────────────┼───────────────────────┼───────────────────────┼────────────────┘
             │                       │                       │
             ▼                       ▼                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              OUTPUT LAYER                                    │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│   ┌─────────────────────────────────────────────────────────────────────┐   │
│   │                         KEY RESULTS                                  │   │
│   ├─────────────────────────────────────────────────────────────────────┤   │
│   │  Effective Rates │ Prices      │ Revenue    │ Macro    │ Sectors   │   │
│   │  Pre:  14.4%     │ Pre:  1.2%  │ Conv: $2.7T│ GDP: -0.5│ Ag:  -1.4%│   │
│   │  Post: 11.9%     │ Post: 0.9%  │ Dyn:  $2.3T│ U:  +0.3 │ Mfg: +2.9%│   │
│   │  All-In: 16.8%   │ $1,671/HH   │            │          │           │   │
│   └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
│   ┌────────────────┐  ┌────────────────┐  ┌────────────────┐               │
│   │ T1 (Partners)  │  │ F1-F7 (Charts) │  │ T3 (Fiscal)    │               │
│   └────────────────┘  └────────────────┘  └────────────────┘               │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## NOTES FOR R CONVERSION

### Key Formulas to Replicate

1. **Weighted ETR:**
   ```r
   weighted_etr <- sum(country_etr * import_weights) / sum(import_weights)
   ```

2. **Price Effect:**
   ```r
   price_effect <- (etr * -usd_offset * goods_share * import_share) +
                   (etr * import_share * goods_share * (1 + passthrough))
   ```

3. **Revenue:**
   ```r
   gross_revenue <- new_duties - baseline_duties
   net_revenue <- gross_revenue * (1 - compliance_effect - income_effect)
   ```

4. **GDP Impact (Q4-Q4):**
   ```r
   gdp_impact <- ((tariff_gdp_q4 / tariff_gdp_q4_prior) /
                  (baseline_gdp_q4 / baseline_gdp_q4_prior) - 1) * 100
   ```

### External Model Dependencies

The following require external model runs that produce outputs to be integrated:
- **GTAP:** Sector output effects, foreign GDP effects, long-run price effects
- **MAUS:** Quarterly GDP, unemployment, employment projections

### Vestigial Elements to Ignore
- `__123Graph_*` named ranges (Lotus 1-2-3 legacy)
- External links to old CBO/Yale SharePoint files
- Sheets marked with `IGNORE>>>`

---

*Generated from analysis of ricco_tariffs_11-17.xlsm on 2026-01-05*
