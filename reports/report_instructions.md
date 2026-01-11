# State of Tariffs Report Generation Instructions

This document provides instructions for generating the Yale Budget Lab "State of Tariffs" report from model outputs.

## Overview

The report summarizes the effects of US tariff policy as of a specific date. It presents results for two scenarios:
1. **Baseline** — Current tariff policies remain in effect indefinitely
2. **IEEPA Invalidation** — IEEPA tariffs are struck down by the Supreme Court and refunded

The baseline scenario is the primary focus; the IEEPA invalidation scenario is presented in italicized parentheticals throughout.

## Input Data

You will receive:

1. **`key_results.csv`** — Primary metrics for the report
2. **`macro_quarterly.csv`** — Quarterly GDP, employment, and unemployment projections
3. **`sector_effects.csv`** — Long-run output changes by sector
4. **`distribution.csv`** — Cost by income decile
5. **`dynamic_revenue_by_year.csv`** — Fiscal effects by year
6. **`product_prices.csv`** — Price effects by GTAP sector
7. **`foreign_gdp.csv`** — Long-run GDP effects by country/region
8. **Previous report** — For "Changes Since Last Report" section and style reference
9. **Policy context** — Description of what changed since the last report

### Key Results Fields

| Field | Description | Unit |
|-------|-------------|------|
| `pre_sub_etr_increase` | ETR increase before substitution | percentage points |
| `post_sub_etr_increase` | ETR increase after substitution | percentage points |
| `pre_sub_price_increase` | Price level increase (short-run) | percent |
| `post_sub_price_increase` | Price level increase (long-run) | percent |
| `pre_sub_per_hh_cost` | Cost per household (pre-substitution) | dollars |
| `post_sub_per_hh_cost` | Cost per household (post-substitution) | dollars |
| `gross_revenue_10yr` | Gross tariff revenue 2026-35 | billions |
| `conventional_revenue_10yr` | Net conventional revenue 2026-35 | billions |
| `dynamic_revenue_10yr` | Net dynamic revenue 2026-35 | billions |
| `dynamic_effect_10yr` | Revenue lost to slower growth | billions |
| `gdp_2025_q4q4` | GDP growth impact in 2025 | percentage points |
| `gdp_2026_q4q4` | GDP growth impact in 2026 | percentage points |
| `urate_2025_q4` | Unemployment rate increase by Q4 2025 | percentage points |
| `urate_2026_q4` | Unemployment rate increase by Q4 2026 | percentage points |
| `payroll_2025_q4` | Payroll employment change by Q4 2025 | thousands |
| `payroll_2026_q4` | Payroll employment change by Q4 2026 | thousands |

---

## Report Structure

### 1. Title

Format: `State of Tariffs: [Month] [Day], [Year]`

Example: `State of Tariffs: November 17, 2025`

### 2. Summary Box

A shaded box containing bullet points for each major finding. Each bullet has:
- **Bold header** (e.g., "Current Tariff Rate:")
- Main finding for baseline scenario
- Italicized parenthetical for IEEPA invalidation scenario (if comparison provided)

#### Summary Bullets

**Current Tariff Rate:**
- Report pre-substitution ETR as the "headline" rate consumers face
- Note historical comparison (e.g., "highest since 1935")
- Report post-substitution ETR as what the rate "will be" after consumption shifts
- IEEPA parenthetical: report the ex-IEEPA pre-substitution rate

**Overall Price Level & Distributional Effects:**
- Brief note that TBL assumes Fed "looks through" tariffs (prices rise, not nominal incomes fall)
- Report pre-substitution price increase percentage
- Report per-household cost (use `pre_sub_per_hh_cost`, round to nearest $100)
- Report cost for bottom decile (from `distribution.csv`, decile 1)
- IEEPA parenthetical: report ex-IEEPA price level impact

**Commodity Prices:**
- Identify the 3-4 product categories with highest price impacts (from `product_prices.csv`)
- Describe in plain English (e.g., "apparel", "motor vehicles", "electrical equipment")
- IEEPA parenthetical: note which categories would see relief

**Real GDP Effects:**
- Report GDP growth slowdown in 2025 and 2026 (from `gdp_2025_q4q4`, `gdp_2026_q4q4`)
- Report long-run GDP level effect (from `sector_effects.csv`, `overall_gdp` row)
- Convert to dollar terms (multiply by ~$29 trillion baseline GDP)
- IEEPA parenthetical: report ex-IEEPA long-run GDP effect

**Labor Market Effects:**
- Report unemployment rate increase by end of 2025 and 2026
- Report payroll employment change by end of 2025 (in thousands, convert to "about X")
- IEEPA parenthetical: note reduced employment impact

**Long-Run Sectoral GDP & Employment Effects:**
- Report manufacturing output expansion
- Report the sectors that contract most (typically construction, agriculture)
- Note the trade-off framing: manufacturing gains are "more than crowded out"
- IEEPA parenthetical: note patterns are similar

**Fiscal Effects:**
- Report gross revenue over 10-year window (round to $X.X trillion)
- Report dynamic revenue (after GDP feedback effects)
- IEEPA parenthetical: note revenue would be cut roughly in half

### 3. Changes Since the Last Report

This section requires comparing current results to the previous report. Structure:

**New Policy.** Describe policy changes since the last report date:
- New tariff announcements
- Exemptions granted
- Trade deals announced
- Note which changes are/aren't reflected in this report

**Refinements to Methodology.** (If applicable)
- Changes to ETR calculation methodology
- Data updates
- Model improvements

### 4. Current Tariff Policy Tables

Two tables summarizing current policy:

**Broad Tariffs under IEEPA Authority**
| Region | "Reciprocal" Tariff | "Fentanyl" Tariff |
|--------|---------------------|-------------------|
| Canada | ... | ... |
| Mexico | ... | ... |
| China | ... | ... |
| etc. | | |

**Product-Specific Tariffs under Section 232 Authority**
| Sector/Product | Current Tariff Rate |
|----------------|---------------------|
| Steel | ... |
| Aluminum | ... |
| etc. | |

These tables require manual policy input — they describe the statutory rates, not the model outputs.

### 5. Results Section

Detailed exposition of results, organized by topic:

#### Average Effective Tariff Rate
- Explain pre- vs post-substitution distinction
- Report pre-substitution ETR increase and total rate
- Historical comparison
- Report post-substitution ETR and total rate
- Different historical comparison if applicable

#### Average Aggregate Price Impact
- Report pre-substitution price increase
- Explain assumption about Fed accommodation
- Report per-household cost
- Report post-substitution price increase and per-household cost

#### US Real GDP & Labor Market Effects
- Report GDP growth impacts for 2025 and 2026
- Report unemployment and employment changes
- Report long-run GDP level effect
- Report export decline percentage if available

#### Long-Run US Sectoral Output & Employment Effects
- Discuss sector-by-sector effects from `sector_effects.csv`
- Include: manufacturing (with durable/nondurable/advanced breakdown), construction, agriculture, mining, services, utilities
- Emphasize the manufacturing vs. rest-of-economy trade-off

#### Global Long-Run Real GDP Effects
- Report world GDP effect
- Report effects on major trading partners:
  - China
  - Canada
  - Mexico
  - EU
  - UK
- Note which countries gain vs lose
- Explain the pattern (countries with high tariff exposure lose, others may gain from trade diversion)

#### Fiscal Impact
- Report conventional 10-year revenue (gross tariff collections)
- Explain dynamic scoring: slower GDP growth reduces other tax revenues
- Report dynamic effect (revenue lost to slower growth)
- Report net dynamic revenue
- Express in trillions, rounded to one decimal

#### Distributional Impact
- Explain methodology: comparing consumption burden to income by decile
- Note that tariffs are regressive because lower-income households spend more of their income
- Report burden as % of income for bottom decile vs top decile
- Report dollar cost for bottom decile vs top decile
- Report median household cost

#### Commodity Price Effects
- Report overall short-run and long-run price level increases
- Highlight specific product categories with largest impacts:
  - **Apparel & Leather**: leather products (shoes, handbags), apparel, textiles
  - **Metals & Electronics**: electrical equipment, consumer electronics, metal products
  - **Vehicles**: motor vehicles (include dollar equivalent for average new car)
  - **Food**: food price increase percentage
- Use both short-run (pre-substitution) and long-run (post-substitution) figures
- Translate GTAP sector codes to plain English:
  - `lea` = leather products (shoes, handbags)
  - `wap` = wearing apparel
  - `tex` = textiles
  - `ele` = electrical equipment
  - `eeq` = electronic equipment
  - `fmp` = fabricated metal products
  - `mvh` = motor vehicles
  - `nfm` = non-ferrous metals

---

## Style Guidelines

### Tone
- Analytical and neutral
- Avoid advocacy language
- Present findings as estimates, not certainties

### Numbers
- Round ETRs to one decimal place (e.g., 16.8%, not 16.78%)
- Round dollar amounts: per-household to nearest $100, aggregates to one decimal in billions/trillions
- Round percentage points to one decimal
- Use "about" or "approximately" for rounded figures

### Formatting
- Use bold for metric headers in summary bullets
- Use italics for IEEPA invalidation parentheticals
- Percentage points abbreviated as "pp" in prose, spelled out in headers
- Use en-dashes for ranges (not hyphens)

### Historical Comparisons
The ETR historical comparison depends on the rate:
- If pre-sub ETR > 22%: "highest since 1932" (Smoot-Hawley peak)
- If pre-sub ETR 18-22%: "highest since 1933"
- If pre-sub ETR 15-18%: "highest since 1935"
- If pre-sub ETR 12-15%: "highest since 1939"
- If pre-sub ETR 10-12%: "highest since the 1940s"

(You may need to look up exact historical rates for precision.)

### Phrases to Use
- "The Budget Lab (TBL) estimates..."
- "consumers face an overall average effective tariff rate of..."
- "the equivalent of..."
- "in the short run" / "in the long run"
- "pre-substitution" / "post-substitution"

### Phrases to Avoid
- "skyrocket", "plunge", or other dramatic verbs
- "will" for uncertain outcomes (use "would" or present tense)
- First person ("we estimate" → "TBL estimates")

---

## Output Format

Generate the report as Markdown with the following structure:

```markdown
# State of Tariffs: [Date]

[Summary box content - use blockquote or table formatting]

## Changes Since the Last Report

[Content]

## Current Tariff Policy as of [Date]

[Tables]

## Results

### Average Effective Tariff Rate
[Content]

### Average Aggregate Price Impact
[Content]

### US Real GDP & Labor Market Effects
[Content]

### Long-Run US Sectoral Output & Employment Effects
[Content]

### Global Long-Run Real GDP Effects
[Content]

### Fiscal Impact
[Content]

### Distributional Impact
[Content]

### Commodity Price Effects
[Content]
```

The Markdown will be converted to Word using Pandoc with a reference document for Yale Budget Lab styling.
