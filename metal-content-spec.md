# Data-Driven Metal Content Shares for Section 232 Derivative Tariffs

## Specification for Implementation

**Author context:** This spec is for The Budget Lab at Yale (TBL). TBL already computes effective tariff rates (ETRs) at the 10-digit HTS level, maps HTS codes to GTAP sectors, and models macroeconomic and distributional effects of tariffs. The existing model applies Section 232 tariffs to the *full customs value* of derivative products. This project adds a data-driven adjustment for the fact that Section 232 tariffs on derivatives legally apply only to the metal content share of the product, not the full value.

---

## 1. Motivation

Since June 2025, the U.S. has imposed a 50% Section 232 tariff on steel, aluminum, and derivative products. For derivative products classified outside HTS Chapters 72/73 (steel) and 76 (aluminum), the tariff applies only to the *value of the steel or aluminum content*, not the full entered value. The non-metal portion of the product is instead subject to other applicable tariffs (IEEPA reciprocal, MFN, etc.).

This means the effective tariff burden of Section 232 on derivative products depends critically on the **metal content share** — the fraction of the product's customs value attributable to steel or aluminum. This share varies enormously across products (a steel trailer vs. an electronic appliance vs. a piece of furniture).

Currently:
- **TBL's upper bound** applies the 50% tariff to the full value of derivative imports.
- **TBL's lower bound** applies the 50% tariff to none of the value (equivalent to repeal).
- **Tax Policy Center's assumption** uses a flat 50% metal share for all derivatives.
- **This project** constructs product-group-varying metal content shares from Bureau of Economic Analysis (BEA) Input-Output data.

---

## 2. Conceptual Approach

The BEA's **direct requirements coefficients** from the Use tables tell us, for each industry, how many cents of each intermediate input are required per dollar of that industry's output. We extract the coefficients for primary metals inputs (steel and aluminum) to each downstream industry that produces derivative products. These coefficients serve as our estimate of the metal content share.

### Why direct requirements (not total requirements)?

The Section 232 tariff on derivatives is assessed on the value of the metal content *in the imported product*. This corresponds to the **direct** requirement — the steel/aluminum purchased as an input by the producing industry. Total requirements would additionally capture steel embedded in other intermediate inputs (e.g., steel in the engine of a bulldozer, where the engine itself is an intermediate input), which would overstate the dutiable metal content.

### Why this is a reasonable proxy

CBP's informal guidance defines the dutiable metal value as the entered value minus the cost of non-metal components, with fabrication/labor costs on the metal portion *not* deductible. The BEA direct requirements coefficient captures the value of metals purchased by the industry inclusive of the producer's margin on those metals, which is conceptually similar to CBP's approach (though not identical — the I-O coefficient is for domestic production, not imports specifically, and reflects average input mixes across all firms in the industry).

---

## 3. Data Required

### 3a. BEA Input-Output Tables

**Source:** Bureau of Economic Analysis, Annual Industry Accounts  
**URL:** https://www.bea.gov/data/industries/input-output-accounts-data  
**Alternative (BLS version with 2024 data):** https://www.bls.gov/emp/data/input-output-matrix.htm

**Specific table needed:** The **Use table (before redefinitions)** or equivalently the **direct requirements coefficients table**. Either the summary-level (71 industries) or the detailed-level (402 industries for benchmark years) version.

**What to extract:**

From the direct requirements matrix, for each **column industry** (the producing industry), extract the coefficient in the following **row commodities**:

| BEA Commodity Code | Description | Maps to |
|---|---|---|
| 3311 | Iron and steel mills and ferroalloy manufacturing | Steel content |
| 3312 | Steel product manufacturing from purchased steel | Steel content |
| 331313 | Alumina refining and primary aluminum production | Aluminum content |
| 331314 | Secondary smelting and alloying of aluminum | Aluminum content |
| 33131A | Aluminum product manufacturing (rolling, drawing, extruding) | Aluminum content |

At the summary level, these collapse into:
- **331** = Primary metals (includes both steel and aluminum — may need to use this if detailed split is unavailable in annual tables)

The **steel content share** for industry *j* is approximately:
```
steel_share_j = DR(3311, j) + DR(3312, j)
```

The **aluminum content share** for industry *j* is approximately:
```
aluminum_share_j = DR(331313, j) + DR(331314, j) + DR(33131A, j)
```

The **total metal content share** is:
```
metal_share_j = steel_share_j + aluminum_share_j
```

If only summary-level data (code 331) is available, this gives a combined primary metals share. This is less precise but still useful. The steel vs. aluminum split matters because (a) the tariff rates are the same (50%) for most countries, but (b) aluminum from unknown smelting origins faces 200%, and (c) analytically separating steel and aluminum exposure is valuable.

**Year:** Use the most recent available. BLS has 2024 data. BEA annual tables typically lag by ~1-2 years. Use 2022 or 2023 from BEA, or 2024 from BLS.

### 3b. Concordance: HTS Code → BEA Industry

**Purpose:** Map each 10-digit HTS code on the Section 232 derivative products list to a BEA producing industry, so that the appropriate metal content share can be applied.

**Existing TBL infrastructure:** TBL already maps HTS codes to GTAP sectors. A GTAP-to-BEA concordance is needed. Alternatively:

- **USITC maintains an HTS-to-NAICS concordance** (available at https://dataweb.usitc.gov/). NAICS codes map straightforwardly to BEA industry codes (BEA uses a modified NAICS classification).
- **BEA publishes a concordance** between I-O commodity codes and NAICS codes.
- **Census publishes an HTS-to-NAICS concordance** in their trade data products.

The mapping chain is: **HTS-10 → NAICS → BEA I-O industry code → metal content share from direct requirements**.

If multiple HTS codes map to the same BEA industry, they receive the same metal content share.

### 3c. List of Section 232 Derivative HTS Codes

**Purpose:** Identify which HTS codes are derivative products (as opposed to primary steel/aluminum in Chapters 72/73/76, where the tariff applies to the full value).

**Source:** The Federal Register notices published by BIS:
- February 18, 2025 (initial derivative lists under Proclamations 10895/10896)
- June 16, 2025 (additional steel derivatives under Proclamation 10947)
- August 19, 2025 (407 additional HTS codes added via inclusions process)

These are available at https://www.bis.gov/about-bis/bis-leadership-and-offices/SIES/section-232-investigations/section-232-steel-aluminum

TBL likely already has these lists as part of its existing HTS-level Section 232 parameterization.

### 3d. 2024 Import Values by HTS Code (already in TBL infrastructure)

**Purpose:** Weight the metal content shares when computing aggregate or sector-level effective tariff rates.

**Source:** USITC DataWeb or Census trade data. TBL already uses this.

---

## 4. Implementation Steps

### Step 1: Load the BEA direct requirements matrix

Download the Use table (direct requirements coefficients) from BEA or BLS. Parse it into a matrix where rows are input commodities and columns are producing industries.

### Step 2: Compute metal content shares by BEA industry

For each column industry *j*:
```
steel_share[j] = sum of direct requirements coefficients for steel input commodities (3311, 3312)
aluminum_share[j] = sum of direct requirements coefficients for aluminum input commodities (331313, 331314, 33131A)
metal_share[j] = steel_share[j] + aluminum_share[j]
```

### Step 3: Build the HTS → BEA concordance

Using the HTS-to-NAICS-to-BEA mapping chain, assign each derivative HTS code to a BEA industry.

### Step 4: Assign metal content shares to each derivative HTS code

Each derivative HTS code inherits the `metal_share` (and optionally `steel_share` and `aluminum_share`) of its mapped BEA industry.

For HTS codes in Chapters 72, 73, and 76 (primary steel/aluminum products), the metal content share is **1.0** (tariff applies to full value). These are not derivative products and should not be modified.

### Step 5: Compute adjusted effective tariff rates

For each derivative HTS code *h* from country *c*:

```
section_232_duty[h,c] = 0.50 * metal_share[industry(h)] * customs_value[h,c]
```

The non-metal portion is subject to other applicable tariffs:

```
other_duty[h,c] = ieepa_rate[c] * (1 - metal_share[industry(h)]) * customs_value[h,c]
```

Total duty on the product:
```
total_duty[h,c] = section_232_duty[h,c] + other_duty[h,c] + mfn_duty[h,c] + ...
```

This replaces the current treatment where `section_232_duty[h,c] = 0.50 * customs_value[h,c]` (i.e., metal_share = 1.0 for all derivatives).

### Step 6: Aggregate and report

Aggregate up to the level needed for TBL reporting: by GTAP sector, by country/region, and overall. Report the results as the "I-O adjusted" scenario alongside the upper bound (full value), TPC middle (flat 50%), and lower bound (zero).

---

## 5. Expected Output

A table/dataset with the following columns:

| Column | Description |
|---|---|
| `hts_10` | 10-digit HTS code |
| `bea_industry` | Mapped BEA I-O industry code |
| `bea_industry_desc` | Industry description |
| `steel_share` | Direct requirements coefficient for steel inputs |
| `aluminum_share` | Direct requirements coefficient for aluminum inputs |
| `metal_share` | Combined steel + aluminum share |
| `imports_2024` | 2024 customs value of imports (from existing TBL data) |
| `section_232_base` | Duty under current TBL methodology (full value) |
| `section_232_io_adjusted` | Duty under I-O metal share methodology |

---

## 6. Sensitivity and Caveats to Document

1. **Domestic production vs. import composition.** The I-O coefficients reflect the average input mix of U.S. domestic producers, not foreign producers of the same goods. Foreign production may use different metal intensities. This is an unavoidable limitation but should be flagged.

2. **Industry-level vs. product-level variation.** Metal content varies within BEA industries. A steel desk and a wooden desk with steel legs are in the same industry but have very different metal shares. The I-O approach captures between-industry variation but not within-industry variation.

3. **Value vs. cost concepts.** The I-O coefficient captures the *cost* of metal inputs to the producing industry. CBP assesses tariffs on the *value* of metal content as declared by the importer, which could differ (e.g., if there are markups on the metal component between the mill and the final product manufacturer). CBP's guidance says fabrication costs on the metal itself are *not* deductible, which would make the true dutiable share higher than the raw material input share. This means the I-O shares may be a **lower bound** on the true dutiable metal share.

4. **Chapter 73/76 products.** For products classified in HTS Chapters 73 (articles of iron/steel) and 76 (articles of aluminum), the tariff was initially assessed on the full value but was changed to apply only to metal content as of June 4, 2025 (Proclamation 10947). These products likely have very high metal content shares (often 80-100%). The I-O adjustment matters less for these and more for the derivative products in other chapters (84, 85, 87, 94, etc.).

5. **The flat 50% assumption as a benchmark.** The TPC's 50% assumption is simple but may significantly overstate metal content for electronics and electrical equipment and understate it for fabricated metals and metal furniture. Reporting the I-O shares alongside the 50% assumption will show where the assumption bites.

---

## 7. Key References

- BEA Input-Output Accounts: https://www.bea.gov/data/industries/input-output-accounts-data
- BLS I-O Tables (1997-2024): https://www.bls.gov/emp/data/input-output-matrix.htm
- BIS Section 232 Steel and Aluminum page (derivative product lists): https://www.bis.gov/about-bis/bis-leadership-and-offices/SIES/section-232-investigations/section-232-steel-aluminum
- USITC DataWeb (trade data by HTS): https://dataweb.usitc.gov/
- CBP Section 232 FAQs (valuation guidance): https://www.cbp.gov/trade/programs-administration/entry-summary/232-tariffs-aluminum-and-steel-faqs
- Global Trade Alert, "Why Section 232 behaves like a supply-chain regulation" (metal content calculus explainer): https://globaltradealert.org/reports/Section-232-Metal-Content-Calculus-Explained
- CBP Base Metals CEE informal valuation guidance (December 2025): see Mondaq summary at https://www.mondaq.com/unitedstates/international-trade-investment/1733968/
