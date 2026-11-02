# resources/treasury_etr — sources for the Treasury-derived effective tariff rates

Inputs to `build_treasury_etr.R` (logic in `src/treasury_etr.R`), which builds the
"Treasury-Derived Rates" figures in the State of Tariffs tool. `--refresh` re-pulls
the API snapshots; `RETRIEVED` records the date of the last pull.

| File | Contents | Source |
|---|---|---|
| `mts_customs_duties_monthly.csv` | Monthly customs duties, gross / refunds / net ($M), 2015-03 on | Monthly Treasury Statement, Table 4, classification "Customs Duties" — fiscaldata API `v1/accounting/mts/mts_table_4` |
| `census_imports_monthly.csv` | Monthly imports for consumption and general imports, customs value ($M), 2015-03 on | Census international trade API, `timeseries/intltrade/imports/hs` (`CON_VAL_MO`, `GEN_VAL_MO`) |
| `bea_nipa_goods_imports_annual.csv` | Annual imports of goods ($M) | BEA NIPA Table 4.1, line 19 ("Goods", imports) — BEA API |
| `hsus_ee429.csv` | Duties as a percent of imports, 1790–2000 (series Ee429 only) | *Historical Statistics of the United States, Millennial Edition*, Table Ee424-430 (Douglas A. Irwin), Cambridge University Press, 2006 |
| `published_etr_1982_2024.csv` | The 1982–2024 values published through April 2026 (validation target only) | Old `src/12_export_excel.R` `HISTORICAL_ETR` (commit `1d02fa9`) |

Pre-2015 net customs receipts come from `resources/calibration/treasury_revenue.csv`
(Haver `FTRU@GOVFIN`), which matches the MTS API to the dollar over their overlap.

## Annual series

- **1790–1981:** HSUS Ee429, used as published. The old series matched Ee429 exactly
  in all 192 years. Ee429 is labelled "as a percentage of total imports" but is
  computed as duties calculated (Ee428) over imports for consumption (Ee425); HSUS
  computed it from unrounded source figures, so it cannot be reproduced exactly
  from the rounded dollar columns.
  - 1790–1821: total imports and duties actually collected (without drawback
    remissions); not comparable to later years (HSUS documentation; Irwin 2003,
    *Journal of Economic History* 63: 506–13).
  - 1822–1866: net general imports; 1867 on: imports for consumption.
  - Years end September 30 (1790–1842) and June 30 (1843–1915); calendar years after.
  - "Duties calculated" is a statistical measure (rates applied to import totals),
    not Treasury collections.
- **1982 on:** calendar-year net customs receipts / NIPA imports of goods. This
  reproduces every published 1982–2024 value to within 0.015 percentage points.
- **Overlap:** HSUS Ee429 for 1982–2000 is kept as a separate series to show the
  definitional gap at the splice (−0.15 to +0.26 pp).
- Only full calendar years are included. 2026 is excluded until the year is
  complete; IEEPA refunds make 2026 net receipts much lower than gross.

## Monthly series

- Customs duties (gross or net) / Census imports for consumption, same month.
  Duties are paid roughly a month after entry, so same-month ratios blur timing.
- Starts 2015-03, the first month in the fiscaldata MTS API (Jan–Feb 2015 exist
  only as net receipts in the Haver series).
- From May 2026, refunds of IEEPA duties (after the Supreme Court ruling) push net
  receipts negative (May–July 2026); gross receipts are the default measure.

## Licensing

The raw HSUS workbook and documentation (`resources/historical_etr/raw/`) are
licensed for personal use and internal circulation only and are gitignored. Only
the derived Ee429 ratios are committed.
