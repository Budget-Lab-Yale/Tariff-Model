# treasury_revenue.csv — source & citation

Monthly U.S. customs-duty collections and goods-import value, **1982-01 through
2026-03**. This pipeline consumes **only** the monthly ratio
`customs_duties / imports_value` (the effective tariff rate) to Treasury-calibrate
the Census-shaped etas — see `code/01_data.R` Section 4/5 and `code/02_analysis.R`.

## Provenance

Stored verbatim from the Budget Lab Tariff Impact Tracker:

- **Repository:** `Budget-Lab-Yale/tariff-impact-tracker` (GitHub)
- **File:** `output/tariff_revenue.csv`
- **Commit:** `f71486492a8f5075fa74cd400e7af164cb87f3d4` (2026-05-01)
- **Retrieved:** 2026-06-07
- **URL:** https://raw.githubusercontent.com/Budget-Lab-Yale/tariff-impact-tracker/main/output/tariff_revenue.csv

## Underlying series (per the tracker's README)

| Column | Series | Source |
|---|---|---|
| `customs_duties` | `FTRU@GOVFIN` — federal customs-duty revenue ($M, monthly) | U.S. Treasury, via Haver Analytics |
| `imports_value` | `TMMCN@USINT` — total merchandise imports at customs value ($M, monthly) | Census Bureau, via Haver Analytics |
| `effective_rate` | `100 × customs_duties / imports_value` (percent) | derived; recomputed downstream, column is informational |

The Treasury/Census inputs are distributed through Haver Analytics (proprietary).
This file is a stored snapshot of Budget Lab's already-assembled public output,
committed here so the calibration is reproducible without a Haver license or a
runtime network pull.

**Why a stored snapshot and not a live FRED rebuild:** the seasonal-adjustment
factors in this series are specific to the Haver/Budget Lab assembly. Reconstructing
the series from a free API (e.g. FRED) would apply different seasonal factors,
shifting the monthly `customs_duties / imports_value` ratios — and therefore moving
*every* calibrated eta. The snapshot is the authoritative input.

## Refreshing

Replace this file with a newer `output/tariff_revenue.csv` from the tracker (a
local checkout or the GitHub URL above), then update the **Commit** and
**Retrieved** lines here. Required columns: `date`, `customs_duties`,
`imports_value` (plus `effective_rate`, `year`, `month`, which are carried
along but not required by the pipeline).
