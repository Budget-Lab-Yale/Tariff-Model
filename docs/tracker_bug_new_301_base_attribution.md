# Tracker bug report: new-301 duties attributed to `base` in `daily_by_authority`

> **RESOLVED upstream 2026-07-21.** The tracker republished
> `scenarios/new_301/daily/daily_by_authority.csv` in place (same vintage
> `2026-07-21-08`). The +3.0pp new-301 increment is now booked to `etr_301`
> (`section_301` = 4.651pp at tip), `etr_base` is back to the current-law level
> (1.224pp), and the authority buckets sum to 12.787pp = the overall ETR.
> Verified in the rebuilt dashboard (`2026-07-21-08-484f`, 13:43). Kept for the
> record; the analysis below describes the pre-fix state.

**Bundle:** Tariff-Rate-Tracker vintage `2026-07-21-08`
**Series:** `scenarios/new_301`
**File:** `scenarios/new_301/daily/daily_by_authority.csv`
**Reported by:** Tariff-Model dashboard (daily-rate-by-authority), 2026-07-21
**Severity:** decomposition only — overall ETR/revenue are correct; the authority
split is wrong.

## Symptom

In the `new_301` scenario, the new Section 301 actions (effective **2026-07-24**)
are booked to the **`etr_base` (MFN)** column instead of a Section 301 column
(`etr_301` / `etr_s301br`). The daily-rate-by-authority chart therefore shows the
new-301 policy as a jump in MFN/base rather than in Section 301.

## Evidence

Comparing `new_301` vs `actual` at the tip date (2026-12-31), etr in pct-points:

| column            | actual | new_301 | Δ (new_301 − actual) |
|-------------------|-------:|--------:|---------------------:|
| `etr_base` (MFN)  |  1.224 |   3.702 |            **+2.478** |
| `etr_301`         |  1.561 |   2.001 |               +0.440 |
| `etr_s301br`      |  0.073 |   0.109 |               +0.036 |
| `etr_232`         |  6.543 |   6.543 |                0.000 |

- Divergence begins **2026-07-24** (the new-301 effective date) and holds flat
  through the tip: `Δbase ≈ +2.48pp`, `Δ301 ≈ +0.44pp`.
- So ~85% of the new-301 policy increment lands in `base` rather than in a 301
  bucket. `etr_232`, `etr_s338`, etc. are unchanged, i.e. the misattribution is
  specifically base-vs-301.

## Expected

The Section 301 increment introduced by the `new_301` scenario should appear in
`etr_301` (or `etr_s301br` if it is a distinct sub-action), not in `etr_base`.
`etr_base` should stay at the current-law MFN level (~1.224pp), matching `actual`.

## Not a bug (verified)

- **Section 338** is handled correctly upstream: `etr_s338` exists (0.286pp in
  `actual`) and is properly zeroed in `scenarios/no_s338`. The Tariff-Model
  dashboard was dropping it — fixed on our side (mapping omission in
  `13_export_dashboard.R`), not a tracker issue.

## Downstream handling (Tariff-Model)

Per decision on 2026-07-21, we are **not** applying a stopgap reconciliation in
the dashboard export (a "base-delta = 301" heuristic is too fragile). The
daily-rate-by-authority chart for `new_301` will show base-heavy until the tracker
corrects the attribution; totals/revenue/ETR are unaffected.
