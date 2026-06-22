# How time works in this model

*A single-page map of the model's temporal logic — what "starts" when, which knob
controls it, and which outputs use the full time path vs. a single snapshot. Written
because this logic is currently spread across five files with no one place to see it.*

---

## TL;DR

The model **starts at the beginning of 2025** and walks through the **real historical
tariff path** day by day. Tariff rates are carried as a daily time series of *increases
over a pinned pre-tariff baseline* (~2.93%, the early-2025 MFN average). That time path
drives **revenue** and **macro**. A **single date** (`gtap_reference_date`, e.g.
`2026-09-29`) is sliced out and fed to the heavy structural model, so **prices, sector
output, and foreign GDP** are "long-run, as if that one date's tariffs were permanent."

Two consequences that trip people up:
- **Macro reports 2025** (GDP 2025 Q4-Q4 etc.); **revenue does not** — the revenue total
  is the FY2026–FY2035 budget window, so Jan–Sep 2025 collections (which fall in FY2025)
  are excluded. This is a deliberate convention, not a bug, and is a one-line change.
- **`eta` (the noncompliance factor) is time-invariant** — one number per
  (partner-group × sector), the same at every date.

---

## The actual timeline (from a real `2026-04-06` run)

The rate schedule is the daily `etr_level` series. For `2026-04-06` it spans
**2025-01-26 → 2026-12-31** (340 days of 2025, 365 of 2026):

```
2025-01-26   2.93%   baseline (pre-tariff MFN avg) — the "zero" every delta is measured from
2025-01-27   4.27%   first China IEEPA tariff
2025-02-04   6.30%
2025-04-02  12.23%   "Liberation Day" reciprocal tariffs
2025-04-05  19.72%   2025 peak
2025-04-14  11.63%   partial rollback
   ... (tracks every 2025 move) ...
2025-11-15  14.84%
2026-04-06  11.58%
2026-09-29   9.40%   last policy change — held flat from here on
2026-12-31   9.40%   schedule horizon (extrapolated)
```

Beyond 2026-12-31 the final rate (2026-09-29's level) is **held constant forward through
FY2035** for revenue (see `compute_fy_weighted_etr_increase`, "After the last date: last
etr_increase continues"). The scenario *name* ("2026-04-06") is a vintage label, not a
start date.

---

## The temporal knobs (what controls what, and where)

| Knob | Lives in | What it controls | Example |
|---|---|---|---|
| **`baseline_date`** | `model_params.yaml` → `rate_panel:` (new path) | The pinned pre-tariff snapshot that every rate is differenced against (the delta denominator, ~2.93%). | `2025-01-01` |
| **the rate schedule** (`etr_increase_by_date`) | produced upstream; the full daily ETR path | The historical + scenario tariff path. Drives revenue (day-weighted) and macro (sampled). | 2025-01-26 → 2026-12-31 |
| **`gtap_reference_date`** | `model_params.yaml` | The **single** date whose tariff snapshot is fed to GTAP + the I-O price model. Everything "structural / long-run" is as-of this date. | `2026-09-29` |
| **`usmm_dates`** | `model_params.yaml` | The handful of dates the macro decomposition samples the path at (keeps it to a few components). | `[2026-02-24, 2026-04-06, 2026-07-24, 2026-09-29]` |
| **`temp_component_dates`** | `model_params.yaml` | The one window `[onset, offset]` treated as a *temporary* tariff (uses the temp IRF, expectations channel) instead of a permanent step. | `[2026-02-24, 2026-07-24]` |
| **IRF onset quarters** | `05a_usmm_surrogate.R` constants | When each macro impulse-response family starts in calendar time. **perm: 2025Q2** (`USMM_SHOCK_QUARTER=6`); **temp: 2026Q1** (`USMM_TEMP_SHOCK_QUARTER=9`). | (fixed quarters) |
| **revenue FY window** | `04_calculate_revenue.R:246` | Which fiscal years enter the headline total. | `FY2026–FY2035` |

> US fiscal year convention everywhere: **FY*N* = Oct 1 of *N−1* → Sep 30 of *N*.** So
> FY2026 = Oct 2025 → Sep 2026. Oct–Dec 2025 tariffs are FY2026; Jan–Sep 2025 are FY2025.

---

## Which outputs use the full time path vs. the single snapshot

This is the split that causes the most confusion. **Not every output sees the whole
schedule.**

| Output block | Time basis |
|---|---|
| **Conventional revenue** | **Full daily path**, day-weighted per fiscal year. (The GTAP behavioral *level* — the substitution/quantity response — comes from the single `gtap_reference_date` solve, then is scaled across years by the time path.) |
| **Macro** (GDP, unemployment, PCE, payroll, fed funds) | **Full path**, sampled at `usmm_dates`, decomposed into permanent + temporary + refund impulses. |
| **Dynamic revenue** | Blends the macro (time-path) and GTAP (reference-date) responses over 16 quarters. |
| **Prices** (pre-sub, PE post-sub, GE) | **Single `gtap_reference_date` snapshot** — long-run, as-if-permanent. |
| **Sector output** | **Single `gtap_reference_date` snapshot.** |
| **Foreign GDP** | **Single `gtap_reference_date` snapshot** (long-run GE). |
| **Distribution** | Derived from the price snapshot. |

So: a price effect of "X%" means "the long-run consumer price impact if the
2026-09-29 tariff schedule were permanent" — *not* an average over 2025–2026. Revenue and
macro, by contrast, integrate the whole bumpy path.

---

## The 2025 question, precisely

- **Rate schedule:** includes all of 2025 (starts 2025-01-26). ✔
- **Macro:** responds from 2025Q2; the model reports GDP/U-rate/PCE for 2025 Q4-Q4. ✔
- **Revenue:** the CBO baseline file *has* FY2025, and the code *computes* FY2025
  revenue — but the final total is filtered to **`fiscal_year >= 2026`**, so FY2025
  (≈ the Jan–Sep 2025 tariff collections) is **dropped** from the headline. This is the
  standard forward-looking budget-window convention.
  - To **include FY2025**: remove the `>= 2026` floor at `04_calculate_revenue.R:246`.
    FY2026 already includes the Oct–Dec 2025 collections regardless.

---

## eta (noncompliance) and time

`eta` is **time-invariant**: `eta_by_partner_gtap.csv` is keyed by `(eta_group, gtap_code)`
only — no date. The same haircut applies in 2025 and 2026.

It is, however, **per-sector**. Because the model tariffs *different products on different
dates*, the import-weighted *average* haircut across all currently-tariffed goods drifts
over time even though no single sector's `eta` changes. (This is the only mechanism by
which the noncompliance-adjusted rate path can have a different *shape* than the
statutory path.)

`eta` is only active when a scenario supplies a granular eta file
(`noncompliance_active = TRUE`, the new rate-panel scenarios). Legacy `tariff_etrs`
scenarios (e.g. `2026-04-06`) run with `eta = 1` (statutory = adjusted) on the structural
side and apply only the flat `compliance_effect` (0.10) haircut to revenue.

---

## Known time-related gotchas (see the model review for detail)

- **IRF onset split** (`05a`): perm and temp impulse-responses start in different quarters
  (2025Q2 vs 2026Q1). Fixed; a `validate_irf_onsets()` assertion now hard-stops if a
  re-extracted IRF vintage breaks the assumption.
- **FY-window edge** (`04_calculate_revenue.R:64-71`): if a scenario's *first* tariff date
  is *after* a budget-window FY start, the pre-policy days wrongly get the first regime's
  rate instead of 0. Does **not** affect schedules that start in early 2025 (like
  `2026-04-06`); does affect scenarios whose schedule starts at 2026-01-01.
- **Daily-output horizon** (`11_write_outputs.R:464`): the daily ETR CSV is hardcoded to
  end 2026-12-31; 2027+ policy dates would be silently truncated.
