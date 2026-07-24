# Eta calibration review — investigation notes (2026-07-23)

Working session reviewing the eta (and, at the edges, alpha) calibration, prompted
by a colleague flagging that the model implies a **post-substitution ETR of ~9%**
during the §122 era while **April/May 2026 data show ~7%**.

Everything below was computed on Slurm reading the calibration `panel` at
HS10×country grain. Scripts + outputs live in
`/nfs/roberts/scratch/pi_nrs36/jar335/calib_ladder/` (see "Reproduction" at the end).

---

## TL;DR — the three takeaways (with the precise version of each)

**(1) We under-weight recent months (their weight is literally 0), which biases us
high.** The eta training window is fixed at **2025-06 … 2026-02** and gives *zero*
weight to the post-ruling months (Mar–May 2026). The compliance wedge is **larger**
in the recent (post-IEEPA/§122) months than in the window, so the fitted eta is
**too small**, so the modeled **collected rate reads too high**. (Note the
direction: it's not that eta is over-estimated — eta is *under*-estimated, which
pushes the *ETR* up.) Refitting on a purely recent window cut the out-of-sample
over-prediction for May from +0.96pp to +0.22pp. Real, but modest (~0.3–0.5pp) and
with a bias–variance cost, since "recent" is only 1–2 months of data → the honest
fix is **recency-weighting**, not just sliding the window.

**(2) Our number is a Treasury concept; the critics are citing a Census concept,
which is structurally lower.** The eta level is pinned (after stripping AD/CVD and
de-minimis) to **Treasury** collections. Over the whole window Treasury ran
**above** Census-implied every single month (pooled 10.61% vs 9.80%). So the model
is *designed* to sit above a Census-implied rate. And **Treasury MTS isn't cleanly
available for the post-IEEPA months** (the committed series ends 2026-03), so for
Apr/May we're effectively *projecting the Treasury concept forward* and comparing it
to critics' *Census* observations — which are structurally ~0.5–0.8pp lower. So
"we're higher" is partly definitional, not a miscalibration.

**(3) The alpha/GTAP piece is still unresolved here** — and it's genuinely a
*separate* step from everything above. Notes on what we do/don't know are in the
"Alpha / GTAP — open" section. Short version: alpha only touches the near-term
*revenue* transition, not the published post-sub ETR; GTAP *overshoots* the trade
shift on the training window (so alpha<1 is correct) but *flips* to slight
undershoot right at the Feb-2026 regime break. Needs its own session.

**Overall lever ranking on out-of-sample ETR bias (vs Census):**
Treasury pin ≈ **0.4–0.5pp** > window/recency ≈ **0.3–0.5pp** > aggregation grain ≈ **0.1pp**.
With a sensible estimator (two-way, no pin) the eta predicts the Census collected
ETR **within ~0.5pp out of sample** — it is *not badly broken*. The rest of the way
to the published ~9% is **downstream of eta** (end-2026 reference basket, GTAP,
eta-applied-to-the-delta-only).

---

## How eta is calculated and applied (so the above makes sense)

**What it is.** A compliance/exemption wedge: `eta = 1 − (collected rate)/(statutory
rate)`. It absorbs everything that makes realized < statutory (exemptions, USMCA
claiming, evasion, and — importantly — any tracker statutory *mismeasurement*)
without taking a stand on which. Its one assumption: the wedge **persists over
time** (it's used in the forecast).

**Calibration (`src/calibration/calibrate_eta.R`).**
- **Shape:** two-way, multiplicatively separable WLS, `log(1−eta) = a_country +
  b_HS2`, weighted by statutory revenue. (A richer full-interaction spec exists but
  is *not* shipped.)
- **Level:** one scalar `k` (`solve_k_pos`) rescales positive-eta cells so the
  calibrated aggregate hits the **Treasury** collected ETR — after AD/CVD and
  de-minimis are stripped from the Treasury target.
- **Grain shipped:** aggregated to **(eta_group × GTAP sector)** — 8 partner groups
  × ~GTAP sectors — `eta_by_partner_gtap.csv` (~312 rows).

**Application (`src/00a_prepare_rate_inputs.R::apply_eta_prime`).** The model reads
`eta_prime = 1 − eta` and applies it **only to the policy delta**, not the baseline
tariff:
```
level_b = (level − delta) + eta_prime · delta
```
So it's a purely multiplicative haircut on the *incremental* (Trump-era) tariff,
one fixed factor per (GTAP sector × partner group), the same for every date. Two
consequences: composition shifts across cells are captured for free (weights are
data), but a *within-cell* change in the true wedge is invisible to it.

---

## Descriptive facts — the three-rung ladder (7-21 series)

For each month, on the matched HS10×cty set: statutory on the 2024 basket →
[eta gap] → realized on the 2024 basket → [trade-shift gap] → realized on the
current basket. `eta_agg = 1 − realized/statutory @ 2024 weights` (trade-weighted).

| month | stat@2024 | real@2024 | real@current | eta_agg |
|---|---:|---:|---:|---:|
| 2025-06 | 13.3% | 12.1% | 9.3% | 8.9% |
| … (window) | ~13–16% | ~12–14% | ~9–11% | ~5–18% |
| 2026-02 | 12.0% | 11.3% | 8.6% | 5.6% |
| **2026-03** | 10.3% | 9.0% | **6.8%** | 13.0% |
| **2026-04** | 11.2% | 9.2% | **6.7%** | 17.6% |
| **2026-05** | 11.4% | 9.2% | **6.9%** | 19.0% |

- The **7% is real** and is the current-basket *collected* (Census) rate. It does
  not depend on the tracker vintage.
- The trade-shift leg is a stable ~2.3pp (importers moving toward lower-duty cells);
  the eta leg ~1.6–1.8pp.

---

## Decompositions

**Eta: composition vs drift** (statutory-rev weighted, partner_group × HS2 = shipped
grain). Freeze the training-window wedge, apply to each month's weights:
- Spring (Apr–May): realized eta **25.5%**, frozen-predicted **14.9%**, **drift +10.7pp**.
- Of the ~9.7pp rise from window to spring, only ~1.8pp is composition; ~7.9pp is drift.
- **Caveat that turned out to matter a lot:** this is a *ratio* metric,
  statutory-revenue-weighted. In **ETR-level** terms (what the model outputs) it's
  only ~0.6–1.0pp, because the drift concentrates on **low-statutory** cells. (≈ 10pp
  eta-ratio miss × ~11% statutory base ≈ 1pp ETR.) Don't quote the +10.7pp as if
  it's an ETR error.

**Product:** the spring drift is **almost entirely Chapter 84 (machinery/computers)**:
realized wedge **3.9% → 43.3%** on a flat ~7% statutory (the electronics/semiconductor
carve-out). Chapter 85 (electrical) same sign. §232 metals (72/73/76) drift the
*other* way (schedule now over-predicts their exemption — metals are locked in).
Pharma (30) shows a **98.6%** realized wedge (see pharma note).

**Country:** biggest drift in Mexico / ROW / Japan / China — but these are largely
the *same cells* as the product story (machinery/electronics sourced from Asia +
Mexican assembly). USMCA (Canada/Mexico claiming) is a smaller, mostly-plateaued,
mostly-historical effect and is **not** a live lever — de-prioritized.

---

## Grain experiment — does finer aggregation help out-of-sample?

Window fixed, one estimator (frozen empirical wedge + fallback), only grain varies.
Metric: OOS prediction of the collected ETR on the actual basket, months 03/04/05.

| grain | OOS mean\|err\| |
|---|---:|
| E country × HS6 (fine both) | 0.59pp |
| D partner × HS6 (fine product) | 0.60pp |
| A partner × GTAP (≈ shipped grain) | 0.64pp |
| B partner × HS2 (baseline) | 0.68pp |
| C country × HS2 (fine country) | 0.69pp |
| SHIPPED (two-way + Treasury pin) | 0.96pp |

**Aggregation is a ~0.1pp lever for this ETR.** Refining *product* (HS2→HS6) buys
~0.08pp; refining *country* buys ~nothing. Directionally consistent with the
machinery finding (product axis matters) but tiny in magnitude — because the
composition shifts sit on low-statutory machinery. (Episode-specific: a shock on
high-statutory cells would make grain matter more.) The theoretical case for
"estimate at the finest grain overfitting allows" is sound; it just isn't where the
money is *this* time.

---

## Window + estimator experiment — where the money actually is

**(A) Window move** (partner×HS2 empirical, no pin; bias = pred − actual):

| test month | old (Jun25–Feb26) | +append post-ruling | rolling recent 6mo | pure post-ruling |
|---|---:|---:|---:|---:|
| Apr (act 6.74%) | +0.81 | +0.77 | +0.93 | +0.52 *(1mo)* |
| May (act 6.89%) | +0.96 | +0.79 | +0.78 | +0.22 *(2mo)* |

Appending recent months to the 9-month window barely helps (old months dominate);
a *pure* recent window nearly kills the bias but on 1–2 months of data → argues for
**recency-weighting**.

**(B) Estimator / Treasury pin** (fixed old window):

| estimator | OOS mean\|err\| | bias |
|---|---:|---:|
| two-way shape, **NO** pin | **0.54pp** | +0.54 |
| empirical partner×HS2 | 0.68pp | +0.68 |
| SHIPPED (two-way + pin → GTAP) | 0.96pp | +0.96 |
| two-way shape, **WITH** pin (raw Treasury) | 1.24pp | +1.24 |

Treasury train ETR **10.61%** vs Census **9.80%**; pin `k = 1.117` scales eta *down*
to hit Treasury → over-predicts vs Census. The two-way *shape* is good (beats
empirical); the *pin* is what hurts. **True pin effect ≈ 0.4pp** (shipped +0.96 vs
no-pin +0.54); the raw-Treasury +1.24 *overstated* it because the shipped pipeline
already strips AD/CVD + de-minimis from the Treasury target.

---

## Treasury vs Census — the structural basis gap

Treasury (actual customs cash receipts, incl. AD/CVD) runs above Census (estimated
calc-duty on the month's goods) **every window month**:

| month | Treasury | Census | gap |
|---|---:|---:|---:|
| 2025-08 | 11.2% | 10.5% | +0.7 |
| 2025-09 | 10.7% | 10.5% | +0.2 |
| 2025-10 | 11.4% | 10.8% | +0.6 |
| 2025-11 | 11.8% | 9.7% | +2.0 |
| 2025-12 | 9.9% | 9.3% | +0.6 |
| 2026-01 | 10.6% | 9.8% | +0.8 |
| 2026-02 | 10.5% | 8.6% | +1.9 |
| 2026-03 | 7.3% | 6.8% | +0.5 |

Reasons: different constructs (receipts vs estimated calc-duty), AD/CVD in Treasury
but not Census calc-duty, receipt timing (Nov/Feb spikes look like lumpy deposits),
different denominators. The shipped calibration **strips AD/CVD + de-minimis** from
the Treasury target before pinning, so the *residual* basis the pin actually chases
is smaller than the raw ~0.8pp. **Treasury MTS committed series ends 2026-03**, so
Apr/May Treasury isn't available — we project the concept forward.

---

## The vintage episode (resolved, no damage)

Initial runs used the **stale** `tracker_actual_2026-06-25` scenario (grabbed for a
cached panel). The **live** production scenario is **`tracker_actual_2026-07-21`**
(vintage `2026-07-21-08`, = tracker `latest`), and it already ships
7-21-calibrated etas. The 06-25 and 07-21 shipped etas are ~identical (mean 0.244 vs
0.245). Re-running on 7-21: collected rates identical (Census, vintage-independent);
newer bundle revised recent *statutory* up slightly, so the eta drift reads a touch
larger — the vintage was never the disease. (The interface `default_vintage` in
`interface_versions.yaml` is stale at 2026-06-25-14, but live scenarios pin their
own vintage, so production isn't stuck.) All final numbers above are on **7-21**.

---

## Pharma — flagged, being chased separately (John)

Chapter-30 statutory jumps 7.5% → 11.2% (06-25) / 13.0% (07-21) only at the
`valid_from=2026-09-29` snapshot — the September pharma §232, correctly **excluded**
from the calibration months by day-weighting. **But**: even in the calibration
window, ch30 carries a `rate_232` charge (~7.3%) with `metal_share = 100` — bizarre
for pharmaceuticals — while Census collects ~0 (98.6% realized wedge). So the tracker
statutory on pharma looks over-stated and eta is laundering it as noncompliance.
Separately, the GTAP crosswalk maps ch30 (pharma) to sector **BPH**, which *also*
absorbs ~80 HS6 lines of **ch29 (organic chemicals)** — so the shipped pharma eta is
a pharma+chemicals blend (confirms the "chemicals rolled into pharma" hunch). The
decisive check is the **HS10 teardown of ch84 + ch30** (tracker statutory-by-program
vs Census realized, line by line) — **not yet run**.

---

## Alpha / GTAP — open, needs its own session (takeaway #3)

This is the *second* calibration step and is largely independent of the eta
(statutory-adjustment) work above. What we established:
- **Alpha only feeds the near-term *revenue* transition** (`04_calculate_revenue.R`).
  The published **post-sub ETR is full GTAP** — alpha never touches it.
- On the training window, **GTAP over-predicts the trade shift** (its implied ETR
  sits *below* observed every month), so **alpha < 1** (within 0.58, between 0.43)
  correctly damps it. This matches John's prior mental model.
- The over/undershoot **flips at the regime break**: obs vs GTAP converge Feb 2026,
  and in **March GTAP *under*-shoots** (GTAP 9.0% > obs 8.3%) — so the sign is
  **regime-breadth dependent**, and the alpha fit (like the eta window) may not
  transfer cleanly into the narrower §122 world. `weighted_etr_ladder.csv` has the
  monthly obs/gtap/alpha series.
- **Still confusing / open:** how alpha/GTAP should interact with the eta findings
  here, and whether the March flip warrants re-fitting alpha on the post-ruling
  regime (would need a GTAP solve on Apr/May, which is cheap). Deferred.

---

## Open questions / suggested next steps

1. **Benchmark — RESOLVED: it's Census** (John, confident), *not* Treasury. So the
   "if Treasury, it dissolves" escape hatch is CLOSED. The basis point (#2) is a real
   but *partial* defense. Decomposition of the ~2pp (model ~9% vs Census ~7%):
   ~0.4pp Treasury pin (basis, defensible) + ~0.6pp recency/zero-weight-on-recent
   (ours, fixable, = takeaway #1) + ~0.2pp irreducible eta residual + ~0.9pp
   **downstream, not eta at all** (end-2026 reference basket + GTAP + eta-on-delta).
   The biggest chunk is downstream and is partly a **date/policy-state mismatch**:
   the published number is an end-2026 *reference* state compared to *spot*
   collections. Own the whole decomposition rather than leading with "it's just
   basis." **Quantifying the ~0.9pp downstream stack is now the top open item.**
2. **Recency-weighting** experiment: instead of sliding the window, down-weight old
   months (exponential decay?) and re-score OOS. Expected ~0.3–0.5pp, watch variance.
3. **Treasury vs Census vs Treasury−AD/CVD** monthly decomposition: how much of the
   ~0.8pp gap is AD/CVD (already stripped) vs a residual the pin genuinely chases →
   informs whether to anchor eta to Treasury or Census at all.
4. **HS10 teardown of ch84 + ch30** (tracker statutory-by-program vs Census realized):
   settles machinery aggregation-vs-real and the pharma statutory question.
5. **Quantify the downstream stack** (end-2026 reference basket + GTAP + delta-only
   application) so the full 9→7 gap is accounted end to end.
6. **Alpha/GTAP** re-fit question given the March flip (its own session).

---

## Reproduction

Scenario: **`tracker_actual_2026-07-21`** (tracker vintage `2026-07-21-08`).
Working dir: `/nfs/roberts/scratch/pi_nrs36/jar335/calib_ladder/`.

| script | what | key output |
|---|---|---|
| `ladder.R` | three-rung descriptive ladder | `ladder_monthly.csv`, `ladder_by_partner_monthly.csv` |
| `decomp.R` | eta composition-vs-drift | `eta_decomp_monthly.csv` |
| `decomp_product.R` | drift by HS2 chapter, era vs era | `eta_drift_by_product.csv` |
| `rate_probe.R` | tracker statutory by program/snapshot (ch30/84/85) | (log) |
| `run_all.R <scenario>` | ladder + both decomps for a scenario | `<scenario>/…` |
| `grain_experiment.R <scenario>` | OOS ETR error by aggregation grain | `<scenario>/grain_experiment.csv` |
| `window_estimator_exp.R <scenario>` | window sweep (A) + Treasury-pin (B) | (log) |

Run pattern (never Rscript on the login node):
```
cd /nfs/roberts/scratch/pi_nrs36/jar335/calib_ladder
sbatch <name>.sbatch     # loads R/4.4.2-gfbf-2024a, runs the .R, logs to <name>_%j.log
```
Cached inputs for the 7-21 scenario: `tracker_actual_2026-07-21/imdb_ext.csv`
(Census IMDB 2025-06..2026-05) and `.../stat_ext.csv` (day-weighted statutory from
the 7-21-08 bundle) — reused by the grain/window scripts so they run in ~1 min.

Persistent memory note: `eta-alpha-regime-drift-diagnosis` (auto-memory) carries the
condensed version of all of the above.
