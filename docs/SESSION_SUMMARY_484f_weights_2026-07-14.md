# Session summary — 484(f) weights + dashboard-rate reconciliation (2026-07-14)

## TL;DR

The dashboard showed two "average tariff rate" numbers that were supposed to agree
but didn't. We found and fixed **two independent bugs**, and they now agree exactly:

| | Model tile | Tracker daily line |
|---|---|---|
| Original (the reported problem) | 9.03% | **8.42%** |
| After the import-weighting fix | 9.06% | 9.27% |
| After the daily rate-basis fix | **9.0596%** | **9.0596%** |

Everything is **built, tested, committed, and verified on real data — but nothing is
live yet.** The live dashboard still reads the old vintage. Go-live is a deliberate,
separate step (see "What still needs to happen").

---

## The problem we started with

The end-2026 statutory tariff rate read **9.03% (model tile)** vs **8.42% (tracker
daily line)**. Two numbers that describe the same thing should not differ by 0.6pp.

Root of the original gap: the tracker's daily series inner-joined current-HTS10 rates
to a raw 2024 Census import-weight base. When the government **renumbers product
codes**, ~**$219B (7%) of imports** fell out of the weighted average (weighted at an
implicit 0%), dragging the tracker line down to 8.42%.

---

## What changed today

### Bug #1 — import weighting ($219B renumbered-trade gap)

Implemented the principled fix from the `wondrous-spinning-frost` plan (Phases 4–5):
re-key the 2024 import weights onto **each revision's** HTS codes at that revision's
HTS-identity date, using the committed USITC **484(f) transfer crosswalk**, instead
of joining one static "tip" weight set across all history.

- Daily series gained a **per-interval weight provider** + interval-local weight
  context, a **memoized per-revision mapper** (`make_interval_weights_fn`), and a
  **fingerprinted daily-part cache** (schema v2) so stale weights can never silently
  be reused.
- New config `weight_method: 484f` (default) vs `static` (legacy). All callers
  (array build, gather, streaming, backfill) wired through.
- Publish/manifest now stamp `revision` on every snapshot record + a full **weights
  provenance block** (method, hashes, totals, source-PDF hashes), with proper
  weight-mode plumbing (`required` aborts on failure; `unweighted` skips explicitly).
- **Model-side guards** (new branch `weights-coverage-guard`): a corrected coverage
  guard (tip slice must be ~100% exact; historical slices gated on a threshold) and a
  **manifest vintage assertion** (weights `hts_vintage` must equal the manifest's
  actual-tip revision).

Result: end-2026 tracker daily rose 8.42% → **9.27%**, with **100% of import dollars
now counted** (the $219B gap closed).

### Bug #2 — daily chart ignored trade-preference (USMCA/FTA) exemptions

Reconciling the model (9.06%) against the tracker (9.27%) surfaced a **second,
pre-existing bug**. The daily series *re-derived* each product's combined rate as
`base_rate + added tariffs`, which re-used the **un-reduced statutory base** and so
**re-charged the full duty on goods that actually enter duty-free** under a trade
agreement.

- Confirmed on real data: **368,473** product-country rows have `base_rate > 0` but a
  stored `total_rate = 0`, and **100% are `usmca_eligible`** (genuinely duty-free).
  The stored `total_rate` correctly reflects USMCA/FTA + MFN-exemption reductions; the
  re-derivation ignored them.
- Fix: the daily aggregators now use the **authoritative stored `total_rate`**, keeping
  re-derivation only where it's genuinely needed (the by-authority breakdown; the live
  Swiss-framework expiry, applied row-level; and the non-default `tpc_additive`
  stacking view).

Result: tip daily weighted rate **9.2740% → 9.0596%**, now **exactly matching the
model tile** and the direct import-weighted dot-product.

---

## Where we ended (verified, not live)

- **Model and tracker daily now agree at 9.0596%** (direct dot-product confirms it).
- **Nothing is live.** The tracker's `latest` pointer is unchanged (still
  `2026-07-13-20`); the corrected data sits beside it as a held vintage.
- Full tracker rebuild ran cleanly to vintage **`2026-07-14-16`** (actual + new_301),
  100% weight coverage.
- Full economic model ran cleanly on it → vintage **`v1/2026-07-14-16-484f`** (GTAP
  completed for both scenarios).
- All unit tests green (tracker: interval-weights 15/15, daily 81/0, mapper 24/24,
  publish/manifest 36; model: coverage-guard 13/13).

### Commits

**tariff-rate-tracker — branch `484f-weights`**
- `2f086fd` P1 484(f) parser + sources
- `cc30d81` P2 crosswalk generation + archive coverage
- `15646c6` P3 versioned-identity mapper + 2025 split-shares
- `7496efd` P4 daily GTAP rollup → HS6 join
- `703feb5` P4 per-interval weight provider + fingerprinted cache
- `b9e930f` P4 callers wired to the provider
- `b90b82a` P5 manifest revision + weights provenance + weight-mode plumbing
- `eb6f6e5` weights_484f build config
- `7adfb27` **daily series: use stored effective rate (Bug #2 fix)** ← last today

**Tariff-Model**
- branch `weights-coverage-guard`: `a6d7cd5` coverage guard + vintage assertion (off master; NOT merged to master)
- branch `eta-diagnostic-export`: `add8979` (eta WIP committed), `af77dc3` (merged the guard branch in), `0ee07a8` (scenario pins → `2026-07-14-16`)

---

## What still needs to be re-run / done before go-live

1. **Rebuild the tracker daily with `7adfb27`.** The published `2026-07-14-16` daily
   CSVs were generated *just before* the Bug #2 fix, so they still read 9.27%. Re-run
   the daily aggregation (array daily-parts, or a gather recompute) so the published
   daily series carries the corrected 9.06%.
2. **Regenerate the golden reference files** (`tests/golden/*/daily/*.csv`). The parity
   check will flag the intended ~0.2pp drop (concentrated in exemption-affected lines
   like tobacco); review the diff, then bless the new goldens.
3. **The model does NOT need re-running for Bug #2** — it already reads the stored
   effective rate (9.06%) and is correct. It only needs a re-run if the tracker is
   republished to a *new* vintage (then bump the 4 scenario pins + rerun).
4. **Merge `weights-coverage-guard` → `master`** (Tariff-Model) when ready (currently
   merged only into `eta-diagnostic-export`).
5. **Go-live (deliberate, user-gated):** repoint `latest` on the tracker
   (`publish_vintage.R --latest-only <vintage>`) and cut the model dashboard over,
   with a before/after review.
6. **Heads-up for the Budget Lab:** Bug #2's fix *lowers* the published daily
   tariff-rate line across all history (it now correctly credits FTA/USMCA/GSP
   duty-free). It's the correct number, but it's a visible change to a published
   series — worth a before/after review before cutover.

---

## What the "S232 fix" is (separate, pre-existing — NOT blocking)

When the tracker rebuild ran its self-check gate (`verify_build.R`), it came back
**10 passed, 1 failed**. The one failure is unrelated to any of today's work:

- It's a **Section 232 steel/aluminum product-scope** test. Certain upstream/scrap
  lines (scrap, pig iron, ferroalloys) and copper cathodes carry (or lack) a 232 rate
  in specific snapshots (`snapshot_rev_14`, `snapshot_2026_rev_5`) that the test
  expects handled by a newer "chapter-inference" scope fix. The test message literally
  says *"snapshot predates the scope fix — rebuild snapshot_rev_14.rds"*.
- **Confirmed pre-existing:** running the same gate against the *current live* vintage
  (`2026-07-13-20`) fails the **exact same** two lines (also 10 passed, 1 failed). So
  it is not a regression from this work and does not block it on a like-for-like basis.
- It's a separate cleanup item: rebuild the affected snapshots with the 232 scope fix
  so the gate goes fully green. Tracked independently.

---

## Key paths / artifacts

- Plans: `~/.claude/plans/wondrous-spinning-frost.md` (484f weights, Phases 1–7 status),
  `~/.claude/plans/dazzling-twirling-pillow.md` (the daily rate-basis fix).
- Held tracker vintage: `<model_data>/Tariff-Rate-Tracker/2026-07-14-16/` (scratch kept
  at `.work/2026-07-14-16/` because the verify gate exited on the pre-existing S232 test).
- Held model vintage: `<model_data>/Tariff-Model/v1/2026-07-14-16-484f/`.
- Reusable Slurm job scripts + diagnostics:
  `/nfs/roberts/scratch/pi_nrs36/jar335/484f_jobs/`.
- Cluster note: R runs **only via Slurm** (`sbatch --partition=devel`,
  `module load R/4.4.2-gfbf-2024a`); a job must **`cd` into the repo before `Rscript`**
  (the `here()` anchor gotcha — `setwd()` inside R doesn't fix it).

---

## Bottom line

Both bugs that made the dashboard's two tariff numbers disagree are fixed and proven
(they now read an identical **9.06%**). The remaining work is mechanical rollout —
rebuild the published daily with the fix, regenerate goldens, and flip the live
pointers with a before/after review — plus the independent, pre-existing S232 scope
cleanup.
