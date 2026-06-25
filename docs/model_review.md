# Yale Budget Lab Tariff Model — Code & Economics Review

**Branch:** `upstream-refactor` · **Reviewers:** 9 specialist passes (upstream-rate-inputs, load-and-etr, price-model, revenue, macro-usmm, sectors-distribution, calibration, outputs-orchestration, cross-cutting) · **Date:** 2026-06-22

---

## 1. Executive summary

The model is broadly sound and internally disciplined: the canonical fractional-ETR convention is respected on the live paths, the noncompliance (a)/(b)/(c) decomposition is implemented coherently, and the calibration harness reproduces its golden fixtures bit-for-bit. Most findings are low-severity consistency, documentation, or defensive-coding gaps that are *latent* on current data rather than active wrong numbers. **There is, however, one critical bug** that distorts a headline output: the temporary-tariff IRFs are shifted using the permanent-shock onset constant, delaying every temporary-tariff macro response by exactly three quarters and corrupting the flagship 2026 Q4-over-Q4 GDP, unemployment, PCE, and fed-funds figures for every production scenario that uses a temp window (including the live `2026-04-06` family). A second methodology issue in the same module (double-counting overlapping temp IRFs) compounds the macro distortion.

The biggest recurring **theme** is a subtle noncompliance/substitution-channel inconsistency in the **time-varying revenue path**: the per-fiscal-year revenue trajectory is anchored to a post-noncompliance (b) GTAP level but scaled by a pre-noncompliance (a) time profile (flagged independently by two reviewers), and a related FY-window bug carries the first tariff regime into pre-policy days, overstating FY2026 revenue in real scenarios. Both feed the headline 10-year revenue total. A second theme is **silent-drop / coercion fragility** (inner_join on PCE keys, missing `nrow>0` guards on the (b) loader, Date-vs-character filters) — all currently dormant but each a house-rule violation that exists precisely to surface the data problems they would otherwise mask.

**Trust assessment:** The price, ETR, sector, foreign-GDP, and distribution headline levels are trustworthy at current parameters. The **macro 2026 Q4-Q4 numbers should not be trusted** until `mac-1` (and `mac-2`) are fixed. **Conventional revenue** is slightly overstated in FY2026 (`rev-1`) and has a bounded inter-year profile bias under active noncompliance (`loa-1`/`xc-1`); the 10-year total is affected but not grossly. Everything else is cosmetic or maintainability.

---

## 2. Top findings, ranked by impact

| Rank | Severity | Category | Title | File:lines | One-line impact |
|---|---|---|---|---|---|
| 1 | **Critical** | bug | Temp IRF shifted by perm-onset constant (3-qtr delay) | `src/05a_usmm_surrogate.R:337-347` | Headline 2026 Q4-Q4 GDP/U-rate/PCE/fed-funds wrong for all temp scenarios |
| 2 | Medium | economic | Overlapping temp IRFs summed (double-counted reversion) | `src/05a_usmm_surrogate.R:258-262` | Macro paths over-apply temporary-tariff dynamics in 4 live scenarios |
| 3 | Medium | consistency | FY-weighted ETR carries first regime into pre-policy days | `src/04_calculate_revenue.R:26-27,64-71` | FY2026 (in 10-yr total) revenue overstated when first date > FY start |
| 4 | Medium | economic | Time-varying revenue: (b) level anchor × (a) time profile | `src/02_calculate_etr.R:438-461` | Per-FY conventional revenue profile biased under active noncompliance |
| 5 | Medium | economic | `domestic_pricing=0.5` markup vs overhaul memo's `(1−offset)` | `src/io_price_model.R:763-767` | Headline price effects ~33% above documented formula — **needs human confirm** |
| 6 | Low | bug | Price model drops tariff'd commodities absent from B_MD (message, not stop) | `src/io_price_model.R:692-700` | Latent: non-zero shock on a future un-mapped commodity silently lost |
| 7 | Low | bug | check-stage SKIP crashes on `%||%` under R 4.3.1 | `src/calibration/check_eta_reproduction.R:38-47` | Reproduction gate dies with confusing error in the common skip case |
| 8 | Low | bug | (b) `tau_M_b` loader lacks `nrow>0` guard | `src/01_load_inputs.R:347-388` | Empty (b) ref-date slice → all-zero post-sub price, no error |
| 9 | Low | bug | `daily_etr_levels` hardcoded 2026-12-31 horizon | `src/11_write_outputs.R:464-471` | Latent: 2027+ policy dates silently truncated from tracker CSV |
| 10 | Low | economic | Unweighted heading-mean rate fallback | `src/00a_prepare_rate_inputs.R:245-268` | Small bias on ~$0.25B dispersed-heading fill |
| 11 | Low | economic | BEA adjustment codes keep `omega_D=1` supply-chain passthrough | `src/io_price_model.R:601-618` | ~0.003–0.014 pp aggregate price overstatement |
| 12 | Low | consistency | Snapshot vs panel baseline_date semantics differ | `src/00a_prepare_rate_inputs.R:601-615` | Same config can run in panel mode, hard-crash in snapshot mode |
| 13 | Low | economic | eta uncovered cells pass through at statutory (η′=1) | `src/calibration/calibrate_eta.R:244-250` | Newly-tariffed lines off the train basket get no noncompliance haircut |
| 14 | Low | economic | `R_alpha` reference-date scalar reused across all dates | `src/02_calculate_etr.R:345-358` | Near-term revenue substitution wedge approximated, fades in 5 FYs |
| 15 | Low | economic | alpha corrects rate channel but not qmwreg quantity | `src/04_calculate_revenue.R:208-223` | Documented design choice; rationale comment missing — **uncertain** |
| 16 | Low | bug | Untraded goods sector would crash Step 2 | `src/00a_prepare_rate_inputs.R:413-424` | Latent: matrix row-set not reconciled with consumer's 45-sector expectation |
| 17 | Low | consistency | Mixed sign convention in `by_decile` (% positive, $ negative) | `src/09_calculate_distribution.R:43-48` | Raw distribution.csv mixes signs; Excel re-coerces defensively |
| 18 | Low | economic | Stale `scaling_factor` after income-vector edit | `resources/distribution/decile_parameters.csv:1-11` | Per-decile %-of-income off by ≤1.5% (working-tree state) |
| 19 | Low | economic | Per-HH average uses simple `mean()` of deciles | `src/09_calculate_distribution.R:109-115` | Correct today; unenforced equal-population assumption |
| 20 | Low | bug | GTAP shocks rounded to 0.1pp, sub-0.05pp cells dropped | `src/00a_prepare_rate_inputs.R:432-440` | GE leg loses tiny shocks price/ETR legs keep; dropped mass unlogged |
| 21 | Low | consistency | BEA per-date slice: Date column vs character RHS | `src/01_load_inputs.R:316-322,374-378` | Works via coercion; diverges from sibling Date==Date slices |
| 22 | Low | consistency | Levels matrix zero-fills absent (partner,sector) cells | `src/00a_prepare_rate_inputs.R:413-418` | Spurious 0% all-in level in reported levels CSV |
| 23 | Low | consistency | Hardcoded `30441` US-GDP magic constant (diagnostic) | `src/run_model.R:550-551` | Console-only; stale-able, inconsistent with vgdp base |
| 24 | Low | consistency | `bea_io_level` default disagrees across 4 sources | `config/global_assumptions.yaml:22-25` | Reader foot-gun on which I-O table a run used |
| 25 | Low | consistency | `inflation_2024_to_2025` documented-but-dead | `config/global_assumptions.yaml:14-16` | Comment claims an adjustment that never happens |
| 26 | Low | consistency | `average_*` helpers use `inner_join` on PCE keys | `src/run_model.R:49-57,82-95` | Latent silent-drop if markup variants' key sets ever diverge |
| 27 | Low | style | `blend_usmm_gdp_deviation` uses base `ifelse()` | `src/helpers.R:96-100` | House-rule violation on dynamic-revenue path; safe today |
| 28 | Low | style | Calibration harness uses `ifelse()` / `na.rm=TRUE` | `src/calibration/calibration_helpers.R:135,186-225,298` | Verbatim upstream port; emit path guarded; style only |
| 29 | Low | bug | CBO convolution relies on implicit CSV row order | `src/07_calculate_dynamic_revenue.R:103-131` | No `arrange()`; silently wrong if CSV reordered |
| 30 | Low | bug | `setdiff()` on Dates prints epoch-day integers | `src/00a_prepare_rate_inputs.R:616-624` | Confusing error message on misconfig |
| 31 | Low | simplification | GE decomp summarizer duplicated in two files | `src/run_model.R:97-122` ↔ `src/io_price_model.R:1212-1233` | Divergence risk if summary definition changes |
| 32 | Low | simplification | Country-ETR weighting math triplicated | `src/02_calculate_etr.R:39-141,165-212` | Central ETR math in 3 places; edit-skew risk |
| 33 | Low | simplification | `imports_by_country`/`vgdp`/`qva`/`vom` loaded unused | `src/01_load_inputs.R:474-476` | Wasted work + misleading dead fields |
| 34 | Low | simplification | `run_model.R` sources but never calls Excel exporter | `src/run_model.R:33` | Unnecessary openxlsx coupling on every run |
| 35 | Low | consistency | Stale docstring references deleted `calculate_prices()` | `src/09_calculate_distribution.R:58` | Points maintainer at a nonexistent function |
| 36 | Low | simplification | `consumption_share` loaded but never used | `src/09_calculate_distribution.R:37-49` | Latent maintenance hazard (reads as if it matters) |
| 37 | Low (uncertain) | bug | Per-slice (not panel-wide) overlap check | `src/read_rate_panel.R:284-302` | Defensive gap; impossible under default slicing |

---

## 3. Detailed findings (grouped by theme)

### 3.1 Macro / USMM surrogate — the highest-impact cluster

#### CRITICAL — Temp IRF shifted by the permanent-shock onset constant (`mac-1`)
**What's wrong.** The perm and temp IRF families have **different native onset quarters**: perm IRFs first respond in 2025Q2 (index 6 = `USMM_SHOCK_QUARTER`), but all three temp IRFs (`temp2/5/10_fed.csv`) first respond materially in 2026Q1 (index 9). `construct_usmm_response()` computes a single `onset_shift = comp$onset_q - USMM_SHOCK_QUARTER` (=6) and applies it to **both** the shifted-perm path and the temp path via `shift_irf(temp_irf[[var]], onset_shift)` (`src/05a_usmm_surrogate.R:337-347`). For the perm path this correctly re-centers a 2025Q2 IRF; for the temp path it double-counts the 3-quarter gap between the two native onsets.

**Why it matters.** Worked through the live `2026-04-06` config (`usmm_dates = [2026-02-24, 2026-04-06, 2026-07-24, 2026-09-29]`, `temp_component_dates = [2026-02-24, 2026-07-24]`): comp1 onset 2026-02-24 → `onset_q=9` → `onset_shift=3`, pushing the temp IRF (already native to index 9) to first respond at index 12 (2026Q4) instead of 2026Q1; comp2 is pushed to 2027Q1. **Every temporary-tariff GDP/unemployment/PCE/fed-funds response moves ~3 quarters too late**, largely out of the 2026 window, directly distorting the flagship 2026 Q4-over-Q4 numbers. The base-case `macro_quarterly` (called with `use_temp_irf=TRUE`) is the headline series. Static and shifted-perm-only scenarios are unaffected.

**Fix.** Introduce a temp-specific constant `USMM_TEMP_SHOCK_QUARTER <- 9` and shift temp components by `comp$onset_q - 9`, keeping perm components on `USMM_SHOCK_QUARTER`. Add an assertion that each IRF family's first-nonzero quarter matches its assumed native onset, so a future IRF re-extraction cannot silently desync. (Note `refund_fed.csv` is already added without a shift, which is correct.)

#### Medium — Overlapping temp IRFs summed, double-applying reversion dynamics (`mac-2`)
**What's wrong.** The temp-match predicate `use_temp <- any(onset >= temp_pairs$onset & offset <= temp_pairs$offset)` (`src/05a_usmm_surrogate.R:258-262`) flags **every** telescoping component fully contained in the temp window. For the `2026-04-06` config, both interior segments `[02-24→04-06]` and `[04-06→07-24]` qualify, so two separate deltas are each fed through the temp IRF. Unlike the shifted-perm path (which differences on/off to telescope correctly), the temp branch has **no offset truncation** — it adds only `shift_irf(temp_irf, onset_shift)`. The temp IRF already embeds a full rise-and-revert path, so summing two same-sign overlapping episodes compounds the expectations/reversion dynamics for what is conceptually one temporary regime with a mid-window step-up.

**Why it matters.** Affects 4 live scenarios with a `usmm_date` strictly inside the temp window: `2026-04-06`, `2026-04-06_ex_pharma`, `2026-04-06_observed`, `tracker_actual_2026-06-04`. This was a deliberate predicate change (commit `071effc`, 2026-06-03, replacing exact-equality with range containment) whose compounding side-effect appears unintended.

**Fix.** Decide explicitly whether a temp window maps to a single temp-IRF impulse (sized to the window's peak delta, placed at window onset) or to one IRF per interior segment. If the latter, document why summing overlapping un-truncated temp IRFs is economically correct; otherwise collapse the window to one temp component.

> **Combined fix note:** `mac-1` and `mac-2` are best fixed together — both live in `construct_usmm_response`/`decompose_shocks` and both concern how temp components are placed and summed.

#### Low — Hardcoded `30441` US-GDP constant for long-run dollar loss (`mac-3` + `out-4`, merged)
Two reviewers flagged the same line. `src/run_model.R:550-551` reports the long-run USA GDP dollar loss as `foreign_gdp_results$usa / 100 * 30441` — an inline literal (2025 nominal US GDP, $B). It is **display-only** (a `message()`, never written to any output CSV) so arithmetic and order of magnitude are fine. Two issues: (a) it duplicates `baseline_nominal_gdp = 30441.1` already in `resources/cbo_rules/cbo_convolution_params.csv` (consumed by `07_calculate_dynamic_revenue.R`), so it is not truly "magic" but is an un-named, stale-able copy; (b) the World / World-ex-USA aggregates in `08_calculate_foreign_gdp.R` use the GTAP `vgdp_baseline` USA level (~$19.5T), so the USA dollar figure is anchored to a different base/vintage than its sibling aggregates. **Fix:** read the value from `inputs$assumptions` (move to `global_assumptions.yaml` as a named, dated `us_gdp_2025_bn`) or derive from `vgdp_baseline['usa']`; at minimum add a source comment.

---

### 3.2 Noncompliance / substitution-channel coherence in revenue

#### Medium — Time-varying revenue: (b) level anchor scaled by (a) time profile (`loa-1` + `xc-1`, merged)
**What's wrong.** Two reviewers converged on this. For time-varying scenarios, the per-FY revenue `etr_increase` is built as `mtax_etr_increase * (weighted_etr / ref_etr)` (`src/02_calculate_etr.R:438-461`). The **level anchor** `etr_increase` traces (via `inputs$etr_increase` → `read_gtap.R` `extract_mtax_etr_increase`) to a GTAP run shocked with the **rate-(b) eta′-adjusted** reference-date rates (`00a` writes `shocks.txt` from `ref_partner_gtap_b`). But the **scaling ratio** `weighted_etr/ref_etr` comes from `per_date_etrs`, computed from `inputs$etr_matrix_by_date` — the **rate-(a) applied-statutory** delta matrix. So the revenue level is post-noncompliance (b) while its time profile is pre-noncompliance (a).

**Why it matters.** This is internally contradictory: the *same module* hands USMM a (b) time profile (`b_etr_increase_by_date`, used at `05a_usmm_surrogate.R:460-462` under a `noncompliance_active` branch) but hands revenue an (a) time profile off the same (b) level. The bias is exactly zero at the reference date (ratio=1) and zero when noncompliance is inactive (a≡b). It bites only for non-reference dates when eta′ composition shifts across dates so the (a) and (b) cross-date ratios diverge. It feeds headline conventional revenue and is diluted by FY day-weighting, hence medium not high.

**Fix.** When `inputs$noncompliance_active`, build the revenue scaling ratio from the (b) per-date series — `per_date_etrs_b / ref_b_etr`, already computed at `src/02_calculate_etr.R:484-492` for USMM. Keep (a) only for the diagnostic pre-sub series. Trivial since the inputs already exist.

#### Medium — FY-weighted ETR carries the first regime into pre-policy days (`rev-1`)
**What's wrong.** `compute_fy_weighted_etr_increase`'s docstring says "Before the first date: `etr_increase = 0`" (`src/04_calculate_revenue.R:26`), but the implementation uses `increases[1]` (the first regime's tariffed rate) when no schedule date precedes the interval start (lines 64-71), and the inline comment documents the *opposite* intent.

**Why it matters — corrected from the original reviewer note.** The reviewer believed the earliest date is 2025-01-27 (FY2025, excluded from the budget total). **That is wrong:** the function consumes `etr_results$etr_increase_by_date`, and produced outputs (`output/2-20nov/.../etrs_by_sector_country.csv` and the `*nov`/`s122-preempt` scenarios) have earliest date **2026-01-01**. FY2026 (2025-10-01 → 2026-09-30) **is inside** the budget window (`filter fiscal_year>=2026`). When the first schedule date (2026-01-01) falls after `fy_start` (2025-10-01), the pre-policy Oct–Dec 2025 quarter (~92 days) is treated as fully tariffed. Reproduced: with first date 2026-01-01 @ 0.10 and 2026-02-20 @ 0.14, the as-coded FY2026 `etr_increase` = 0.1244 vs the docstring-correct 0.0992 — a **2.52pp overstatement** that flows through `total_etr → new_duties → gross_revenue` into the **10-year headline total** with no clamp.

**Fix.** Set `active_etr <- 0` when `length(active_idx)==0`. Baseline subtraction (`gross = new_duties − duties_bn`) already accounts for pre-policy collections, so 0 is the economically correct pre-policy value. Reconcile the docstring accordingly.

#### Low — `R_alpha` is a single reference-date scalar reused across all dates (`xc-3`)
`R_alpha = post_sub_etr_alpha / post_sub_etr` is computed once from `inputs$viws` and the reference-date `etr_matrix_b` (`src/02_calculate_etr.R:345-358`), then multiplies the per-FY day-weighted `fy_etr_vec` across all years in `04_calculate_revenue.R`. The substitution wedge is non-linear in the shock level, so applying one reference-date wedge uniformly across a multi-date schedule is an approximation. Bounded: `R_alpha=1` (alpha inactive, the default) is an exact no-op, and `alpha_phi` decays its influence to zero by `anchor_fy+5`. **Fix:** document that `R_alpha` is a reference-date approximation applied uniformly, or compute a per-date `R_alpha` for time-varying scenarios.

#### Low (uncertain) — Alpha corrects the rate channel but not the qmwreg quantity channel (`xc-2`)
The alpha→GTAP transition scales `total_etr` up by `(1 + phi*(R_alpha−1))` but leaves `new_imports` (built from full-GTAP `qmwreg`) uncorrected (`src/04_calculate_revenue.R:208-223`). **This is a documented, validated design decision, not a bug:** `R_alpha` (a rate ratio) and `qmwreg` (an aggregate quantity change) are distinct, separable terms of `new_duties = new_imports × total_etr`; the revenue identity stays coherent; and project memory records the explicit decision that "alpha is used ONLY in the revenue transition; the structural model stays full GTAP." With `R_alpha>1` the correction unambiguously raises near-term revenue (the "ambiguous sign" worry is incorrect). **Needs human confirm only on intent:** an economist could argue over-stated substitution should also dampen the quantity channel. **Fix (optional):** add a rationale comment explaining why only the rate channel is corrected.

---

### 3.3 Price model

#### Medium (uncertain) — `domestic_pricing=0.5` markup vs the overhaul memo (`pri-1` + `xc-1 units`, merged)
**What's wrong.** The applied scaling is `1 + domestic_pricing − usd_offset` = 1 + 0.5 − 0.174 = **1.326** (`src/io_price_model.R:763-767`), i.e. every pre-sub and PE-post-sub commodity price effect is multiplied up ~33% before the USD offset. This contradicts `price_model_overhaul_memo.md`, which presents the new formula as `× (1 − USD offset)` and explicitly argues the old 0.50 multiplier "is hard to defend."

**Key correction from the cross-check.** The price-model reviewer claimed the *in-file docstring* also contradicts the code — **that is false.** The in-file docstring (`src/io_price_model.R:11`), the `@param` block (661-664), the config comment, and the runtime log are all **consistent** with `1 + domestic_pricing − usd_offset`. Git history (`3f24056` shipped `(1−usd_offset)`; `6297b8e` deliberately re-added `domestic_pricing` afterward) strongly indicates **the code is intentional and the memo is stale.** The drift exists only in out-of-file project notes (MEMORY.md line 30, the assignment brief).

**Why it matters / what to check.** `pre_sub_price_increase` and `pe_postsub_price_increase` are headline outputs feeding distribution; if `0.5` is *not* the intended central case, this is a live ~33% overstatement. The dominant evidence says it is intended — but **a human owner must confirm.** **Fix:** if intentional, update `price_model_overhaul_memo.md` and MEMORY.md to document and justify the retained markup; if not, set `domestic_pricing=0` and correct the docstring.

#### Low — Price model drops tariff'd commodities absent from B_MD (message, not stop) (`xc--2`)
`compute_io_prices` aligns `tau_M` to the B_MD commodity set by intersection; unmatched entries are reported via `message()` and **dropped** (`src/io_price_model.R:692-700`). A real non-zero tariff on a commodity present in the rate rollup but missing from B_MD rownames would be silently excluded, understating prices — and this is the *only* dimension mismatch in the function that warns rather than `stop()`s (B_MD-vs-Leontief and propagated-vs-commodities both stop). **Currently latent:** all 41 candidate BEA codes in the checked-in crosswalks are present in B_MD (0 unmatched). **Fix:** promote to `stop()` when any unmatched `tau_M` code carries a non-zero shock (mirror `rollup_bea`'s `delta != 0` backstop); pure-zero unmatched codes can stay a message.

#### Low — BEA adjustment commodities keep `omega_D=1` supply-chain passthrough (`pri-5`)
For BEA statistical-adjustment commodities ('Other'/'Used' at summary level), the code zeroes `omega_M` but sets `omega_D=1` (`src/io_price_model.R:601-618`), so they retain the **full** domestic Leontief supply-chain price increase though "noncomparable imports" are by construction imports. They are neutralized in the direct channel but not the supply-chain channel. Quantified under a uniform 10% shock: +0.0029 pp (constant_dollar) to +0.0141 pp (constant_percentage) aggregate overstatement; real scenarios shock only a subset, so smaller. **Fix:** if these should be fully neutralized, set both `omega_M` and `omega_D` to 0; if domestic passthrough is intended, document why.

---

### 3.4 Upstream rate-inputs (00a) & refactor integrity

#### Low — Snapshot vs panel `baseline_date` semantics differ (`ups-1`)
Panel-mode resolves the baseline with `slice_panel_at`, which finds the interval **covering** the date (works mid-interval); snapshot-mode calls `read_rate_snapshot`, which requires an exact `valid_from=<baseline_date>` directory and **hard-stops** otherwise (`src/00a_prepare_rate_inputs.R:601-615`, `read_rate_panel.R:121-130`). The existing configs set `baseline_date: '2025-01-01'`, which is not a revision-start date — so a config that runs in panel mode would crash in snapshot mode. The baseline is the denominator of every delta. Failure is loud (a `stop`), so medium-low. **Fix:** in snapshot mode, resolve the baseline by selecting the latest actual snapshot date `<= baseline_date`, mirroring the half-open covering-interval semantics; document the chosen contract.

#### Low — Unweighted heading-mean rate fallback (`ups-3` + `xc--2`, merged)
Two reviewers flagged the same code. When a weighted `(hts10, cty)` pair has no exact panel rate, `apply_prefix_rate_fallback` fills it with the **simple (unweighted) mean** of `rate_total` over the heading (`src/00a_prepare_rate_inputs.R:245-268`), inconsistent with the import-weighted convention everywhere else (`rollup_gtap`, `rollup_bea`, `calculate_country_etrs`). Impact is small and self-instrumented: the dominant HS8 tier fills 1,830 rows / $10.86B with **zero** rate dispersion (so unweighted = weighted there); only the HS4 tier fills $0.25B in dispersed headings (~0.008% of the ~$3.12T base), and the `ambiguous` (non-constant-rate) count is already logged. The bias also largely cancels in deltas (the same fallback applies to scenario and baseline). The panel slice carries no import column, so import-weighting requires a join, not a one-line change. **Fix:** import-weight the heading mean where the weight base is joinable, or at minimum log the import value covered by ambiguous fills.

#### Low — Untraded goods sector would crash Step 2 (`xc--1`)
`to_sector_country_wide` filters the matrix to `intersect(SECTOR_ORDER, wide$gtap_code)` — only sectors appearing in the import-weighted rollup survive (`src/00a_prepare_rate_inputs.R:413-424`). But `calculate_country_etrs` requires **every** goods sector in `rownames(import_baseline_dollars)` (45 sectors) and hard-stops otherwise ("ETR matrix missing gtap_code for sector(s)"). If any baseline goods sector is untraded in the weights, the new path crashes Step 2 where the legacy (zero-filled) path did not. On current data all 45 trade, so dormant. The fragility: the matrix is driven by the weight universe, the consumer by `import_baseline_dollars`, and nothing reconciles them. **Fix:** reindex the wide matrix to the full goods-sector set the consumer expects, zero-filling absent sectors.

#### Low — GTAP shocks rounded to 0.1pp; sub-0.05pp cells dropped (`xc-4`)
`write_shocks` rounds each partner×sector delta to 1 dp (`round(etr*100, 1)`) then filters `etr_pct != 0` (`src/00a_prepare_rate_inputs.R:432-440`). Any aggregated delta below 0.05pp is dropped from the GTAP shock file while the same un-rounded delta flows into the BEA/price path and ETR matrices at full precision — a small GE-vs-price/ETR leg inconsistency. The 0.1dp granularity is a deliberate GTAP solver convention (ported from legacy). Each dropped cell is a sector×partner average tariff change below 5 bps; GTAP would respond near-zero anyway. **Fix:** confirm GTAP's precision requirement; if finer is accepted increase to `round(.,3)`, or at minimum log the import-weighted mass dropped by the filter.

#### Low — Levels matrix zero-fills absent (partner,sector) cells (`xc-5`)
`to_sector_country_wide` is shared between deltas and levels and pivots with `values_fill = 0` (`src/00a_prepare_rate_inputs.R:413-418`). For deltas 0 is correct (no change); for **levels**, a zero-import pair gets a spurious **0% all-in tariff** rather than its baseline rate. Bounded in weighted consumers (the cell carries ~0 derived imports in `calculate_country_etrs`), but the levels matrix is also written to `sector_country_levels.csv` for reporting, where a 0 reads as "0% tariff" instead of "no imports." **Fix:** pass a value-type flag so levels use `NA` / a baseline-derived fill; keep delta behavior unchanged. Or document that absent cells = zero-import pairs.

#### Low — BEA per-date slice compares Date column to character RHS (`xc-2` + `xc--3`, merged)
Two reviewers flagged the same lines. The BEA-delta slices filter `date == as.character(inputs$gtap_reference_date)` (`src/01_load_inputs.R:319-321, 376-378`), while the sibling ETR/levels slices compare Date==Date (`filter(date == inputs$gtap_reference_date)`). It works via `Ops.Date` coercion and is guarded by `stopifnot(nrow > 0)` at line 322, and the sole writer (`00a`) always emits ISO dates — so no live bug. **Fix:** coerce once and compare Date-to-Date in both BEA slices, dropping the `as.character()` wrapper, for uniformity with the sibling slices.

#### Low — `setdiff()` on Dates prints epoch-day integers (`ups-4`)
`missing_dates <- setdiff(requested_dates, snapshot_dates)` strips the Date class (verified: yields `20120`), so the `stop()` message on a `snapshot_dates` misconfig prints epoch-day integers instead of dates (`src/00a_prepare_rate_inputs.R:616-624`). Diagnostic-quality only; logic is correct. **Fix:** `requested_dates[!requested_dates %in% snapshot_dates]`, or wrap in `as.Date(..., origin='1970-01-01')`.

#### Low (uncertain) — Per-slice (not panel-wide) overlap check (`ups-2`)
`slice_panel_at`'s "exactly one interval per pair" assert runs only at the single sliced date (`src/read_rate_panel.R:284-302`). **The titled failure mode is impossible under default slicing:** any panel-internal overlap is caught because the later interval's own `valid_from` (always a slice date) lies inside the overlap. The residual gap is narrow — a user-supplied `snapshot_dates` subset over a *malformed* upstream panel could skip the offending interval start. Defensive only. **Fix (hardening):** add a one-time panel-wide overlap validation in `normalize_panel`.

---

### 3.5 Calibration harness

#### Low — check-stage SKIP branch crashes on `%||%` under R 4.3.1 (`cal-1`)
The reproduction gate's SKIP branch calls `vintage %||% 'NA'` (`src/calibration/check_eta_reproduction.R:42-43`), but `%||%` only entered base R in 4.4.0 and this repo runs **4.3.1** (reproduced: `could not find function "%||%"`). `run_check()` sources only `calibration_helpers.R` (which, despite the misleading `# %||%` comment at `calibrate.R:98`, does *not* define it) and `check_eta_reproduction.R` — neither loads purrr/rlang. SKIP is the **common case** (the fixture dir has one vintage; any other vintage or NA vintage hits it), so the gate dies confusingly exactly when it should print a skip message. PASS/FAIL paths are masked. **Fix:** replace with `if (is.na(vintage)) 'NA' else vintage` (clearer, and `%||%` no-ops on NULL not NA anyway), or load purrr/rlang. **Also fix the false comment** at `calibrate.R:98`.

#### Low — eta uncovered cells pass through at statutory η′=1 (`cal-2`)
The shipped `eta_by_partner_gtap.csv` covers only `(eta_group, gtap_code)` pairs with positive **train-window** statutory revenue (312 rows; NA cells dropped at `src/calibration/calibrate_eta.R:244-250`). Production (`apply_eta_prime`, `00a:548-559`) sets uncovered cells to `fallback=1` (no noncompliance haircut) whenever any eta_file loads — **not** the flat `(1−compliance_effect)=0.90`. So a tariffed sector×partner off the train basket gets full statutory pass-through, understating noncompliance for newly-tariffed lines. **This is deliberate and documented** (apply_eta_prime docstring; `global_assumptions.yaml:45-53`), and the suggested coverage logging **already exists** (verbose at the reference date). Uncovered cells are by construction the low/zero train-trade sectors, bounding the dollar magnitude. Hence low. **Fix (optional):** emit a full `(eta_group × all-gtap)` grid filling missing cells with the partner-group constant eta (already available as `schedule`), so production never silently passes a tariffed sector through at statutory.

#### Low — Calibration harness uses `ifelse()` and `na.rm=TRUE` (`xc--4`)
The standalone calibration modules use base `ifelse()` and `na.rm=TRUE` against house rules (`src/calibration/calibration_helpers.R:135,186-225,298` and siblings). Impact is minimal: the cited `na.rm` weighted-mean feeds only the diagnostic `frac_clipped` (never the emitted eta), several `ifelse`s are intentional NA-fallbacks immediately followed by `[is.na()] <- 0`, and the eta emit path uses a **local** non-`na.rm` `wmean` plus a `filter(!is.na(eta))` that drops NA etas before writing. The file header explicitly marks this as a **verbatim upstream port** that must not be restyled to preserve bit-for-bit reproduction. Production also re-validates the outputs (`load_eta_prime` range-checks `[0,1.5]` and stops on NA). Style only. **Fix (optional):** extend the `check` gate to assert finite, in-range eta for every emitted cell.

---

### 3.6 Revenue / dynamic revenue

#### Low — CBO convolution relies on implicit CSV row order (`rev-2`)
`apply_cbo_convolution` uses `cumprod()` and the convolution loop `level_dev[t-s+1]` with **no `arrange(fiscal_year)`** before these positional ops (`src/07_calculate_dynamic_revenue.R:103-131`). Correct today only because `cbo_convolution_params.csv` is stored ascending and `filter`/`left_join` preserve order. The sibling `calculate_fy_growth_deviations` *does* `arrange(fiscal_year)` before its `lag()`, confirming the author knows ordering matters — this is an inconsistency. `impulse_response[1]=1.0956` makes first-row misalignment especially damaging. **Fix:** add `%>% arrange(fiscal_year)` after the `left_join`.

#### Low — `blend_usmm_gdp_deviation` uses base `ifelse()` (`rev-3`)
The USMM→GTAP 16-quarter blend helper on the dynamic-revenue critical path uses base `ifelse()` (`src/helpers.R:96-100`), violating the CLAUDE.md `if_else()` rule. Functionally safe today (all numeric, no coercion/NA/recycling). **Fix:** swap to `dplyr::if_else`.

---

### 3.7 Sectors & distribution

#### Low — Stale `scaling_factor` after the income-vector edit (`sec-1`)
The working-tree edit changed every decile's `income` but left `scaling_factor` untouched (`resources/distribution/decile_parameters.csv:1-11`). Since `pct_of_income = pce_effect * scaling_factor * 100` and `cost_per_hh = (pct_of_income/100) * income`, and `scaling_factor` was calibrated as `consumption_level/income` against the **old** income (recoverable from `excel_distribution_raw.csv`), both published figures are now off by the per-decile income-change ratio: −1.44% (decile 1) to +0.11% (decile 10). The headline regressivity table is what this module exists to produce. **Fix:** re-run the calibration that produces `scaling_factor` against the shipped income vector, or document `scaling_factor` as exogenous and reconcile.

#### Low — Mixed sign convention in `by_decile` output (`sec-2`)
Within one row, `pct_of_income` is stored **positive** while `cost_per_hh` is **negative** (`src/09_calculate_distribution.R:43-48`); both are written verbatim to `distribution.csv`. The Excel exporter defensively re-coerces **both** to negative (`12_export_excel.R:591,595`), confirming the inconsistency and hiding it from the Excel path while the raw CSV stays mixed. Cosmetic — every magnitude is correct. **Fix:** pick one convention (costs negative) at the source in `calc_decile_distribution` and drop the defensive `-abs()` coercions.

#### Low — Per-HH average uses simple `mean()` of deciles (`sec-4`)
`mean(cost_per_hh)` over 10 rows and the positional median `(cost[5]+cost[6])/2` (`src/09_calculate_distribution.R:109-115`) are correct only if rows are equal-population deciles in order 1..10 — true for the committed CBO deciles, but **unenforced**. `assert_has_columns` checks only column names. **Fix:** assert `nrow==10` and decile order before the positional indexing; comment that the simple mean is valid only for equal-population deciles.

#### Low — `consumption_share` loaded but never used (`sec-3`)
`consumption_share` is in `decile_parameters.csv` but referenced nowhere in R (`src/09_calculate_distribution.R:37-49`); the loader's `assert_has_columns` doesn't even require it. Latent maintenance hazard — it reads as if it constrains `scaling_factor` (see `sec-1`) but nothing uses it. **Fix:** either use it (consumption-weight the average, or derive/validate `scaling_factor`) or drop it from the resource and docs.

---

### 3.8 Outputs & orchestration

#### Low — `daily_etr_levels` hardcoded 2026-12-31 horizon (`out-1`)
The tracker daily ETR series is expanded to a hardcoded `last_date <- as.Date('2026-12-31')` (`src/11_write_outputs.R:464-471`), and the comment falsely references a `valid_until` column absent from `per_date_levels` (which carries only `date`, `weighted_etr`). Any policy date ≥ 2026-12-31 is silently truncated. **Latent today** (current max policy date is 2026-09-29, last valid_until is exactly 2026-12-31), but a 2027+ schedule would silently lose steps with no error. **Fix:** `last_date <- max(as.Date('2026-12-31'), max(pdl$date))` and add a `warning()`/`stop()` if `max(pdl$date) > last_date`; fix the comment.

#### Low — `bea_io_level` default disagrees across four sources (`out-2`)
The YAML comment annotates `'summary'` as `(default)`, the active value is `'detail'`, the code fallback is `'summary'` (`01_load_inputs.R:132`), and `run_scenario`'s docstring says `'detail' (default)` (`run_model.R:142-143`). `bea_io_level` materially changes outputs (73 vs ~400 commodities, 2024 vs 2017 data) and selects different downstream files. Documentation foot-gun only — at runtime the explicit `'detail'` is read. **Fix:** align all four; state that `'detail'` is the active choice and `'summary'` is only the bare code fallback.

#### Low — `inflation_2024_to_2025` documented-but-dead (`xc-5 coherence`)
`global_assumptions.yaml:14-16` defines `inflation_2024_to_2025: 0.027` with the comment "Applied to per-household cost outputs in distribution calculation" — but the key is **read nowhere** in any `.R` file, and `09_calculate_distribution.R` produces `cost_per_hh` with no inflation factor (its docstring says income is already in 2025 dollars). The comment is factually false (log files show the adjustment existed and was removed). **Fix:** delete the parameter and comment (dead), or re-apply the deflation if it belongs.

---

## 4. Cross-cutting observations

1. **(a) vs (b) discipline is the model's main coherence risk.** The pipeline correctly routes (b) to GTAP/USMM/prices, but the *time-varying revenue path* mixes a (b) level with an (a) profile (`loa-1`/`xc-1`), and the FY-window edge (`rev-1`) overstates pre-policy days. Both feed headline revenue. Fixing them together (and adding a single `noncompliance_active` branch in the revenue per-date scaling) would make the revenue module use the same (b) concept USMM already uses.

2. **IRF onset assumptions are fragile and unasserted.** `mac-1` and `mac-2` both stem from the surrogate assuming all IRF families share a single onset/placement convention. A family-level "first-nonzero quarter matches assumed native onset" assertion would have caught `mac-1` at re-extraction time.

3. **Silent-drop / coercion as a recurring anti-pattern.** `inner_join` on PCE keys (`out-3`/`xc-6`), missing `nrow>0` guard on the (b) loader (`xc--1`), `message()`-not-`stop()` on unmatched `tau_M` (`xc--2`), and Date-vs-character filters (`xc-2`/`xc--3`) are all currently dormant but each contradicts the explicit house rule that missing values must surface. They are cheap to harden and the rules exist precisely to catch the upstream-refactor data-shape changes.

4. **Two un-named copies of the same constant and several stale docs.** `30441` appears inline (`run_model.R:550`) duplicating a named CBO parameter; the price-formula narrative drifted across MEMORY/brief/memo while the code stayed consistent; `bea_io_level`'s default is stated four ways. The model's *code* is more trustworthy than its *prose* right now — narrative docs need a reconciliation pass.

5. **Reference-date approximations applied uniformly across time-varying schedules** (`R_alpha` in `xc-3`, and the (a)-profile scaling in `loa-1`) share a root: GTAP runs only at the reference date, and several quantities derived from that single solve are stretched across a multi-date schedule. Worth a documented statement of which quantities are reference-date approximations and why that is acceptable.

---

## 5. Simplification opportunities

| Opportunity | Files | Effort | Payoff |
|---|---|---|---|
| Extract a shared `summarize_ge_pce_decomp()` and call from both sites (`xc--1`) | `run_model.R:97-122` ↔ `io_price_model.R:1212-1233` | Low | Removes char-for-char duplicated decomp arithmetic; kills divergence risk |
| Collapse `calculate_country_etrs` / `_alpha` into one parameterized fn (defaults α=1) + module-scope `COUNTRY_CONFIG` (`xc--2`) | `02_calculate_etr.R:39-141,165-212` (+ `calculate_weighted_etr` country list) | Medium | Central ETR-weighting math in one place; α=1 reduction is provably bit-for-bit |
| Drop unused `inputs$imports_by_country`, `inputs$vgdp`, returned `qva`/`vom`, and the `get_imports_by_country()` call (`xc--3`) | `01_load_inputs.R:474-476`; `read_gtap.R:587,744-745` | Low | Removes wasted GTAP work + 4 misleading dead fields |
| Remove `source('src/12_export_excel.R')` from the core runner (`xc--4`) | `run_model.R:33` | Trivial | Decouples core pipeline from openxlsx/report layer |
| Delete dead `calculate_etr_increase()` (orphaned on arrival, stale `/100` convention) (`loa-2`) | `02_calculate_etr.R:215-223` | Trivial | Removes a misleading pp/fraction convention |
| Fix stale docstring → `calculate_prices()` no longer exists (`xc--5`) | `09_calculate_distribution.R:58` | Trivial | Points maintainers at the real producer |
| Resolve `consumption_share`: use or delete (`sec-3`) | `decile_parameters.csv`; `09_calculate_distribution.R` | Low | Removes latent "looks-load-bearing" column |
| Fix the false `# %||%` comment + the `%||%` crash (`cal-1`) | `calibrate.R:98`; `check_eta_reproduction.R:42-43` | Trivial | Un-breaks the common reproduction-gate SKIP path |

---

## 6. What I could NOT verify statically (needs a run or a domain owner)

1. **`mac-1`/`mac-2` magnitude.** The 3-quarter misalignment and the double-count are unambiguous from the IRF data and code, but the **pp impact on 2026 Q4-Q4 GDP/U-rate/PCE/fed-funds** requires running the surrogate. *Owner action:* run the `2026-04-06` scenario before/after the temp-onset fix and report the deltas.

2. **`pri-1` — is `domestic_pricing=0.5` the intended central case?** Git history and in-file docs say yes (memo stale); the memo says it was eliminated. This is a **policy/methodology decision for the model owner**, not resolvable from code. If "no," headline price effects are ~33% high.

3. **`loa-1`/`xc-1` realized bias.** Confirmed structurally; the *size* of the per-FY revenue distortion depends on how non-uniform eta′ is across products and how the product mix shifts across dates in real scenarios — needs a run comparing the (a)-scaled vs (b)-scaled revenue series.

4. **`xc-2` (alpha rate-vs-quantity).** Whether the rate-only correction is the *intended* near-term anchor (vs also dampening `qmwreg`) is a modeling-judgment call for the owner; the code is internally coherent either way.

5. **Legacy parity.** Per the refactor design, the pre-refactor pipeline is broken, so **no golden-output parity check** was possible for the 00a upstream path. Findings `xc--1` (untraded-sector crash) and `xc-5` (levels zero-fill) are behavioral divergences from the legacy producer that could only be confirmed against a legacy golden output that no longer runs.

6. **`sec-1` exact error.** Recoverable from `excel_distribution_raw.csv` (≤1.44% per decile), but the authoritative re-derivation of `scaling_factor` against the new income vector should be done by whoever owns the distribution calibration.

---

# Coverage Gap Review — Prioritized Follow-Up Tasks

## High priority — fresh working-tree edits under-scrutinized

1. **End-to-end trace of the new (a)/(b)/eta' fork — the largest uncommitted change, never centered by any reviewer.** The calibration reviewer covered eta' *production*; no one reviewed its *consumption*. Confirm the four legs stay consistent: GTAP shock (00a `ref_partner_gtap_b`), USMM impulse (05a `b_etr_increase`), post-sub prices (run_model `tau_M_b`/`etr_matrix_b`), and revenue compliance zero-out (04 `compliance_effect_applied`). Files: `src/00a_prepare_rate_inputs.R:451-720`, `src/01_load_inputs.R:342-396`, `src/run_model.R:248-256`, `src/05a_usmm_surrogate.R:455-466`, `src/04_calculate_revenue.R:120-134`.

2. **(b) BEA date-slice uses character compare while the (b) sector-matrix slice uses Date compare — same dual-convention the (a) reviewer flagged, duplicated and unflagged in the (b) block.** `read_sector_matrix_b` filters `date == inputs$gtap_reference_date` (Date) but the bea_b path filters `date == as.character(...)`. If one convention silently yields zero rows, post-sub price goes all-zero (the existing `01` finding noted (b) BEA lacks the nrow>0 guard — this is the upstream cause). File: `src/01_load_inputs.R:347-388`.

3. **Pre-sub price path still consumes (a) `tau_M` while post-sub now consumes (b) `tau_M_b` — verify this asymmetry is intended and that pre-sub "applied statutory" reporting is not meant to be eta'-adjusted.** The `update_state_of_tariffs.R` relabeling (Pre-Substitution → "Applied Statutory") signals a concept rename that should be checked against what each price leg actually feeds. Files: `src/run_model.R:230-256`, `src/io_price_model.R`, `src/update_state_of_tariffs.R`.

4. **decile_parameters income edit changes the regressivity *gradient*, not just per-HH levels.** Income moved down for low deciles and *up* for high deciles while `scaling_factor` (= consumption/income) was left stale — so the headline regressive gradient is biased in opposite directions across deciles, a stronger claim than the existing "off by <=1.5%" levels finding. Also confirm the coupled removal of `inflation_2024_to_2025` (now genuinely unreferenced) is fully consistent. Files: `resources/distribution/decile_parameters.csv`, `src/09_calculate_distribution.R:32-49`.

## Medium priority — never-centered pipeline modules

5. **`src/06_calculate_sectors.R` received zero findings — confirm it was actually reviewed.** `get_effect()` (line 61) returns the raw filtered vector with no length assertion: a missing or duplicated `aggregate_sector` label yields a zero-length or multi-element scalar silently. Also `calc_weighted_avg` and `get_effect` use `na.rm`-free sums but no coverage guard on the manufacturing-flag subsets.

6. **`src/08_calculate_foreign_gdp.R` uses two different region-mapping mechanisms in one function** — `region_codes` (line 64-65, `eu='eu'`) for the per-region scalars vs `ABBR_TO_GTAP[region]` (line 80) for the World/World-ex-USA aggregates. If these ever disagree, the published per-region GDP and the World aggregate are computed on inconsistent region definitions. Pairs with the existing run_model `30441` magic-constant finding (same dollar-conversion path).

7. **`read_gtap.R` parsing depth was assigned to price-model but findings only touch `io_price_model.R`.** No finding addresses GTAP `.sol`/HAR variable extraction robustness (mtax, qgdp, viws, vgdp parsing) — the trust boundary where all downstream revenue/macro/sector numbers originate. Worth a focused pass on column-order/label assumptions in the GTAP readers.

## Lower priority — standalone scripts and dimensions

8. **`src/12_export_excel.R` (878 lines, largest unreviewed file) and `src/add_s122_15_columns.R`** — output-only Excel formatting, sourced but not called in the model run. Low correctness risk, but the s122/relabeling churn in the working tree touches adjacent reporting; a light pass for hardcoded cell coordinates drifting against the new "Applied Statutory" labels is warranted.

9. **`src/weighted_avg_2025_prices.R` (253 lines) and `src/build_import_content_matrix.R` (284 lines) — never mentioned.** Confirm whether either feeds resources the live pipeline consumes (e.g. omega/import-content inputs to the price model); if so they are an unreviewed upstream of the price block, not just utilities.

10. **Economic double-counting was checked only at the eta'/revenue seam (finding exists) and the omega_D BEA-adjustment seam.** No finding examines the USMM→GTAP 16-quarter revenue blend (`src/07` + `src/helpers.R blend_usmm_gdp_deviation`) for double-counting of the substitution channel against the GTAP `mtax` revenue base. Given the critical temp-IRF shock-quarter bug already found in 05a, the blend arithmetic feeding dynamic revenue deserves an independent coherence pass. Files: `src/07_calculate_dynamic_revenue.R`, `src/helpers.R:96-100`.

11. **Calibration `%||%` crash is correctly scoped to the standalone harness, NOT the pipeline** (verified: 00a/00b/read_rate_panel load tidyverse, which attaches rlang's `%||%`). No follow-up needed on the pipeline side, but the existing check-stage finding should note `helpers.R` still never defines `%||%` — every standalone calibration entrypoint that does not attach rlang remains exposed.
