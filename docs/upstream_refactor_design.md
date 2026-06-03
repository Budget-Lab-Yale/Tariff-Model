# Refactor Design: tariff-model ↔ tariff-rate-tracker

*(deprecating tariff-etrs; redesigning the upstream rate source)*

## Context

`tariff-etrs` is being deprecated. The redesigned `tariff-rate-tracker` will become the
single upstream source of tariff-rate data — producing both its native
parameter-parsing output **and** the detailed rate series this model needs for baseline
and scenario counterfactuals.

The organizing principle for the split: **node A (the tracker) writes only what is
native to its operation — effective tariff *rates* at its native granularity. Any
transformation that exists only because node B (this model) needs it (GTAP-sector
rollups, BEA-commodity rollups, GTAP shock files, deltas-vs-baseline) lives in B, not A.**

Today that boundary is violated: tariff-etrs computes HS10×country rates *and* does the
GTAP/BEA aggregation, delta computation, and GTAP `shocks.txt` generation that only this
model consumes. This refactor pulls those B-only transforms down into this model and
reduces the upstream contract to a single native artifact: a rate panel.

This doc captures the decided shape of that boundary and the surfaces to build. A
companion interface map of the *current* dependency lives at
`docs/upstream_refactor_interface.md`.

## Decisions

| # | Decision |
|---|---|
| D1 | **No invocation.** This model no longer calls the tracker. The tracker is run independently and publishes a pinned, versioned output bundle per scenario; the model just reads it. `src/00_run_tariff_etrs.R` is retired. |
| D2 | **Full HS10×country panel handoff.** The tracker hands over the raw, interval-encoded **level** panel at HS10×country×date. All GTAP-sector and BEA-commodity aggregation, crosswalks, import weights, and `shocks.txt` generation move **into this model**. |
| D3 | **Levels only; model computes deltas.** Tracker emits absolute level (effective-rate) time series for current-law (`actual/`) + each scenario. Model differences them (`scenario − baseline`) at HS10×country, then aggregates. The "baseline" is a slice of the `actual/` series at the reference date — see D7. |
| D4 | **Tracker owns scenarios.** A scenario (a hypothetical policy path) is defined and run in the tracker (absorbing tariff-etrs's baseline-date + counterfactual-dates + reform-overlay model). The model's scenario config points at the resulting tracker output. |
| D5 | **Panel column schema is an upstream decision.** The model's *hard requirement* is the **post-stacking total effective rate** (`rate_total`) at HS10×country×date — the combined ETR after the tracker's stacking step, not the pre-stacking per-authority components. Per-authority decomposition (232/301/IEEPA/122/…) is optional-but-recommended; the tracker's RATE_SCHEMA panel *already carries* typed per-authority `rate_*` columns natively (AuthoritySpec), so the breakdown is available — consume it as fractions that combine (post-stacking) to `rate_total`. Useful later for S122/temp validation and state-of-tariffs charts. |
| D6 | **Drop census-country/HTS2 outputs.** `deltas/levels_by_census_country(.csv)` and `*_hts2` are pure passthrough today (read in `01_load_inputs.R`, re-written verbatim in `11_write_outputs.R:423-441`, never computed on). Removed from the model's contract. |
| D7 | **Baseline is a single pinned static date — a slice of `actual/`, not a separate artifact.** The tracker does not publish a standalone static baseline panel; it publishes the current-law `actual/` time series. `00a` slices `actual/` at the fixed reference date to form the delta base (matches today's invariant in `01_load_inputs.R:263-266`). Deltas are scenario-path minus that fixed snapshot. |
| D8 | **Canonical internal unit = fraction.** All rates are carried internally as fractions (e.g. `0.25`), with conversion to percent only at display/output. This resolves today's mismatch — raw matrices arrive as percentage points and get `/100` (`02_calculate_etr.R:392`), while `tau_M` is already fractional (`io_price_model.R:654`). The prepare step normalizes to fraction on read. |
| D9 | **Hard cutover.** When the tracker lands, `src/00_run_tariff_etrs.R` and the `tariff_etrs_path` config are deleted outright — no compatibility flag. The golden comparison (see Verification) is run in the same PR *before* deletion, against captured pre-refactor fixtures. |

## The new upstream surface (what the model reads)

The tracker publishes, per vintage, the bundle the model reads directly (no invocation). It maps
onto the tracker's AuthoritySpec "Output layout":
`<root>/Tariff-Rate-Tracker/<vintage>/{actual, scenarios/<name>}/`. All rate columns are
**fractions** (D8); the tracker's native unit, so no conversion on read. Artifacts:

- **`manifest`** (per vintage + per scenario) — what makes "pinned vintage" real. The tracker
  already plans a `manifest.json` recording the scenario *recipe* (`base`, `baseline_mode`,
  operations, `adjustment_params`, parent vintage). Reconcile that with the *consumption-contract*
  fields the model needs in the **same** manifest: `schema_version`, `tracker_sha`,
  `source_data_vintage`, `hts_vintage`, `country_code_vocabulary` (= Census codes / `cty_code`),
  `rate_unit` (`fraction`), `missing_value_policy`, per-file `checksums`. (One `manifest.json`, not a
  separate `.yml`.)
- **`actual/` series** — current-law effective-rate time series (`actual/timeseries/rate_timeseries.parquet`
  + `daily/`). `00a` slices it at the baseline reference date to form the delta base (D7).
  `hts10, country(cty_code), valid_from, valid_until, rate_total[, rate_<authority>…]`
- **`scenarios/<name>/` series** — the counterfactual, same schema, interval-encoded over the
  scenario's effective dates.

Format is **parquet** (`timeseries/rate_timeseries.parquet`) + **csv** (`daily/daily_*.csv`); `00a`
reads parquet via the R `arrow` package.

**Interval semantics (first-class).** Each rate is encoded over a half-open interval
`[valid_from, valid_until)`, per `hts10×country`: **no gaps, no overlaps**, and **coverage through
the last policy-change date the model needs** (the model extrapolates the final ETR forward itself —
`compute_fy_weighted_etr_increase`, `04_calculate_revenue.R:22-28` — so it does *not* require panel
coverage all the way to FY2035). This drives duration-dependent steps: revenue
(`04_calculate_revenue.R:34`), the USMM decomposition (`05a_usmm_surrogate.R:459`), daily ETR output
(`11_write_outputs.R:448`), the 2025 weighted-price script (`weighted_avg_2025_prices.R:74`).
**Coordination:** AuthoritySpec admits the tracker has not yet converged its end-date convention
(IEEPA invalidation is exclusive-end; the expiry splitter is inclusive-end — its line 244); the
published series must settle on the half-open convention above.

Model-side pointer (replaces today's `tariff_etrs:` block in `model_params.yaml`):

```yaml
rate_panel:
  root: '<shared root>/Tariff-Rate-Tracker'  # filesystem read, not invocation
  vintage: '2026-04-06'           # pinned tracker vintage dir (or 'latest')
  tracker_scenario: '<name>'      # the published scenarios/<name>/ folder ('actual' = current-law).
                                  # The name encodes the tracker's full {policy × assumptions ×
                                  # baseline_mode × base} combination — distinct from this model's
                                  # scenario identity (gtap_reference_date/usmm/refund/retaliation).
  baseline_date: '2025-01-01'     # slice point in actual/ for the delta base (D7)
gtap_reference_date: '2026-09-29'   # unchanged; model-side slice for the single GTAP/I-O run
# usmm_dates, temp_component_dates, refund_2026, retaliation: unchanged (model knobs)
```

The model keeps `gtap_reference_date` slicing (which date feeds the single GTAP/I-O run)
and the full date panel for the USMM decomposition — unaffected by the move.

## What this model absorbs (new B-side aggregation layer)

A new **pre-GTAP** step, `src/00a_prepare_rate_inputs.R`, turns the raw tracker panel into the
artifacts downstream steps expect. It must run **before** GTAP, because GTAP (Step 0b) consumes
`shocks.txt` (`00b_run_gtap.R:152`) and runs *before* `load_inputs` (Step 1) — see the new step
order below. The step does the whole job in one place:

0. **Resolve + validate** the pinned tracker bundle: check the manifest (schema version, rate
   unit, checksums) and the interval invariants (no gaps/overlaps, coverage through horizon);
   normalize all rates to fraction (D8).
1. **Deltas:** `delta = scenario_level − baseline_level` at HS10×country (per date).
2. **GTAP rollup:** import-weighted HS10×country → GTAP-sector × 8-partner
   (china/canada/mexico/uk/japan/eu/row/ftrow) → reproduces today's
   `gtap_deltas_by_sector_country` / `gtap_levels_by_sector_country` matrices.
3. **BEA rollup:** HS10 → BEA-summary commodity → `bea_deltas` → `tau_M`
   (then the existing `disaggregate_tau_M` for detail-level tables).
4. **GTAP shocks:** write `output/{scenario}/rate_inputs/shocks.txt`
   (`Shock tms("GTAP","PARTNER","USA") = VALUE;`) for `00b_run_gtap.R` to consume — moved from
   tariff-etrs's `write_shock_commands`.

The derived artifacts (GTAP/BEA matrices, `bea_deltas`, `tau_M`, the `*_by_date` panels) are then
read by `load_inputs` (Step 1), which is rewritten to consume them instead of tariff-etrs output.

**New step order:**
`read params → 00a prepare rate inputs → 0b GTAP → 1 load inputs → 2+ unchanged`

Resources that move into this model, vintaged under a dedicated
`resources/rate_aggregation/{vintage}/` (not loose in `resources/mappings/`, and kept separate
from the price-model's BEA I/O crosswalks in `resources/io` + `resources/io/detail` loaded at
`io_price_model.R:125`):
- `hs10_gtap_crosswalk.csv`, `hs10_bea_crosswalk.csv`, `gtap_bea_crosswalk.csv`
- `country_partner_mapping.csv` (cty_code → partner)
- HS10×country import weights for the rollup (model now owns the weight vintage/concordance choice —
  reuse the tracker's 2024 concordance-weighted import file).

Reused as-is (no change): `disaggregate_tau_M`, `resources/mappings/gtap_sectors.csv`,
`src/read_gtap.R` (reads native GTAP `.sol`), `src/00b_run_gtap.R` (now fed by the
model-generated `shocks.txt`).

## Code changes in this model (for when the tracker lands)

- **Retire** `src/00_run_tariff_etrs.R`; remove Step 0 + the `tariff_etrs_path` config and
  the output-dir rename logic (hard cut, D9 — no compatibility flag).
- **Add** `src/00a_prepare_rate_inputs.R` (the pre-GTAP layer above) + the vintaged
  crosswalk/weight resources, and wire it into `run_scenario` before `run_gtap`
  (`run_model.R:186`).
- **Rewrite the tariff-etrs-output section of `src/01_load_inputs.R`** (`:192-332`) to read
  the derived artifacts produced by `00a` instead of tariff-etrs output, still populating the
  same `inputs$*` objects downstream steps consume (`etr_matrix`, `levels_matrix`,
  `baseline_levels_matrix`, `bea_deltas`, `tau_M`, `*_by_date` panels). Keep the
  interval/`gtap_reference_date` slicing logic.
- **Drop** census-country/HTS2 loads (`01_load_inputs.R:271-299`) and their writes
  (`11_write_outputs.R:423-441`).
- **Update** `src/00b_run_gtap.R` to read the model-generated `shocks.txt` (it already
  reads `output/{scenario}/.../shocks.txt`; only the producer changes).
- **Update** `CLAUDE.md` (external-dependency section).

## Open items for the upstream (tracker) build

The things this refactor *requires* the tracker to provide — the "what to build upstream" list.
Each is cross-checked against the tracker's AuthoritySpec design doc
(`../tariff-rate-tracker/docs/authority_spec.md`); see the next section for the full alignment.

1. **Scenario/reform mechanism** rich enough to express tariff-etrs's counterfactuals — ✅
   **addressed by AuthoritySpec.** Its operation engine (`add_program`, `set_rate`,
   `set_country_scope`, `base` pins, `baseline_mode`) meets and exceeds the old baseline-date +
   counterfactual-dates + reform-overlay model (it also adds new coverage and re-scoping, which
   tariff-etrs could not do).
2. **Per-vintage published bundle**: a `manifest.json` + `actual/` current-law series + each
   `scenarios/<name>/` series, under a pinned vintage the model can point at. **Reconcile** the
   tracker's planned recipe-manifest with the consumption-contract fields the model needs
   (`schema_version`, `tracker_sha`, vintages, `country_code_vocabulary`, `rate_unit`,
   `missing_value_policy`, per-file `checksums`) in one manifest — this is what makes the pinned
   vintage verifiable rather than nominal.
3. **A post-stacking total effective rate** (`rate_total`, fraction) per
   `hts10×country×interval` — **hard requirement.** This feeds directly into the tracker's
   *unresolved* output-contract decision (AuthoritySpec impl req 2, "Define the output contract":
   whether the resolved-program table is internal-only, persisted as snapshot extras, or replaces
   the downstream decomposition). The model needs the combined post-stacking rate published as a
   consumable column; per-authority `rate_*` decomposition alongside it is recommended (D5).
   Scenario series must satisfy the interval invariants (half-open `[valid_from, valid_until)`,
   no gaps/overlaps, coverage through the model's last policy-change date).
4. **Import-weights publication decision** — recommend weights live with the aggregation (in this
   model); confirm whether the tracker publishes its 2024 Census import-weight file (used for ETR
   weighting at `hts10×country`, AuthoritySpec `08:389`) as a shared resource we can reuse.
5. **Stable scenario naming.** The model pins `tracker_scenario` to a published `scenarios/<name>/`
   folder, so the names must be deterministic and stable. AuthoritySpec leaves how the
   `policy × assumptions × baseline_mode × base` cross-product is enumerated/named as a deferred
   item — settle it before the model points at it.

## Verification (cutover gate)

Because the cutover is a hard cut (D9 — no live old path to diff against post-deletion), the
golden comparison runs **in the same PR, before deletion**, against **captured pre-refactor
fixtures**: snapshot the current run of scenario `2026-04-06` (`gtap_*_by_sector_country.csv`,
`bea_deltas_by_commodity.csv`, and `shocks.txt`), then run the new path and diff against those
saved fixtures.

Bundle/contract tests (on the tracker output, before aggregation):
- **Manifest** — schema version recognized, `rate_unit = fraction`, checksums match files.
- **Interval invariants** — per `hts10×country`: half-open `[valid_from, valid_until)`, no gaps,
  no overlaps, coverage through the model's last policy-change date.
- **Coverage** — every country/HTS in the panel resolves in the import-weight and crosswalk
  resources (no silent drops in the rollup).

Aggregation/golden diffs (new path vs captured fixtures):
- `inputs$etr_matrix`, `inputs$levels_matrix`, `inputs$baseline_levels_matrix`
  (GTAP-sector × partner) — must match the saved `gtap_*_by_sector_country.csv`.
- `inputs$tau_M` (BEA) — must match the saved `bea_deltas_by_commodity.csv`-derived vector.
- Generated `shocks.txt` — line-for-line equal to the saved `shocks.txt`.
- Then full `Rscript run.R 2026-04-06`: KEY RESULTS block (ETRs, price increases, revenue,
  macro) within rounding of the pre-refactor run.

Tolerances: exact for shocks/crosswalk rollups (deterministic); rounding-level for final
headline numbers.

## Alignment with the tracker's AuthoritySpec build

The upstream redesign is specified in `../tariff-rate-tracker/docs/authority_spec.md`
(AuthoritySpec). It was designed against the same A/B boundary from the upstream side and
**validates this plan** — the two docs independently arrived at the same vintaged-bundle +
manifest + scenario-as-delta shape, and AuthoritySpec explicitly defers the
"revenue/ETR-delta decomposition" to its consumer (us). Key points of intersection:

**What it confirms.** The split holds: the tracker owns statutory structure + ETR adjustment
params (`adjustment_params`: USMCA utilization, metal-content method, exemption shares) and
publishes **effective** rates; this model owns the GTAP/BEA rollups, deltas, `shocks.txt`,
revenue, and macro. Granularity (`hts10×country`), unit (fraction), country vocabulary (Census
`cty_code`), and per-authority decomposition columns are all native to the tracker output.

**What it refined in this doc.**
- *Baseline is a slice, not an artifact* (D3, D7): the tracker publishes `actual/` (current-law
  time series); the static baseline is a slice `00a` takes, not a separate panel.
- *We read the post-stacking total* (D5, open item 3): `rate_total` is the combined ETR after
  the tracker's stacking step.
- *Layout/format/manifest/config pointer* updated to the AuthoritySpec output layout
  (`<root>/Tariff-Rate-Tracker/<vintage>/{actual,scenarios/<name>}`, parquet + csv, one
  `manifest.json`, `tracker_scenario` pointer).

**Open coordination items (upstream decisions that gate this model's cutover):**
1. **Output contract (AuthoritySpec impl req 2).** *The* critical one: the tracker must decide
   which published artifact carries the post-stacking `rate_total` (and optional `rate_*`
   decomposition). This determines exactly what `00a` reads. Our hard requirement (open item 3)
   is the input to that decision.
2. **Interval end-date convention** (AuthoritySpec line 244) — currently unconverged (exclusive
   vs inclusive). The published series must use the half-open convention this model assumes.
3. **Milestone dependency.** This model's refactor depends specifically on AuthoritySpec
   **migration step 5** (restructure published output so scenarios are first-class on disk) plus
   the impl-req-2 output-contract decision. Earlier AuthoritySpec steps (the internal
   spec/stacking refactor) are parity-gated upstream and do not block us; the *published surface*
   does.
4. **Stable scenario naming** (open item 5) and **horizon/weights for 2027+ scenarios**
   (AuthoritySpec open questions: `series_horizon` is `2026-12-31`, import weights fixed at 2024
   Census) — both must be settled before scenarios beyond the current horizon are pinned.
