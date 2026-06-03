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
| D3 | **Levels only; model computes deltas.** Tracker emits absolute level panels for baseline + each scenario. Model differences them (`scenario − baseline`) at HS10×country, then aggregates. |
| D4 | **Tracker owns scenarios.** A scenario (a hypothetical policy path) is defined and run in the tracker (absorbing tariff-etrs's baseline-date + counterfactual-dates + reform-overlay model). The model's scenario config points at the resulting tracker output. |
| D5 | **Panel column schema is an upstream decision.** The model's *hard requirement* is the **total effective rate** (`rate_total`) at HS10×country×date. Per-authority decomposition (232/301/IEEPA/122/…) is optional-but-recommended: if the tracker exposes it, do so as a consistent set of typed `rate_<authority>` fraction columns that sum to `rate_total` — useful later for S122/temp validation and state-of-tariffs charts. Not a forced/reserved schema slot. |
| D6 | **Drop census-country/HTS2 outputs.** `deltas/levels_by_census_country(.csv)` and `*_hts2` are pure passthrough today (read in `01_load_inputs.R`, re-written verbatim in `11_write_outputs.R:423-441`, never computed on). Removed from the model's contract. |
| D7 | **Baseline is a single pinned static date.** Deltas are scenario-path minus a fixed reference snapshot (matches today's invariant in `01_load_inputs.R:263-266`). |
| D8 | **Canonical internal unit = fraction.** All rates are carried internally as fractions (e.g. `0.25`), with conversion to percent only at display/output. This resolves today's mismatch — raw matrices arrive as percentage points and get `/100` (`02_calculate_etr.R:392`), while `tau_M` is already fractional (`io_price_model.R:654`). The prepare step normalizes to fraction on read. |
| D9 | **Hard cutover.** When the tracker lands, `src/00_run_tariff_etrs.R` and the `tariff_etrs_path` config are deleted outright — no compatibility flag. The golden comparison (see Verification) is run in the same PR *before* deletion, against captured pre-refactor fixtures. |

## The new upstream surface (what the model reads)

The tracker publishes, per scenario, a versioned bundle the model reads directly (no
invocation). The bundle is **three artifacts**: a manifest plus two level panels. All rate
columns are **fractions** (D8).

- **`manifest.yml`** — what makes "pinned vintage" real. Required fields:
  `schema_version`, `tracker_sha` (git SHA / release ID), `source_data_vintage`,
  `hts_vintage`, `country_code_vocabulary`, `rate_unit` (must be `fraction`),
  `missing_value_policy`, and per-file `checksums`.
- **Baseline level panel** — static, single reference date.
  `hts10, country(cty_code), rate_total[, base_rate, authority cols…]`
- **Scenario level panel** — interval-encoded over counterfactual dates.
  `hts10, country, valid_from, valid_until, rate_total[, …]`

**Interval semantics (first-class).** The scenario panel encodes each rate over a half-open
interval `[valid_from, valid_until)`. The tracker must guarantee, per `hts10×country`:
**no gaps, no overlaps**, and **coverage through the full model horizon**. This is a hard
requirement — duration/coverage (not just point-in-time slices) drives revenue
(`04_calculate_revenue.R:34`), the USMM decomposition (`05a_usmm_surrogate.R:459`), daily ETR
output (`11_write_outputs.R:448`), and the 2025 weighted-price script
(`weighted_avg_2025_prices.R:74`).

Model-side pointer (replaces today's `tariff_etrs:` block in `model_params.yaml`):

```yaml
rate_panel:
  source: tracker-release        # decoupled read, not invocation
  vintage: '2026-04-06'          # pinned tracker release
  policy_scenario: '2026-04-06'  # which tracker policy panel in that release
                                 # (named distinctly from this model's scenario identity,
                                 #  which also carries gtap_reference_date/usmm/refund/retaliation)
  baseline: '2025-01-01'         # static baseline reference (D7)
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

The things this refactor *requires* the tracker to provide — the "what to build upstream" list:

1. **Scenario/reform mechanism** rich enough to express tariff-etrs's counterfactuals
   (baseline date + counterfactual dates + per-date policy-reform overlays), not just the
   current "disable an authority" model in `config/scenarios.yaml`.
2. **Per-scenario published bundle**: `manifest.yml` + baseline static level panel + scenario
   interval-encoded level panel, under a pinned vintage the model can point at. The manifest
   carries `schema_version`, `tracker_sha`, `source_data_vintage`, `hts_vintage`,
   `country_code_vocabulary`, `rate_unit` (`fraction`), `missing_value_policy`, and per-file
   `checksums` — this is what makes the pinned vintage verifiable rather than nominal.
3. **Panel granularity = HS10×country×date**, level (not delta), with at minimum
   `rate_total` as a fraction (authority decomposition optional-but-recommended per D5).
   Scenario panel must satisfy the interval invariants (half-open `[valid_from, valid_until)`,
   no gaps/overlaps, coverage through horizon).
4. **Import-weights publication decision** — recommend weights live with the aggregation
   (in this model); confirm whether the tracker also publishes weights as a shared resource.

## Verification (cutover gate)

Because the cutover is a hard cut (D9 — no live old path to diff against post-deletion), the
golden comparison runs **in the same PR, before deletion**, against **captured pre-refactor
fixtures**: snapshot the current run of scenario `2026-04-06` (`gtap_*_by_sector_country.csv`,
`bea_deltas_by_commodity.csv`, and `shocks.txt`), then run the new path and diff against those
saved fixtures.

Bundle/contract tests (on the tracker output, before aggregation):
- **Manifest** — schema version recognized, `rate_unit = fraction`, checksums match files.
- **Interval invariants** — per `hts10×country`: no gaps, no overlaps, coverage through horizon.
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
