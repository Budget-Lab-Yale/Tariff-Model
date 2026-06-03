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
| D5 | **Panel column schema is an upstream decision.** The model's *hard requirement* is the **total effective rate** at HS10×country×date. Per-authority decomposition (232/301/IEEPA/122/…) is a nice-to-have the tracker may already expose; not forced by this model. |
| D6 | **Drop census-country/HTS2 outputs.** `deltas/levels_by_census_country(.csv)` and `*_hts2` are pure passthrough today (read in `01_load_inputs.R`, re-written verbatim in `11_write_outputs.R:423-441`, never computed on). Removed from the model's contract. |
| D7 | **Baseline is a single pinned static date.** Deltas are scenario-path minus a fixed reference snapshot (matches today's invariant in `01_load_inputs.R:263-266`). |

## The new upstream surface (what the model reads)

The tracker publishes, per scenario, a versioned bundle the model reads directly (no
invocation). Minimum contract:

- **Baseline level panel** — static, single reference date.
  `hts10, country(cty_code), rate_total[, base_rate, authority cols…]`
- **Scenario level panel** — interval-encoded over counterfactual dates.
  `hts10, country, valid_from, valid_until, rate_total[, …]`

Model-side pointer (replaces today's `tariff_etrs:` block in `model_params.yaml`):

```yaml
rate_panel:
  source: tracker-release      # decoupled read, not invocation
  vintage: '2026-04-06'        # pinned tracker release
  scenario: '2026-04-06'       # which scenario panel in that release
  baseline: '2025-01-01'       # static baseline reference (D7)
gtap_reference_date: '2026-09-29'   # unchanged; model-side slice for the single GTAP/I-O run
# usmm_dates, temp_component_dates, refund_2026, retaliation: unchanged (model knobs)
```

The model keeps `gtap_reference_date` slicing (which date feeds the single GTAP/I-O run)
and the full date panel for the USMM decomposition — unaffected by the move.

## What this model absorbs (new B-side aggregation layer)

A new step between "load panel" and "calculate ETR", e.g. `src/01b_aggregate_rates.R`,
that turns the raw panel into the artifacts downstream steps already expect:

1. **Deltas:** `delta = scenario_level − baseline_level` at HS10×country (per date).
2. **GTAP rollup:** import-weighted HS10×country → GTAP-sector × 8-partner
   (china/canada/mexico/uk/japan/eu/row/ftrow) → reproduces today's
   `gtap_deltas_by_sector_country` / `gtap_levels_by_sector_country` matrices in memory.
3. **BEA rollup:** HS10 → BEA-summary commodity → `bea_deltas` → `tau_M`
   (then the existing `disaggregate_tau_M` for detail-level tables).
4. **GTAP shocks:** write `shocks.txt` (`Shock tms("GTAP","PARTNER","USA") = VALUE;`)
   for `00b_run_gtap.R` to consume — moved from tariff-etrs's `write_shock_commands`.

Resources that move into this model (`resources/mappings/`):
- `hs10_gtap_crosswalk.csv`, `hs10_bea_crosswalk.csv`, `gtap_bea_crosswalk.csv`
- `country_partner_mapping.csv` (cty_code → partner)
- import weights for the rollup (model now owns the weight vintage/concordance choice —
  reuse the tracker's 2024 concordance-weighted import file).

Reused as-is (no change): `disaggregate_tau_M`, `resources/mappings/gtap_sectors.csv`,
`src/read_gtap.R` (reads native GTAP `.sol`), `src/00b_run_gtap.R` (now fed by the
model-generated `shocks.txt`).

## Code changes in this model (for when the tracker lands)

- **Retire** `src/00_run_tariff_etrs.R`; remove Step 0 + the `tariff_etrs_path` config and
  the output-dir rename logic.
- **Add** `src/01b_aggregate_rates.R` (the layer above) + crosswalk/weight resources.
- **Rewrite the tariff-etrs-output section of `src/01_load_inputs.R`** (`:192-332`) to read
  the tracker panel + run the aggregation, producing the same `inputs$*` objects
  downstream steps consume (`etr_matrix`, `levels_matrix`, `baseline_levels_matrix`,
  `bea_deltas`, `tau_M`, `*_by_date` panels). Keep the interval/`gtap_reference_date`
  slicing logic.
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
2. **Per-scenario published bundle**: baseline static level panel + scenario
   interval-encoded level panel, under a pinned vintage the model can point at.
3. **Panel granularity = HS10×country×date**, level (not delta), with at minimum
   `rate_total` (authority decomposition optional per D5).
4. **Import-weights publication decision** — recommend weights live with the aggregation
   (in this model); confirm whether the tracker also publishes weights as a shared resource.

## Verification (cutover gate)

Run the **existing** tariff-etrs path and the **new** tracker-panel path for the same
scenario (e.g. `2026-04-06`) and diff:

- `inputs$etr_matrix`, `inputs$levels_matrix`, `inputs$baseline_levels_matrix`
  (GTAP-sector × partner) — must match the old `gtap_*_by_sector_country.csv`.
- `inputs$tau_M` (BEA) — must match the old `bea_deltas_by_commodity.csv`-derived vector.
- Generated `shocks.txt` — line-for-line equal to tariff-etrs's `shocks.txt`.
- Then full `Rscript run.R 2026-04-06`: KEY RESULTS block (ETRs, price increases, revenue,
  macro) within rounding of the pre-refactor run.

Tolerances: exact for shocks/crosswalk rollups (deterministic); rounding-level for final
headline numbers.
