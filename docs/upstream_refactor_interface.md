# Upstream Refactor: tariff-model ↔ tariff-etrs interface review

**Goal:** deprecate `tariff-etrs`; have the redesigned `tariff-rate-tracker` produce
both the baseline tariff-parameter-parsing output *and* the detailed tariff-rate
series this model needs for baseline + scenario counterfactuals. Apply the
separation-of-concerns principle: **node A (the tracker) writes only what is
native to its operation; transformations that only node B (this model) needs
live in B.**

This document maps (1) exactly what this model consumes from tariff-etrs today,
(2) how it's invoked, (3) which pieces are "native to A" vs "B-only transforms,"
and (4) what the new tracker should emit + what this model should absorb.

---

## 1. What this model consumes from tariff-etrs today

Everything is read in `src/01_load_inputs.R` (the consumer) from two output
trees produced by a single tariff-etrs run:

- `output/{scenario}/tariff_etrs/`  → the **scenario / counterfactual** outputs
- `output/{scenario}/baseline/`     → the **baseline** outputs (written in the same run)

### 1a. Files consumed (exact schemas)

| # | File (under `tariff_etrs/` unless noted) | Schema | Keyed by | Loaded into | Downstream consumer |
|---|---|---|---|---|---|
| 1 | `gtap_deltas_by_sector_country.csv` | `gtap_code`, `china`, `canada`, `mexico`, `uk`, `japan`, `eu`, `row`, `ftrow` (+ optional `date`,`valid_from`,`valid_until`) | GTAP sector (45) × 8 partners [× date] | `inputs$etr_matrix` / `etr_matrix_by_date` | `02_calculate_etr.R` (pre-/post-sub ETR, VIWS weighting) |
| 2 | `gtap_levels_by_sector_country.csv` | same shape, absolute level | same | `inputs$levels_matrix` / `levels_matrix_by_date` | ETR levels / baseline ETR |
| 3 | `bea_deltas_by_commodity.csv` | `bea_code`, `etr_delta`, `total_imports` (+ optional `date`…) | BEA summary commodity [× date] | `inputs$bea_deltas` → `inputs$tau_M` | `io_price_model.R` (Boston Fed I-O price model) |
| 4 | `deltas_by_census_country.csv` *(optional)* | `cty_code`, `country_name`, `etr` | Census country | `inputs$census_country_deltas` | passthrough/reporting |
| 5 | `levels_by_census_country.csv` *(optional)* | `cty_code`, `country_name`, `level` | Census country | `inputs$census_country_levels` | passthrough/reporting |
| 6 | `shocks.txt` (flat, or per-date subdir `{date}/shocks.txt`) | GEMPACK lines: `Shock tms("GTAP","PARTNER","USA") = VALUE;` | GTAP sector × partner [× date] | read by `00b_run_gtap.R` | **GTAP CGE shock input** |
| 7 | `baseline/gtap_levels_by_sector_country.csv` | same as #2, **static only** (errors if `date` present) | GTAP sector × partner | `inputs$baseline_levels_matrix` | baseline ETR, delta reconstruction |
| 8 | `baseline/levels_by_census_country.csv` *(optional)* | same as #5 | Census country | `inputs$baseline_census_country_levels` | passthrough/reporting |

Notes:
- `tau_M` is built from #3 (`setNames(etr_delta, bea_code)`) and, when
  `bea_io_level == 'detail'`, disaggregated to detail BEA codes via a model-side
  crosswalk (`disaggregate_tau_M`). So the model already owns part of the BEA chain.
- The model also reads `resources/mappings/gtap_sectors.csv` (its own copy of GTAP
  sector attributes: `aggregate_sector`, `is_manufacturing`, `is_durable`, …).

### 1b. NOT part of the tariff-etrs interface (stay in this model)

- GTAP **CGE solution** files (`.sol`/`.sl4` HAR) read by `src/read_gtap.R` —
  native output of the GTAP run this model owns.
- `resources/gtap_baseline/` (VIWS baseline, import_baseline_dollars), CBO
  baselines, USMM IRFs, distribution params — all model-native baseline data.

---

## 2. How tariff-etrs is invoked today

`src/00_run_tariff_etrs.R::run_tariff_etrs(scenario, etrs_scenario, tariff_etrs_path)`:

1. `system2(Rscript, "src/main.R", "--scenario", etrs_scenario, "--output-dir", <abs output/{scenario}>)`
   run with cwd set to `tariff_etrs_path`.
2. tariff-etrs writes `output/{scenario}/{etrs_scenario}/` **and** `output/{scenario}/baseline/`
   in one run (baseline block confirmed in `scenario_workflow.R:205-245`).
3. This model **renames** `{etrs_scenario}/` → `tariff_etrs/` for downstream stability;
   `baseline/` is left as-is.

Config contract (`config/scenarios/{scenario}/model_params.yaml`):
```yaml
tariff_etrs:
  scenario: '2026-04-06'   # points at tariff-etrs/config/scenarios/2026-04-06
  baseline: 'baseline'
gtap_reference_date: '2026-09-29'   # which date's slice feeds the single GTAP run
```
- `tariff_etrs_path` lives in `config/global_assumptions.yaml` (default `../Tariff-ETRs`).
- **Time-varying:** if outputs carry a `date` column / per-date `shocks.txt` subdirs,
  `01_load_inputs.R` slices to `gtap_reference_date` for the single-run GTAP/I-O path
  while retaining the full `*_by_date` panels for the USMM decomposition.

---

## 3. Separation-of-concerns analysis (the crux)

**Native to A (a tariff *rate* engine):** parse statutory tariff policy → compute
effective tariff *rates* (levels) at its native granularity (HS10 × country × date,
decomposed by authority), applying stacking, USMCA/MFN/metal-content/exemption logic.
This is exactly what both repos already do at HS10×country.

**B-only transforms currently living in A (should move to this model):**

| Concern in tariff-etrs today | Why it's B-only | Move to model? |
|---|---|---|
| **HS10→GTAP 45-sector × 8-partner aggregation** (`aggregate_countries_to_partners`, `gtap_import_weights.csv`, partner mapping) | Exists *only* because this model runs GTAP. Aggregation scheme + import weights are modeling choices owned by B. | **Yes** |
| **GTAP `shocks.txt` generation** (`write_shock_commands`) | Pure GTAP CGE input format — meaningless outside this model. | **Yes** |
| **HS10→BEA commodity aggregation** (`bea_deltas_by_commodity.csv`, `hs10_bea_crosswalk.csv`, `gtap_bea_crosswalk.csv`) | Exists *only* to feed B's Boston-Fed I-O price model. B already disaggregates `tau_M` to detail BEA itself. | **Yes** |
| **Delta computation** (counterfactual − baseline, then aggregate) | A "delta vs baseline" is a comparison B wants; if B holds both level panels it can difference them. Lets B control reference baseline + aggregation order. | **Yes** (emit levels; derive deltas in B) |
| Census-country / HTS2 rollups | Arguably reporting-native to a "rate tracker" (it already emits `daily_by_country`, `daily_by_authority`). | Optional — keep in tracker as reporting |

**Consequence:** the tracker's contract to this model shrinks to **one native
artifact** — a rate panel — and this model grows a new aggregation layer.

---

## 4. Recommended target design

### 4a. What the new tracker should emit (native, B-agnostic)

A **level rate panel** at native granularity, for baseline and each scenario/
counterfactual, interval-encoded (the tracker already does this natively via
`rate_timeseries.rds` with `valid_from`/`valid_until` — a better fit than
tariff-etrs's bolt-on `date` column):

```
hts10, country(cty_code), valid_from, valid_until,
  rate_total, rate_232, rate_301, rate_ieepa_recip, rate_ieepa_fent,
  rate_s122, rate_201, rate_other, base_rate, metal_share, usmca_eligible, ...
```

Emit this for:
- the **baseline** policy path, and
- each **scenario** (counterfactual policy path), at the counterfactual dates.

No GTAP sectors, no BEA codes, no shocks, no deltas — those are B's job. The
authority decomposition (already present) is the natural native granularity and
lets B aggregate / re-weight however it needs.

Format: Parquet/CSV.gz keyed `(hts10, country, valid_from)`. The tracker already
produces almost exactly this internally — the work is *exposing it as the contract*
rather than the dense per-authority `statutory_rates.csv.gz` it currently exports
for tariff-etrs to re-process.

### 4b. What this model should absorb (new B-side layer)

Move into `tariff-model` (likely a new `src/02x_aggregate_rates.R` + `resources/mappings/`):
- `hs10_gtap_crosswalk.csv`, `hs10_bea_crosswalk.csv`, `gtap_bea_crosswalk.csv`
- import weights for the rollup (model now owns the weight vintage/concordance choice)
- HS10×country → GTAP-sector×partner import-weighted aggregation → produces the
  current `gtap_*_by_sector_country` matrices internally
- HS10 → BEA-commodity aggregation → produces `tau_M` directly (folds in the
  existing `disaggregate_tau_M` step)
- delta = scenario_level − baseline_level (at HS10×country, then aggregate)
- `shocks.txt` writer (the `tms(...)` formatter) feeding `00b_run_gtap.R`

`00_run_tariff_etrs.R` becomes `00_run_tariff_rate_tracker.R` (or the panel is read
directly if the tracker publishes a versioned release the model points at).

### 4c. Scenario-mechanism unification (design risk)

The two repos model "scenario" differently and the tracker's current mechanism is
**weaker** than what this model relies on:

- **tariff-etrs scenarios**: rich policy-reform overlays + multiple counterfactual
  dates (e.g. metal-232 overhaul, pharma-232, S122 perm vs temp) — see
  `config/scenarios/{name}/scenario.yaml` + `reforms/` overlays.
- **tracker scenarios** (`config/scenarios.yaml`): mostly "disable an authority"
  + a few patch overrides — not expressive enough to represent the model's
  counterfactuals.

The redesigned tracker must absorb tariff-etrs's overlay/counterfactual-date model
(baseline date + list of counterfactual dates, each optionally carrying a policy
reform overlay) to produce the per-scenario level panels this model needs.

---

## 5. Open questions to resolve before building

1. **Coupling mode:** does the model invoke the tracker via CLI (as today) or
   consume a published, versioned tracker release (parquet) it points at? The
   latter decouples vintages but needs a release/version contract.
2. **Granularity of the panel handed over:** full HS10×country (≈19k × 240, large)
   vs a pre-thinned panel? B's aggregation wants HS10×country to weight correctly,
   so probably full — confirm size/perf is acceptable as a handoff artifact.
3. **Import weights ownership:** the tracker already has `hs10_by_country_gtap_2024_con.rds`.
   If aggregation moves to B, does the weight file move too, or stay published by
   the tracker as a separate resource? (Recommend: weights live with the
   aggregation, i.e. in B.)
4. **Census-country / HTS2 reporting outputs** (#4,#5,#8): keep as tracker reporting,
   or also move to B? They're only passthrough/reporting here.
5. **Time-varying baseline:** today baseline levels must be static. Tracker is
   natively interval-encoded; decide whether baseline stays a single date or becomes
   a path.
6. **`gtap_reference_date` slicing** stays a model concern (which date feeds the
   single GTAP/I-O run vs the full USMM date panel) — unaffected by the move.

---

*Generated as a planning reference; not loaded by the pipeline.*
