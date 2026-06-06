# Tariff-Rate-Tracker: Import-Weight Base Request

Audience: the `tariff-rate-tracker` Claude/Codex instance (companion to
`tariff_rate_tracker_output_contract_request.md`).

Goal: publish an **HS10 × country import-weight base keyed to the tracker's own
HS10 code universe**, so `Tariff-Model` can roll your rate panel up to GTAP
sectors / BEA commodities with an **exact** join instead of papering over a code
mismatch.

## Why we need this (the problem we found)

`Tariff-Model` consumes your rate panel at HS10 × country and import-weights it to
GTAP-45 × 8-partner and to BEA commodities, using its own import weights. Those
weights are a frozen 2024 Census extraction (`hs10_by_country_gtap_2024_con`).
Your rate panel is built on the **current HTS statistical-suffix vintage**. The two
10-digit code universes do not line up:

- Weights: 18,626 distinct HS10. Your panel: 19,856 distinct HTS10.
- Joining weights → your panel on exact `(hts10, country)` matches only **94.8% of
  import value**. **5.2% ($164B)** has no exact 10-digit match.
- The gap is a pure **statistical-suffix renumbering** — it recovers to 99.8% at
  the 8-digit heading. USITC/Census split/merged/renumbered 10th-digit suffixes;
  the 8-digit tariff headings are stable. Concrete examples in your panel vs the
  weights:
  - `8507600020` (Li-ion batteries, **$13.2B from China**) — your panel dropped
    `...0020`, now has `...0030`/`...0090`.
  - `8542310001` (semiconductors, ~$8B) — your panel **dropped `...0001` entirely**.
  - `30049092` (pharma) — suffixes renumbered (`...202`/`...291` gone).
  - `71131950` (jewelry) — `...090` → `...091`/`...095`.
- We can't fix this on our side by aggregating to HS8, because the suffix carries
  **real rate information**: 2.57% of multi-suffix headings have rates that vary by
  10th digit, and those headings hold **$542B (17.4%)** of import value (pharma,
  tobacco ch. 24, some electronics). A heading-mean fallback gets those wrong.
- We can't fix it by swapping weight vintages either — no Census flow extraction
  carries your full 19,856-code universe, because Census only reports lines that
  actually traded while you enumerate the whole schedule.

The clean fix is for **you** to key the weights to your own HS10 universe, because
you already own the schedule enumeration and the HTS-vintage logic that produced
the rate panel. Then our join is exact by construction.

## What to build

For each published bundle (the same `<root>/<vintage>/` you already produce), emit
**one import-weight file whose `(hts10, country)` keys are a superset of every
`(hts10, country)` key that appears in the rate-panel snapshots**.

The import flows are 2024 Census calendar-year customs-value imports (this is the
weight base we use today; keep it unless we agree to change the vintage). The work
is mapping those 2024 flows **forward onto your current HTS codes**: apply the same
USITC HTS-revision concordance you already use to build the schedule, so that value
on a retired 10-digit suffix is reassigned to its successor suffix(es) under your
current vintage. Codes that exist in your schedule but never traded simply get
`imports = 0` (we drop zero-weight rows; including them is fine).

### Layout

Weights are vintage-specific but **not** revision/scenario-specific (the same import
flows apply to `actual` and every scenario), so one file per bundle:

```text
<root>/<vintage>/weights/
  import_weights_hs10_country.parquet
  import_weights_hs10_country.csv.gz   # optional fallback
```

### Schema (`import_weights_hs10_country.parquet`)

Required:

```text
hts10        character   # MUST be drawn from the same code universe as the rate panel
country      character   # same code system as the rate panel's `country` (Census cty)
imports      double      # 2024 customs-value imports, USD (not $M; raw USD is fine)
```

Nice-to-have (lets us cross-check, but optional — we attach our own concordances):

```text
import_value_year   integer   # e.g. 2024
hts_vintage         character  # the HTS revision the codes are keyed to
```

Do **not** add GTAP/BEA codes — those concordances and the aggregation stay on our
side (separation of concerns; we own the bucketing). We only need flows keyed to
your codes.

## Acceptance criteria

1. **Exact join.** Every `(hts10, country)` row in a rate-panel snapshot with
   `imports > 0` has a matching row in the weight file — i.e. when we drive our
   rollup off the weights, **100% of import value matches an exact 10-digit code in
   the panel** (today it's 94.8%).
2. **No value lost in the forward-mapping.** Total `sum(imports)` after the
   2024→current concordance equals the original 2024 total (≈ $3,124B) to within
   rounding — reassigning suffixes must conserve value, not drop the retired ones.
3. **Reproducible.** The concordance step is scripted in the tracker build, not a
   manual remap, and re-runs deterministically for a new vintage.
4. The file is keyed `(hts10, country)`; one row per pair; no duplicates.

## If you can't do the forward-concordance soon

Lower-effort interim that still unblocks us: publish the **2024→current HTS10
crosswalk** you use internally as a standalone table —

```text
<root>/<vintage>/weights/hts10_revision_crosswalk.csv
  old_hts10, new_hts10, split_weight   # split_weight sums to 1 per old_hts10
```

— and we'll apply it to our existing weight file ourselves. But the concorded
weight file above is preferable, because it keeps the vintage logic with the node
that owns the schedule.

## One-line summary

Our weights and your rates are on different HTS suffix vintages; please publish an
HS10×country import-weight file keyed to **your** rate-panel codes (2024 flows
mapped forward through the HTS revision concordance) so our GTAP/BEA rollup joins
exactly.
