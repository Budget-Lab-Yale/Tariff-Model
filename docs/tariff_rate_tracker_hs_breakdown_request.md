# Tariff-Rate-Tracker: HS-Based Product Breakdown Request

Audience: the `tariff-rate-tracker` Claude/Codex instance (companion to
`tariff_rate_tracker_output_contract_request.md` and
`tariff_rate_tracker_weights_request.md`).

Goal: publish a new daily rollup keyed to an **aggregated-HS product
classification** instead of GTAP sectors, so the "State of Tariffs" dashboard can
show the product breakdown of daily rates on an HS basis that maps cleanly to the
tariff actions (Section 232 steel / aluminum / autos / semiconductors, pharma,
etc.).

## Context / problem

`Tariff-Model` builds the dashboard's Tab 2 product breakdown
(`statutory-rates`, "by category" figure) as a **pass-through** of your
`actual/daily/daily_by_category.csv`, which is keyed on `gtap_code`. See
`build_daily_rates(daily_dir, 'by_category')` in
`src/13_export_dashboard.R` — we only relabel `gtap_code` → description, collapse
upper/lower-case duplicate GTAP rows, drop the `unmapped` residual, and ×100.

We want the breakdown to be **HS-based**, not GTAP-based. The daily
category-level aggregation (per-date, per-category import-weighted ETR across the
whole timeline) is computed inside the tracker; there is no HS-keyed daily file to
point at today. The raw HS10×country panel only exists in
`actual/snapshots/*/rates.parquet`. Replicating the daily rollup downstream would
duplicate your aggregation logic, so this belongs upstream.

## Requested output

Publish a new daily rollup alongside the existing ones, for both `actual` and each
scenario:

```text
<root>/<vintage>/actual/daily/
  daily_by_hs.csv
  daily_by_hs.parquet          # preferred, mirrors daily_by_category.parquet

<root>/<vintage>/scenarios/<name>/daily/
  daily_by_hs.csv
```

**Shape: identical to `daily_by_category.csv`, with `gtap_code` replaced by a
stable category code + label.** Keep every other column so the downstream branch
is a near-copy of the GTAP one:

| column | notes |
|--------|-------|
| `date` | same as daily_by_category |
| `category_code` | stable snake_case key from the table below (e.g. `iron_steel`) |
| `category_label` | human label (e.g. `Iron & steel`) |
| `mean_additional_exposed` | same definition as daily_by_category |
| `mean_total_exposed` | " |
| `mean_additional_all_pairs` | " |
| `mean_total_all_pairs` | " |
| `n_products_present` | product count in the category on that date |
| `n_pairs_present` | " |
| `revision` | same `basic` / projected revision label |
| `n_products_total` | " |
| `weighted_etr` | import-weighted ETR for the category (fraction, as today) |

Use the **same import-weight base and weighting method** already behind
`daily_by_category.weighted_etr`, just grouped by `category_code` instead of
`gtap_code`. Do **not** pre-multiply by 100 (downstream does the ×100), matching
the existing daily files.

## Classification: "Budget Lab Tariff HS Aggregation"

~21 categories plus a residual. Defined by HS chapter (first 2 digits) except for
the sub-chapter carve-outs marked with the HS4 heading. **Mutually exclusive and
exhaustive over HS chapters 01–97.**

| `category_code` | `category_label` | HS chapters / headings |
|-----------------|------------------|------------------------|
| `ag_live_animals` | Agriculture & live animals | 01–15 |
| `processed_foods` | Processed foods, beverages & tobacco | 16–24 |
| `minerals_ores` | Minerals & ores | 25–26 |
| `energy` | Energy (mineral fuels) | 27 |
| `pharmaceuticals` | Pharmaceuticals | 30 |
| `chemicals` | Chemicals (ex-pharma) | 28–29, 31–38 |
| `plastics_rubber` | Plastics & rubber | 39–40 |
| `leather` | Leather & furskins | 41–43 |
| `wood_paper` | Wood, paper & printing | 44–49 |
| `textiles_apparel_footwear` | Textiles, apparel & footwear | 50–67 |
| `stone_glass` | Stone, cement, ceramics & glass | 68–70 |
| `precious_metals` | Precious metals & stones | 71 |
| `iron_steel` | Iron & steel | 72–73 |
| `aluminum` | Aluminum | 76 |
| `other_base_metals` | Other base metals | 74–75, 78–83 |
| `semiconductors` | Semiconductors & electronic components | **8541, 8542** (HS4) |
| `electronics` | Electronics & electrical equipment | 85 **excl. 8541–8542** |
| `machinery` | Industrial machinery & computers | 84 |
| `motor_vehicles` | Motor vehicles & parts | 87 |
| `other_transport` | Other transport equipment | 86, 88, 89 |
| `instruments` | Precision instruments & optical | 90–92 |
| `misc_manufactures` | Miscellaneous manufactures | 93–97 |
| `unclassified` | Special / unclassified | 98–99 + anything not mapped above |

### Classification rules

1. **Most-specific prefix wins.** Apply the HS4 carve-outs (`semiconductors` =
   8541/8542) *before* the 2-digit chapter rule, so those headings do not also
   land in `electronics`.
2. **Classify on the underlying product HS (chapters 01–97), not the Chapter 99
   modification heading.** US HTS codes the *additional* 232/301/IEEPA duties under
   Chapter 99 provisions with the real product on a linked line. Attribute the rate
   to the product's own chapter — the same base you already use to assign
   `gtap_code`. Chapter 99 should appear in `unclassified` only for genuinely
   unresolvable lines, and ideally never carry import weight.
3. **Residual is explicit, not silent.** Anything with no chapter match →
   `unclassified`. Don't drop it.

### Optional finer carve-out (only if cheap)

If you want to expose the auto action more precisely, split `motor_vehicles` into
`passenger_vehicles` (8703) + `auto_parts` (8708) + `other_vehicles` (rest of 87).
Not required — whole-chapter 87 is already policy-meaningful. Tell us if you add it
so we size the chart legend accordingly.

## Acceptance / reconciliation checks

Please verify before publishing (these are the checks that catch a broken
weighting or a leaky partition):

1. **Ties to overall.** For every `date`/`revision`, the `n_products`- (or import-)
   weighted mean of `weighted_etr` across all `category_code`s equals
   `daily_overall.weighted_etr` for that date, to rounding. Same base as the GTAP
   rollup already satisfies.
2. **Partition is complete.** Sum of `n_products_present` across categories on a
   date equals the total product count for that date (no product counted twice, none
   dropped). `unclassified` should hold a small, explainable share.
3. **Carve-out precedence works.** `semiconductors` is non-empty and
   `electronics` excludes 8541/8542 (spot-check a date where semis carry duty).
4. **Scenarios use the identical classification** as `actual`.

## Downstream integration (our side, once you ship it)

Small change in `Tariff-Model/src/13_export_dashboard.R`:

- add a `which == 'by_hs'` branch in `build_daily_rates()` reading
  `daily_by_hs.csv` (mirror the `by_category` branch: NA-drop, ×100,
  `revision` → `projected`), keyed on `category_code`;
- carry `category_label` straight through (no `gtap_sectors.csv` join needed;
  the GTAP upper/lower-case dedup step drops out);
- register the new figure slug and point Tab 2's product breakdown at it.

No change needed to the rate panel, weights, or any other daily file.
