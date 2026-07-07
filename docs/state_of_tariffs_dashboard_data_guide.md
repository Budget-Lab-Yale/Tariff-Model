# Tariffs Dashboard — Data Guide

This document describes the proposed tariff dashboard webpage design and defines the structure for the underlying data.

## Underlying data

```
<root>/model_data/Tariff-Model/v<version>/<vintage>/dashboard/
  manifest.json
  dependencies.csv
  data/
    statutory-rates/<slug>/data.csv
    default-scenario/<slug>/data.csv
    alternative-scenarios/<slug>/data.csv
```

`<root>` is the shared production tree; `<vintage>` is a Tariff-Model publish timestamp (e.g. `2026070114`). The Johns supply this filepath for Sylva.

Every `data.csv` is long-format.

**`manifest.json`** — the only place that says which scenario is the default. It has a `scenarios` array copied from `config/dashboard.yaml`, each with an `id`, `label`, `short_label`, and `default: true/false`. It also records `tracker_vintage` (which tracker data the run used), `git_commit`, and `published_at`. The individual `data.csv` files in `default-scenario/` do not carry a scenario id column (you need the manifest to know which scenario they came from).

**`dependencies.csv`** — describes the upstream interface it consumed (Tariff-Model itself + Tariff-Rate-Tracker). For recordkeeping purposes only.

## Recurring columns

| Column | Meaning |
|---|---|
| `time` | ISO date, line charts only |
| `category` | bar/table row label (already human-readable, e.g. "China", "Agriculture") |
| `series` | sub-line or sub-bar label within a category/time (e.g. "Pre-substitution") |
| `value` | the number. Percent unless a figure says otherwise (see `unit` on summary-statistics) |
| `substitution` | `presub` / `postsub` — pre- vs post-substitution estimate |
| `dimension` | `sector` / `country` — the Figure 4/5 toggle, `gdp-by-category` only |
| `scenario` | model scenario id — present on `statutory-rates` and `alternative-scenarios` figures only |
| `measure` | `level` / `delta_vs_default` — the levels-vs-deltas toggle, `alternative-scenarios` only |
| `projected` | 0/1 — drives the grey "future projections" band, `statutory-rates` only |
| `country_code` / `category_code` / `gtap_code` | machine key behind the display `category`, for stable joins/colors |

Everything is validated on write (no NAs, no non-finite values, required columns present) — if a figure looks wrong, it's wrong in the model output, not a frontend parsing issue.

---

# Tab 1) Introduction

Intro + outline of dashboard, and changes since last update.

No data files. Words only. QUESTION FOR SYLVA: How best should we produce text for this piece? (All the text, really.) Like should the text itself be in the underlying data interface? In sharepoint? How do we think about that part of this?

---

# Tab 2) Daily Statutory Rates

All four figures are daily line charts under `statutory-rates/`, styled after the [tariff-rate-tracker blog](https://budgetlab.yale.edu/research/introducing-tariff-rate-tracker-open-source-tool-daily-effective-tariff-rates): dashed vertical lines at major tariff policy changes, and a grey band over future projections.

The grey band is driven by the `projected` column (0/1) — shade any date where `projected == 1`. The dashed policy-change lines have no data file today; that needs a small new CSV (date + label) that doesn't exist yet. TK.

Every figure also carries a `scenario` column (see Recurring columns).

## Overall

`statutory-rates/daily-rate-overall` — `series`: `total`, `additional`.

## By Authority

`statutory-rates/daily-rate-by-authority` — `series`: `base`, `section_232`, `section_301`, `ieepa`, `fentanyl`, `section_122`, `section_201`, `other`.

## By Trading Partner

`statutory-rates/daily-rate-by-country` — `series`: display name; `country_code`: group key (China / Canada / Mexico / EU / Japan / UK / free-trade partners / rest-of-world).

## By Product

`statutory-rates/daily-rate-by-category` — `series`: GTAP sector description; `gtap_code`: key.

---

# Tab 3) Projected Tariff Effects: Default Policy Scenario

These figures follow [The State of US Tariffs, April 8 2026](https://budgetlab.yale.edu/research/state-us-tariffs-april-8-2026). All show the single default scenario (per `manifest.json`), under `default-scenario/`.

## Overview

Some text.

## Summary of results (Table 1 from above)

Pre- and post-substitution

`default-scenario/summary-statistics` — 12 fixed metrics. Columns `series` (carries the "Pre-substitution" / "Post-substitution" split), `unit` (pct/usd/usd_bn/pp), `metric`.

## ETRs (Table 2 from above)

Pre- and post-substitution, by country, with a total row.

`default-scenario/etr-by-country` — `substitution` = the pre/post split; `category` = country. **The total row is already present** as `country_code == 'total'`.

## Consumer prices across PCE categories (Figure 7 from above)

Pre- and post-substitution, by PCE spending category, with total bars at the very top.

`default-scenario/consumer-prices` — `substitution` = pre/post; `category` = PCE category; `group` = Durable goods / Nondurable goods / Services. **The total is included** as the `category == 'Total'` row (also `group == 'Total'`) — the overall PCE price increase, matching the headline on summary-statistics. Render it as the top bars.

## Distribution of household income, by decile, as a share of ATT income (Figure 6 from above)

Toggle a "total" column on and off, with a different color (or other clear delineation).

`default-scenario/distribution-pct-income` — `category` = decile `"1"`–`"10"` plus a `"Total"` row (all-household burden, aggregate cost ÷ aggregate income); `substitution` = pre/post; `value` already negative (burden convention). Render `category == 'Total'` as the toggleable total.

## Distribution of household income, by decile, in 2025 dollars (Figure 6 from above)

Toggle a "total" column on and off, with a different color (or other clear delineation).

`default-scenario/distribution-dollars` — same shape as above, dollar units; the `"Total"` row is the average household cost (matches the per-household cost on summary-statistics).

## US Real GDP effects (Figure 3 from above)

`default-scenario/real-gdp` — line chart. 44 rows, 2025Q1–2035Q4, mid-quarter dates in `time`.

## US Real GDP effect, by category (Figures 4 and 5 from above)

Toggle by Sector or by Country group. Move the total category to the far left and give it a different color (or other clear delineation).

`default-scenario/gdp-by-category` — `dimension` = the Sector/Country toggle (`sector` / `country`); `category_code` = key. **The total for each side is already included** (`overall_gdp` for sectors, `world` for countries) — filter it out and render it separately on the far left.

## Estimated Revenue Effect (Table 3 from above)

A bar graph with two sets of bars, dynamic and conventional. Toggle a "total" column on and off, with a different color (or other clear delineation).

`default-scenario/revenue-by-year` — `series` = `conventional` / `dynamic` (the two sets of bars); `category` = fiscal year string. **The "total" is already present** as the `category == 'Total'` row — render it separately for the toggle.

---

# Tab 4) Projected Tariff Effects: Alternative Policy Scenarios

Same 8 figures as Tab 3, under `alternative-scenarios/`, but every row is stacked across **all** scenarios in `config/dashboard.yaml` and tagged with two extra columns:

- `scenario` — which policy scenario the row belongs to.
- `measure` — `level` / `delta_vs_default`, the toggle between showing 1) `level`, i.e., each scenario's own effects relative to the January 2025 baseline, and 2) `delta_vs_default`, the "delta" or incremental effect relative to the default scenario.

## Overview

Some text.

## Summary of results

Same as Tab 3, but add super-columns for scenarios.

`alternative-scenarios/summary-statistics` — same columns as Tab 3, plus `scenario` (the super-columns) and `measure`.

## ETRs by Scenario

Switch from the by-country break-out to switching between countries, with rows = scenarios. (Best for comparison.)

`alternative-scenarios/etr-by-country` — **no new data needed.** Same file as Tab 3's ETR figure with `scenario`/`measure` added; just pivot it: pick one `country_code`, put `scenario` on the rows instead of country on the x-axis.

## Consumer prices across PCE categories

Same as Tab 3, but add super-columns for scenarios.

`alternative-scenarios/consumer-prices` — Tab 3 columns + `scenario`/`measure`. Includes the `category == 'Total'` row, as in Tab 3.

## Distribution of household income, by decile, as a share of ATT income

Same as Tab 3, with bars for different scenarios. Toggle a "total" column on and off, with a different color (or other clear delineation).

`alternative-scenarios/distribution-pct-income` — Tab 3 columns + `scenario`/`measure`. Includes the `"Total"` row, as in Tab 3.

## Distribution of household income, by decile, in 2025 dollars

Same as Tab 3, with bars for different scenarios. Toggle a "total" column on and off, with a different color (or other clear delineation).

`alternative-scenarios/distribution-dollars` — Tab 3 columns + `scenario`/`measure`. Includes the `"Total"` row, as in Tab 3.

## US Real GDP effects, by year

Same as Tab 3, with lines for different scenarios.

`alternative-scenarios/real-gdp` — Tab 3 columns + `scenario`/`measure`.

## US Real GDP effect, by category

Same as Tab 3, with bars for different scenarios. Toggle by Sector or by Country group. Move the total category to the far left and give it a different color (or other clear delineation).

`alternative-scenarios/gdp-by-category` — Tab 3 columns + `scenario`/`measure`. `dimension` toggle and included total rows work exactly as in Tab 3.

## Estimated Revenue Effects

Two figures on the page, one dynamic and one conventional, with bars for different scenarios. Toggle a "total" column on and off, with a different color (or other clear delineation).

`alternative-scenarios/revenue-by-year` — Tab 3 columns + `scenario`/`measure`. For the two figures, split on the `series` column. The `category == 'Total'` row is already present for the toggle.

---

# Tab 5) Methodology

Some text. TK.
