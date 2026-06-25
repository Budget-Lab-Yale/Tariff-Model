# Rate Tracker Cutover Status

Last updated: 2026-06-23

## Where Things Stand

The model has been cut over to the tariff-rate-tracker path only.

- Runtime no longer invokes or reads the old external rate engine.
- Scenarios must have a `rate_panel:` block in `config/scenarios/<scenario>/model_params.yaml`.
- Step 0a reads tracker snapshots/panels and writes model-ready artifacts to:
  `output/<scenario>/rate_inputs/`
- Downstream steps read only `rate_inputs/` plus the static baseline directory:
  `output/<scenario>/baseline/`
- Legacy scenario configs without `rate_panel:` were removed from `config/scenarios/`.

The two remaining configured scenarios are:

- `tracker_actual_2026-06-04`
- `tracker_actual_2026-06-13`

## How Time Variation Is Used

The tracker publishes a time path: one rate state per policy start date. The model
uses that time path in different ways depending on the output.

1. Main ETR, price, and GTAP-based metrics use one `gtap_reference_date`.
   `01_load_inputs.R` selects the rate matrix at that date. For the 06-13 run, this
   was set to `2026-11-10`, so the main `key_results.csv` ETR is the final/end-2026
   rate state, not the April rate.

2. Revenue uses the full time path.
   `04_calculate_revenue.R` day-weights the ETR increase within each fiscal year.

3. USMM/macro uses the full time path as step changes.
   The surrogate decomposes the schedule into permanent and intermediate components.

4. `daily_etr_levels.csv` is the easiest way to inspect the daily all-in pre-sub ETR
   path written by the model.

## What The 06-13 `actual` Run Represents

The 06-13 configured scenario currently points at:

```yaml
rate_panel:
  root: 'C:/Users/jar335/Documents/Interfaces/model_data/tariff-rate-tracker'
  vintage: 'latest_2026_06_13'
  weights_vintage: '2026-06-04_2'
  tracker_scenario: 'actual'
  baseline_date: '2025-01-01'
  interval_end: 'inclusive'

gtap_reference_date: '2026-11-10'
```

The raw tracker snapshots show that this `actual` series is not the intended
"Section 122 expires and new Section 301 fills in" policy path.

Import-weighted raw authority rates in the 06-13 bundle:

| Series/date | 232 | 301 | 301_cs | S122 | Total ETR |
|---|---:|---:|---:|---:|---:|
| actual 2026-04-06 | 6.35 | 1.47 | 0.00 | 4.01 | 11.8 |
| actual 2026-06-08 / 2026-07-24 regime | 6.18 | 1.47 | 0.00 | 4.01 | 10.5 |
| actual 2026-09-29 | 6.43 | 1.47 | 0.00 | 0.00 | 9.0 |
| actual 2026-11-10 | 6.43 | 1.56 | 0.00 | 0.00 | 9.1 |

After September, `actual` and `no_s122` are effectively the same with respect to
S122: `rate_s122` is zero. The `rate_301_cs` column is zero in these snapshots,
and regular `rate_301` only rises slightly by November. So the large post-July
ETR decline is coming from the tracker input, not from downstream model logic.

## Implication

Do not treat `latest_2026_06_13` / `actual` as the production analog of the old
`2026-04-06_s122perm` scenario. It appears to represent an actual/current-law path
where S122 drops out and is not replaced by a new 301 fill-in.

If you want the intended production scenario, generate a new tracker scenario that
explicitly contains the post-S122 replacement action.

## What To Generate Next

For the next tracker run, useful scenarios would be:

- `actual` or a named production scenario with the intended current-law path.
- A no-301 counterfactual for attribution.
- A no-S122 counterfactual for attribution.
- If relevant, an explicit S122-expiration scenario and an explicit S122-extended
  scenario, rather than relying on `actual` semantics.

The model can point at any named tracker scenario under:

```text
<root>/<vintage>/scenarios/<tracker_scenario>/
```

Set `tracker_scenario:` in `model_params.yaml` to that directory name. For example:

```yaml
rate_panel:
  root: 'C:/Users/jar335/Documents/Interfaces/model_data/tariff-rate-tracker'
  vintage: 'new_vintage_name'
  weights_vintage: '2026-06-04_2'
  tracker_scenario: 's122_replaced_by_301'
  baseline_date: '2025-01-01'
  interval_end: 'inclusive'

gtap_reference_date: 'YYYY-MM-DD'
```

`gtap_reference_date` must be one of the tracker scenario's snapshot start dates.
Use the final/end-2026 policy start date if the headline metrics should represent
the end-of-year rate state.

## Quick Checks After A New Tracker Run

Before running the full model, check the raw tracker path:

1. Confirm the scenario directory exists:

```powershell
Get-ChildItem C:\Users\jar335\Documents\Interfaces\model_data\tariff-rate-tracker\<vintage>\scenarios
```

2. Confirm the expected snapshot dates exist:

```powershell
Get-ChildItem C:\Users\jar335\Documents\Interfaces\model_data\tariff-rate-tracker\<vintage>\scenarios\<scenario>\snapshots
```

3. Check authority contributions around the key dates:

- April policy date
- S122 expiration/replacement date
- final/end-2026 reference date

In particular, verify that `rate_s122` falls when expected and that the intended
replacement authority, likely `rate_301` or `rate_301_cs`, rises enough to offset it.

## Running The Model

After creating/updating the scenario config:

```powershell
Rscript run.R <scenario_name>
```

GTAP may need to run outside the sandbox because the external executable writes
temporary log/work files. If Step 0a finishes but GTAP fails with an error like
"Unable to open temporary log file", rerun the GTAP step with normal filesystem
permissions.

