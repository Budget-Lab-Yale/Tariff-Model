# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Overview

This is the Yale Budget Lab Tariff Model, an R-based pipeline for tariff impact
analysis. It calculates effective tariff rates, consumer price effects, revenue
estimates, macroeconomic impacts, sector output effects, foreign GDP effects, and
distributional impacts.

## Architecture

The model consumes a published tariff-rate-tracker bundle, converts the
HS10-country rate panel into model-ready GTAP and BEA artifacts, runs GTAP, then
computes downstream price, revenue, macro, sector, foreign GDP, and distribution
outputs, and publishes the results to the shared `model_data` interface tree.

```
tariff-rate-tracker bundle -> Step 0a rate inputs -> GTAP + model pipeline -> results -> published interface output
```

Inputs and outputs follow the Budget Lab `model_data` interface convention
(`config/interfaces/`), shared with Tax-Simulator / Tax-Data:

- `output_roots.yaml` sets the `production`/`local` filesystem roots. The tracker
  bundle is read from `<production>/model_data/Tariff-Rate-Tracker/<vintage>/`;
  results publish to `<production>/model_data/Tariff-Model/v<version>/<vintage>/<scenario>/`.
- `interface_versions.yaml` sets this model's `version` and the tracker
  dependency's default vintage/scenario; a scenario's `rate_panel:` pins its own
  vintage/series. The tracker root is **not** hardcoded in scenarios (it resolves
  from `output_roots`, overridable via `TARIFF_RATE_TRACKER_ROOT`).
- Each published run writes `dependencies.csv` + `manifest.json` into the scenario
  dir, stamping the exact tracker vintage/scenario it consumed (traceable lineage).

The eta (noncompliance) and alpha (substitution) parameters come from a standalone
calibration harness (`calibrate.R`); a run consumes their CSV outputs via the
scenario's `noncompliance.eta_file` / `substitution.alpha_file`.

GTAP/GEMPACK is Windows-only; on the HPC it runs under Wine via Slurm — see
`docs/gtap_hpc_setup.md`.

## Directory Structure

```text
config/
  global_assumptions.yaml
  gtap_config.yaml          # GTAP/GEMPACK paths (gitignored; see .example)
  dashboard.yaml            # dashboard scenario set (id/label/default) for update_dashboard.R
  interfaces/
    interface_versions.yaml # this model's version + upstream tracker dependency
    output_roots.yaml       # production (shared) + local (scratch) roots
  scenarios/{scenario_name}/
    model_params.yaml       # rate_panel (vintage/series), noncompliance, substitution
    retaliation/            # optional GTAP retaliation shocks

resources/
  baselines/
  mappings/
  rate_aggregation/
  calibration/              # eta/alpha calibration inputs (treasury revenue, etc.)

calibrate.R                 # standalone eta/alpha calibration CLI (NOT part of run_model.R)
update_dashboard.R          # standalone dashboard-data export CLI (NOT part of run_model.R)

src/
  run_model.R
  interfaces.R              # resolve tracker dependency + publish to shared model_data
  00a_prepare_rate_inputs.R
  00b_run_gtap.R
  read_rate_panel.R         # tracker-bundle reader; resolves root from output_roots
  01_load_inputs.R
  02_calculate_etr.R
  io_price_model.R
  04_calculate_revenue.R
  05a_usmm_surrogate.R
  05_calculate_macro.R
  06_calculate_sectors.R
  13_export_dashboard.R     # per-figure dashboard CSV exporters (consumed by update_dashboard.R)
  calibration/              # IMDB panel, eta, gtap-weights, alpha stages
```

## Running the Model

Use the CLI runner from the repo root:

```bash
Rscript run.R tracker_actual_2026-06-25
Rscript run.R tracker_actual_2026-06-25 --markup constant_percentage
```

Options:

- `--markup <type>`: `constant_dollar` default, `constant_percentage`, or `average`
- `--bea-io-level <level>`: `detail` or `summary`
- `--vintage <id>`: output interface vintage (default: run timestamp)
- `--write-local`: publish under the `local` (scratch) root instead of `production`

Prereqs: `config/gtap_config.yaml` and `config/interfaces/output_roots.yaml` set
for the machine; calibrated `eta`/`alpha` present for scenarios that reference them
(produced by `calibrate.R`; `_no_eta_alpha` bridge variants need none). On the HPC,
submit via Slurm — see `docs/gtap_hpc_setup.md`.

## Coding Style

- Use two-space indentation.
- Use single quotes for strings.
- Prefer tidyverse transforms over ad hoc loops where they keep the code clearer.
- Let missing values fail loudly unless a fallback is explicitly part of the model contract.
- Use snake_case for variables and functions.
- Keep scenario selection config-driven through `rate_panel:`.
