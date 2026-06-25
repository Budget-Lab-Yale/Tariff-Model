# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Overview

This is the Yale Budget Lab Tariff Model, an R-based pipeline for tariff impact
analysis. It calculates effective tariff rates, consumer price effects, revenue
estimates, macroeconomic impacts, sector output effects, foreign GDP effects, and
distributional impacts.

## Architecture

The model reads a published tariff-rate-tracker bundle from each scenario's
`rate_panel:` block, converts the HS10-country rate panel into model-ready GTAP
and BEA artifacts, runs GTAP, then computes downstream price, revenue, macro,
sector, foreign GDP, and distribution outputs.

```
tariff-rate-tracker bundle -> Step 0a rate inputs -> GTAP + model pipeline -> results
```

## Directory Structure

```text
config/
  global_assumptions.yaml
  scenarios/{scenario_name}/
    model_params.yaml     # must include rate_panel
    retaliation/          # optional GTAP retaliation shocks

resources/
  baselines/
  mappings/
  rate_aggregation/

src/
  run_model.R
  00a_prepare_rate_inputs.R
  00b_run_gtap.R
  01_load_inputs.R
  02_calculate_etr.R
  io_price_model.R
  04_calculate_revenue.R
  05a_usmm_surrogate.R
  05_calculate_macro.R
  06_calculate_sectors.R
```

## Running the Model

Use the CLI runner from the repo root:

```bash
Rscript run.R tracker_actual_2026-06-13
Rscript run.R tracker_actual_2026-06-13 --markup constant_percentage
```

Options:

- `--markup <type>`: `constant_dollar` default, `constant_percentage`, or `average`
- `--bea-io-level <level>`: `detail` or `summary`

## Coding Style

- Use two-space indentation.
- Use single quotes for strings.
- Prefer tidyverse transforms over ad hoc loops where they keep the code clearer.
- Let missing values fail loudly unless a fallback is explicitly part of the model contract.
- Use snake_case for variables and functions.
- Keep scenario selection config-driven through `rate_panel:`.
