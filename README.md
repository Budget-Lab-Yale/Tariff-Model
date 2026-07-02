# The Budget Lab Tariff Model

An R pipeline for estimating the economic effects of U.S. tariff policy:
effective tariff rates, consumer price effects, revenue, macro effects, sector
output effects, foreign GDP effects, and distributional impacts.

## How the pipeline fits together

Tariff analysis spans **two codebases** that coordinate through a versioned data
interface:

1. **`tariff-rate-tracker`** (upstream) — publishes a dated **rate bundle**: the
   HS10 × country statutory-rate panel, import weights, and a `manifest.json`,
   under `<shared>/model_data/Tariff-Rate-Tracker/<vintage>/`. It produces
   standalone content *and* the model-facing inputs consumed here.
2. **`tariff-model`** (this repo) — consumes a tracker bundle, runs GTAP and the
   downstream economics, and **publishes its own versioned outputs** for other
   analyses to consume.

The two coordinate on **scenarios** through the tracker's published bundle, and
every model run records exactly which tracker vintage + scenario produced it — so
any published result is traceable to its inputs. For an analysis that needs output
from both (e.g. a blog post), point it at the published outputs of (1) and (2).

### The interface convention

This repo follows the Budget Lab `model_data` interface convention (the same one
Tax-Simulator / Tax-Data use), configured under `config/interfaces/`:

- **`output_roots.yaml`** — the `production` (shared, durable) and `local`
  (scratch) filesystem roots. The model reads the tracker bundle from
  `<production>/model_data/Tariff-Rate-Tracker/` and publishes its results to
  `<production>/model_data/Tariff-Model/v<version>/`.
- **`interface_versions.yaml`** — this model's output `version`, plus the upstream
  `Tariff-Rate-Tracker` dependency (its default vintage + scenario).

Because the tracker root comes from `output_roots.yaml`, scenario configs are
**portable across machines** — they don't hardcode a path. (Setting
`TARIFF_RATE_TRACKER_ROOT` overrides the root if you need to.)

### Run stages

The entrypoint is `run.R`, which runs the pipeline orchestrated by
`src/run_model.R`:

| Step | Module | Description |
|------|--------|-------------|
| 0a | `00a_prepare_rate_inputs.R` | Reads the tracker bundle (resolved via the interface config) and writes model-ready rate matrices |
| 0b | `00b_run_gtap.R` | Runs the GTAP trade model |
| 1 | `01_load_inputs.R` | Loads baselines, mappings, assumptions, eta/alpha calibration, and model inputs |
| 2 | `02_calculate_etr.R` | Calculates weighted effective tariff rates |
| 3 | `run_model.R` + `io_price_model.R` | Computes pre-sub, post-sub, and GE price effects |
| 3a | `05a_usmm_surrogate.R` | Constructs the USMM surrogate macro response |
| 4 | `04_calculate_revenue.R` | Calculates conventional revenue |
| 5 | `05_calculate_macro.R` | Calculates macro effects |
| 6 | `06_calculate_sectors.R` | Calculates sector output effects |
| 7 | `07_calculate_dynamic_revenue.R` | Calculates dynamic revenue effects |
| 8 | `08_calculate_foreign_gdp.R` | Calculates foreign GDP effects |
| 9 | `09_calculate_distribution.R` | Calculates pre-sub and post-sub household burdens |
| 10 | `11_write_outputs.R` | Writes result CSVs to `output/<scenario>/results/` |
| 11 | `run_model.R` + `interfaces.R` | Publishes results to the shared interface tree with a dependency stamp |

### Dashboard data export (standalone, downstream of published runs)

The State of Tariffs report is an interactive dashboard (hosted in the
`budget-lab-interactives` repo). This model builds only the data side:
`update_dashboard.R` reads the published scenarios listed in
`config/dashboard.yaml` at one interface vintage and writes per-figure
long-format `data.csv` files (plus `manifest.json` + `dependencies.csv`) under
`<root>/model_data/Tariff-Model/v<version>/<vintage>/dashboard/`, for hand-copy
into the interactives repo via PR. It is a separate step from `run.R`.

```bash
Rscript update_dashboard.R --interface-vintage <V> [--write-local]
```

All logic lives in `src/13_export_dashboard.R`. (This replaced the retired
`12_export_excel.R` / `update_state_of_tariffs.R` Excel data-download pipeline.)

### Calibration (standalone, upstream of a run)

The noncompliance `eta` and substitution `alpha` parameters are produced by a
**standalone** harness (`calibrate.R`), *not* by `run_model.R`. A run consumes
their CSV outputs via the scenario's `noncompliance.eta_file` /
`substitution.alpha_file` pointers. Stages (see `calibrate.R --help`):

```
panel  ->  eta            # IMDB actuals panel -> eta_by_partner_gtap.csv
panel  ->  gtap-weights -> alpha   # GTAP across the calibration window -> alpha_parameters.csv
```

Scenarios with the `_no_eta_alpha` suffix are bridge variants that use a neutral
(zero) eta and disable the alpha correction, so they need no calibration.

## Requirements

| Software | Purpose |
|----------|---------|
| R 4.0+ | Main runtime |
| `tariff-rate-tracker` bundle | HS10-country rate panel + import weights (read via the interface config) |
| GTAP + GEMPACK | Trade model simulation and extraction |

```r
install.packages(c('tidyverse', 'yaml', 'HARr', 'openxlsx', 'jsonlite', 'arrow'))
```

Set up the two local configs (both are gitignored / machine-specific):

```bash
cp config/gtap_config.yaml.example config/gtap_config.yaml   # GTAP/GEMPACK paths
# edit config/interfaces/output_roots.yaml for your machine's shared + scratch roots
```

> **Running on the Yale HPC?** GTAP's GEMPACK binaries run under Wine in an
> Apptainer container, and runs go through Slurm. See
> [`docs/gtap_hpc_setup.md`](docs/gtap_hpc_setup.md) for the full setup.

## Usage

```bash
Rscript run.R tracker_actual_2026-06-25
Rscript run.R tracker_actual_2026-06-25 --markup constant_dollar --bea-io-level detail
Rscript run.R tracker_actual_2026-06-25 --write-local        # publish to scratch, not production
```

From an R session:

```r
source('src/run_model.R')
run_scenario('tracker_actual_2026-06-25')
```

| Option | Description |
|--------|-------------|
| `--markup constant_dollar` | Lower-bound markup assumption (default) |
| `--markup constant_percentage` | Upper-bound markup assumption |
| `--markup average` | Average of the upper and lower bounds |
| `--bea-io-level detail` | Detail BEA I-O tables (~400 commodities, default) |
| `--bea-io-level summary` | Summary BEA I-O tables (73 commodities) |
| `--vintage <id>` | Output vintage id (default: run timestamp) |
| `--write-local` | Publish under the `local` (scratch) root instead of `production` |

## Scenario config

Each scenario is a directory `config/scenarios/<scenario>/` with a
`model_params.yaml`. The tracker root is **not** in the scenario (it comes from
`output_roots.yaml`); the scenario pins the vintage + series and the calibration
pointers:

```yaml
rate_panel:
  vintage: '2026-06-25-14'      # tracker bundle vintage (omit to use the interface default)
  tracker_scenario: 'actual'    # 'actual', or a scenarios/<name> series (e.g. new_301)
  baseline_date: '2025-01-01'
  interval_end: 'inclusive'

noncompliance:
  eta_file: 'output/calibration/<scenario>/eta_by_partner_gtap.csv'
substitution:
  alpha_file: 'output/calibration/<scenario>/alpha/alpha_parameters.csv'
```

Step 0a writes model-ready artifacts to `output/<scenario>/rate_inputs/`; later
stages read only that directory.

## Outputs

Working artifacts are written to `output/<scenario>/` (gitignored):
`results/*.csv`, the GTAP solution under `gtap/`, and rate inputs under
`rate_inputs/`.

| File (`output/<scenario>/results/`) | Description |
|------|-------------|
| `key_results.csv` | Headline results |
| `goods_weighted_etrs.csv` | Weighted ETRs by goods category |
| `revenue_by_year.csv` | Conventional annual revenue |
| `dynamic_revenue_by_year.csv` | Revenue with GDP feedback |
| `macro_quarterly.csv` | Quarterly macro series |
| `sector_effects.csv` | Sector output effects |
| `foreign_gdp.csv` | Foreign GDP effects |
| `distribution.csv` / `distribution_postsub.csv` | Household burden by decile (pre / post-sub) |
| `pce_category_prices.csv` | Consumer-category price effects |
| `bea_commodity_prices.csv` | BEA commodity price effects |

### Published interface outputs

Each run also publishes to the shared, versioned tree so other analyses can
consume it like any sibling model:

```
<production>/model_data/Tariff-Model/v1/<vintage>/<scenario>/
  dependencies.csv   # ID, interface, version, vintage, scenario  (tracker lineage stamp)
  manifest.json      # model version, vintage, git commit, run options
  *.csv              # the run's result files
```

`dependencies.csv` records exactly which tariff-rate-tracker vintage + scenario
produced the run (e.g. `Tariff-Rate-Tracker, 2, 2026-06-25-14, actual`), so every
published output is traceable to its input.
