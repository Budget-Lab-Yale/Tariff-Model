# The Budget Lab's Tariff Model

An R-based pipeline that automates tariff impact analysis, calculating effective tariff rates (ETRs), consumer price effects, revenue estimates, and macroeconomic impacts.

## Quick Start

### From R Console

```r
# Set working directory to project root
setwd('C:/Users/jar335/Documents/Repositories/Tariff-Model')

# Load and run
source('src/run_model.R')
run_scenario('11-17')
```

### From Command Line

```bash
cd C:/Users/jar335/Documents/Repositories/Tariff-Model
Rscript run.R 11-17
```

## Workflow

The model pauses after generating MAUS shocks so you can run MAUS and drop in the
results, then it continues in the same run.

```bash
Rscript run.R 11-17
```

### MAUS Integration

When the run pauses:
1. Find the generated shocks at: `output/{scenario}/maus_inputs/shocks.csv`
2. Run MAUS with those inputs
3. Save MAUS output to: `config/scenarios/{scenario}/maus_outputs/quarterly.csv`
4. Return to the terminal and press Enter to continue

If `config/scenarios/{scenario}/maus_outputs/quarterly.csv` already exists, the
model skips the pause and continues.

Required MAUS output columns:
- `year`, `quarter`
- `GDP` (real GDP, billions)
- `LEB` (employment, millions)
- `LURC` (unemployment rate, %)

## Scenarios

Scenarios are stored in `config/scenarios/{scenario_name}/`:

```
config/scenarios/11-17/
  tariff_etrs/          # Tariff-ETRs configuration files
  other_models/         # GTAP output CSVs
  maus_outputs/         # MAUS output (quarterly.csv)
  model_params.yaml     # Price passthrough, shares, elasticities
```

To create a new scenario, copy an existing one and modify the configuration files.

## Output

Results are written to `output/{scenario}/`:
- `key_results.csv` - Summary of main results
- `revenue_annual.csv` - Annual revenue estimates
- `distribution.csv` - Impact by income decile
- `sectors.csv` - Sector output effects
- `products.csv` - Product-level price effects

## Requirements

Core model:
- R 4.0+
- R packages: tidyverse, yaml, HARr
- External: Tariff-ETRs repo at `../Tariff-ETRs`
- GTAP + GEMPACK tools: `gtapv7.exe` and `sltoht.exe` (paths set in `config/scenarios/*/model_params.yaml`)
- MAUS: uses the included surrogate at `resources/maus_surrogate/interpolators.rds` (for manual runs, install MAUS and write `config/scenarios/{scenario}/maus_outputs/quarterly.csv`)

Report generation (optional, for `--report` or `generate_report()`):
- R packages: openxlsx, httr2, jsonlite
- Pandoc installed and on PATH (used to convert Markdown to Word)
- Anthropic/Claude API key set in `ANTHROPIC_API_KEY` (e.g., `Sys.setenv()` or `.Renviron`)
- `reports/ybl_style.docx` for Word styling (recommended)

API key example (set once in `.Renviron`, or at session start via `Sys.setenv()`):
```
ANTHROPIC_API_KEY=your-api-key-here
```

Pandoc install: https://pandoc.org/installing.html

## Setup Checklist

- Install required R packages:
```r
install.packages(c('tidyverse', 'yaml', 'HARr'))
```
- For report generation:
```r
install.packages(c('openxlsx', 'httr2', 'jsonlite'))
```
- Confirm GTAP paths in `config/scenarios/*/model_params.yaml`
- Confirm Tariff-ETRs repo is available at `../Tariff-ETRs`
