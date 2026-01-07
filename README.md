# Yale Budget Lab Tariff Model

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

The model requires MAUS to be run externally, so you run it twice:

```bash
# First run - generates MAUS inputs, then stops
Rscript run.R 11-17

# Run MAUS externally with the generated shocks...

# Second run - completes the model
Rscript run.R 11-17
```

### MAUS Integration

After the first run:
1. Find the generated shocks at: `output/{scenario}/maus_inputs/shocks.csv`
2. Run MAUS with those inputs
3. Save MAUS output to: `config/scenarios/{scenario}/maus_outputs/quarterly.csv`
4. Run the model again - it will detect the MAUS output and continue

Required MAUS output columns:
- `year`, `quarter`
- `gdp_baseline`, `gdp_tariff`
- `employment_baseline`, `employment_tariff`
- `urate_baseline`, `urate_tariff`

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

- R 4.0+
- tidyverse, yaml packages
- External: Tariff-ETRs repo at `../Tariff-ETRs`
