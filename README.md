# Yale Budget Lab Tariff Model

An R-based pipeline that calculates the economic impacts of U.S. tariff policies, including effective tariff rates (ETRs), consumer price effects, revenue estimates, GDP/employment impacts, and distributional effects by income decile.

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Command Line Usage](#command-line-usage)
- [Pipeline Steps](#pipeline-steps)
- [Directory Structure](#directory-structure)
- [Scenarios](#scenarios)
- [Outputs](#outputs)
- [MAUS Integration](#maus-integration)
- [Report Generation](#report-generation)

## Overview

The model orchestrates multiple components to produce tariff impact estimates:

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  Tariff-ETRs    │     │      GTAP       │     │      MAUS       │
│  (ETR Matrix)   │     │  (Trade Model)  │     │  (Macro Model)  │
└────────┬────────┘     └────────┬────────┘     └────────┬────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                                 ▼
                    ┌─────────────────────────┐
                    │     Tariff Model        │
                    │  (This Repository)      │
                    └────────────┬────────────┘
                                 │
                                 ▼
                    ┌─────────────────────────┐
                    │   Key Results Output    │
                    │  - ETRs & Price Effects │
                    │  - Revenue Estimates    │
                    │  - GDP/Employment       │
                    │  - Sector Effects       │
                    │  - Distribution         │
                    └─────────────────────────┘
```

## Prerequisites

### Required Software

| Software | Version | Purpose |
|----------|---------|---------|
| R | 4.0+ | Core runtime |
| Tariff-ETRs repo | - | ETR calculations (sibling directory) |
| GTAP + GEMPACK | v7 | Trade model simulations |

### Required R Packages

```r
# Core packages (required)
install.packages(c('tidyverse', 'yaml', 'HARr'))

# Report generation (optional)
install.packages(c('openxlsx', 'httr2', 'jsonlite'))
```

### External Dependencies

1. **Tariff-ETRs**: Clone to `../Tariff-ETRs` (sibling directory)
2. **GTAP + GEMPACK**: See [GTAP Setup](#gtap-setup) section below
3. **MAUS surrogate**: Pre-trained model at `resources/maus_surrogate/interpolators.rds`

### For Report Generation (Optional)

- [Pandoc](https://pandoc.org/installing.html) installed and on PATH
- Anthropic API key set as environment variable:
  ```bash
  # In .Renviron or shell profile
  ANTHROPIC_API_KEY=your-api-key-here
  ```

## Installation

```bash
# Clone the repository
git clone https://github.com/Budget-Lab-Yale/Tariff-Model.git
cd Tariff-Model

# Clone Tariff-ETRs as sibling
cd ..
git clone https://github.com/Budget-Lab-Yale/Tariff-ETRs.git
cd Tariff-Model

# Install R packages
Rscript -e "install.packages(c('tidyverse', 'yaml', 'HARr'))"
```

## GTAP Setup

The model requires GTAP (Global Trade Analysis Project) and GEMPACK for trade simulations. Follow these steps to configure GTAP for your system.

### 1. Install Required Software

| Component | Source | Purpose |
|-----------|--------|---------|
| **RunGTAP** | [GTAP Website](https://www.gtap.agecon.purdue.edu/products/rungtap/) | GTAP model executable (gtapv7.exe) |
| **GEMPACK** | [COPSMODELS](https://www.copsmodels.com/gempack.htm) | Solution extraction tools (sltoht.exe) |
| **GTAP Database** | Included with RunGTAP | Data files (sets.har, basedata.har, default.prm) |

### 2. Configure GTAP Paths

Edit `config/gtap_config.yaml` with your installation paths:

```yaml
# Path to GTAP executable (gtapv7.exe)
gtap_executable: 'C:/runGTAP375/gtapv7.exe'

# GTAP working directory
gtap_work_dir: 'C:/runGTAP375/work'

# GTAP data directory (contains sets.har, basedata.har, default.prm)
gtap_data_dir: 'C:/runGTAP375/tbltarv7'

# GTAP auxiliary files directory
gtap_aux_dir: 'C:/runGTAP375'

# Path to sltoht executable (GEMPACK)
sltoht_executable: 'C:/GP/sltoht.exe'
```

### 3. Verify Installation

The model will automatically validate your GTAP configuration on startup. If components are missing, you'll see detailed error messages:

```
============================================================
GTAP SETUP ISSUES DETECTED
============================================================

- GTAP executable not found: C:/runGTAP375/gtapv7.exe
  -> Install RunGTAP from https://www.gtap.agecon.purdue.edu/

- sltoht executable not found: C:/GP/sltoht.exe
  -> Install GEMPACK from https://www.copsmodels.com/gempack.htm

Please update config/gtap_config.yaml with correct paths.
============================================================
```

### Common GTAP Installation Paths

| System | Typical RunGTAP Location | Typical GEMPACK Location |
|--------|--------------------------|--------------------------|
| Windows | `C:/runGTAP375/` | `C:/GP/` |
| Windows (user) | `C:/Users/{name}/Documents/GTAP/` | `C:/GP/` |

### Required GTAP Files

The model needs these files in your `gtap_data_dir`:

| File | Description |
|------|-------------|
| `sets.har` | GTAP region/sector definitions |
| `basedata.har` | Baseline trade flow data |
| `default.prm` | Default parameter values |

## Command Line Usage

### Basic Syntax

```bash
Rscript run.R <scenario_name> [options]
```

### Options

| Option | Description |
|--------|-------------|
| `--manual-maus` | Use manual MAUS workflow instead of surrogate model |
| `--report` | Generate State of Tariffs report after model run |
| `--report-date YYYY-MM-DD` | Date for report header (required with `--report`) |
| `--policy-changes "text"` | Policy changes description for report (optional) |

### Examples

```bash
# Run model with default settings (uses MAUS surrogate)
Rscript run.R 11-17

# Run with manual MAUS workflow (pauses for external MAUS run)
Rscript run.R 11-17 --manual-maus

# Run and generate report
Rscript run.R 11-17 --report --report-date 2025-11-17

# Run with report and policy changes note
Rscript run.R 11-17 --report --report-date 2025-11-17 --policy-changes "Added steel exemptions"

# List available scenarios (run without arguments)
Rscript run.R
```

### Running from R Console

```r
source('src/run_model.R')

# Basic run
run_scenario('11-17')

# With manual MAUS
run_scenario('11-17', use_maus_surrogate = FALSE)
```

## Pipeline Steps

The model executes these steps in sequence:

| Step | Module | Description |
|------|--------|-------------|
| 0 | `00_run_tariff_etrs.R` | Calls Tariff-ETRs to generate ETR matrix |
| 0b | `00b_run_gtap.R` | Runs GTAP trade model simulation |
| 1 | `01_load_inputs.R` | Loads all inputs (baselines, parameters) |
| 2 | `02_calculate_etr.R` | Calculates weighted effective tariff rates |
| 3 | `03_calculate_prices.R` | Computes consumer price effects |
| 4a | `04a_generate_maus_inputs.R` | Generates MAUS shock inputs |
| 4 | `04_calculate_revenue.R` | Estimates tariff revenue (conventional) |
| 5a | `05a_predict_maus.R` | Predicts GDP/employment via surrogate |
| 5 | `05_calculate_macro.R` | Computes macro effects (GDP, unemployment) |
| 6 | `06_calculate_sectors.R` | Calculates sector output effects |
| 7 | `07_calculate_dynamic_revenue.R` | Adjusts revenue for GDP feedback |
| 8 | `08_calculate_foreign_gdp.R` | Estimates foreign country GDP impacts |
| 9 | `09_calculate_distribution.R` | Distributes costs by income decile |
| 10 | `10_calculate_products.R` | Product-level price effects |
| 11 | `11_write_outputs.R` | Writes results to CSV files |

## Directory Structure

```
Tariff-Model/
├── run.R                      # Command-line entry point
├── src/
│   ├── run_model.R            # Main orchestrator
│   ├── 00_run_tariff_etrs.R   # Step 0: ETR calculation
│   ├── 00b_run_gtap.R         # Step 0b: GTAP simulation
│   ├── 01_load_inputs.R       # Step 1: Load inputs
│   ├── ...                    # Steps 2-11
│   ├── helpers.R              # Shared utilities
│   └── read_gtap.R            # GTAP HAR file reader
├── config/
│   ├── gtap_config.yaml       # Global GTAP installation paths
│   ├── global_assumptions.yaml # Model-wide parameters
│   └── scenarios/
│       ├── 11-17/             # Example scenario
│       │   ├── tariff_etrs/   # Tariff-ETRs config (232.yaml, ieepa_*.yaml)
│       │   ├── retaliation/   # Retaliation shock definitions
│       │   ├── maus_outputs/  # MAUS output (quarterly.csv)
│       │   └── model_params.yaml  # Scenario-specific settings
│       └── {other-scenarios}/
├── resources/
│   ├── baselines/             # CBO baseline projections
│   ├── mappings/              # GTAP sector crosswalks
│   ├── gtap/                  # GTAP CMF template and mappings
│   ├── maus_surrogate/        # Pre-trained MAUS surrogate
│   └── distribution/          # Consumption weight data
├── output/
│   └── {scenario}/
│       ├── results/           # CSV output files
│       ├── gtap/              # GTAP run outputs
│       ├── tariff_etrs/       # ETR matrix outputs
│       ├── maus_inputs/       # Generated MAUS shocks
│       ├── images/            # Generated figures
│       └── report/            # Generated reports
└── reports/
    ├── report_instructions.md # Report generation docs
    └── tariff_report_template.docx
```

## Scenarios

### Available Scenarios

Run `Rscript run.R` without arguments to list available scenarios:

```
Available scenarios:
  1-8
  1-8-ex-ieepa
  1-8-ex-ieepa-refund
  11-17
  11-17-ex-ieepa
  1-11_hypothetical
```

### Creating a New Scenario

1. Copy an existing scenario directory:
   ```bash
   cp -r config/scenarios/11-17 config/scenarios/my-scenario
   ```

2. Edit tariff parameters in `tariff_etrs/`:
   - `232.yaml` - Section 232 tariffs
   - `ieepa_fentanyl.yaml` - IEEPA fentanyl tariffs
   - `ieepa_reciprocal.yaml` - IEEPA reciprocal tariffs
   - `other_params.yaml` - Other tariff parameters

3. Edit scenario-specific parameters in `model_params.yaml`:
   ```yaml
   gtap:
     include_retaliation: true  # Include retaliation shocks

   refund_2026: 0.0  # Optional refund amount in billions
   ```

   > **Note:** GTAP installation paths are configured globally in `config/gtap_config.yaml`, not per-scenario.

4. (Optional) Add retaliation shocks in `retaliation/shocks.txt`

5. Run the scenario:
   ```bash
   Rscript run.R my-scenario
   ```

## Outputs

Results are written to `output/{scenario}/results/`:

| File | Description |
|------|-------------|
| `key_results.csv` | Summary of headline results |
| `revenue_by_year.csv` | Annual revenue estimates (conventional) |
| `dynamic_revenue_by_year.csv` | Revenue with GDP feedback adjustment |
| `distribution.csv` | Cost burden by income decile |
| `sector_effects.csv` | Output changes by sector |
| `product_prices.csv` | Product-level price effects |
| `macro_quarterly.csv` | Quarterly GDP/employment projections |
| `foreign_gdp.csv` | GDP effects on trading partners |
| `goods_weighted_etrs.csv` | Weighted ETRs by goods category |

### Key Results Summary

After a successful run, the model prints a summary:

```
----------------------------------------------------------
KEY RESULTS
----------------------------------------------------------
Pre-substitution ETR increase:  17.45%
Post-substitution ETR increase: 12.31%
Pre-sub price increase:         2.234%
Post-sub price increase:        1.567%
Pre-sub per-HH cost:            $3,800
Post-sub per-HH cost:           $2,700
10-yr conventional revenue:     $3,450B
10-yr dynamic effect:           -$890B
10-yr dynamic revenue:          $2,560B
----------------------------------------------------------
```

## MAUS Integration

### Default: Surrogate Model

By default, the model uses a pre-trained MAUS surrogate (`resources/maus_surrogate/interpolators.rds`) to predict GDP and employment effects without requiring an external MAUS run.

### Manual MAUS Workflow

For custom MAUS runs, use `--manual-maus`:

```bash
Rscript run.R 11-17 --manual-maus
```

The model will:
1. Generate shock inputs at `output/{scenario}/maus_inputs/shocks.csv`
2. Pause and display instructions
3. Wait for you to run MAUS externally
4. Resume when you save MAUS output to `config/scenarios/{scenario}/maus_outputs/quarterly.csv`

#### Required MAUS Output Format

```csv
year,quarter,GDP,LEB,LURC
2025,1,21500.5,158.2,4.1
2025,2,21550.0,158.5,4.0
...
```

| Column | Description | Units |
|--------|-------------|-------|
| `year` | Calendar year | Integer |
| `quarter` | Quarter (1-4) | Integer |
| `GDP` | Real GDP | Billions USD |
| `LEB` | Employment | Millions |
| `LURC` | Unemployment rate | Percent |

## Report Generation

Generate a formatted "State of Tariffs" report:

```bash
Rscript run.R 11-17 --report --report-date 2025-11-17
```

### Requirements

- Pandoc installed and on PATH
- `ANTHROPIC_API_KEY` environment variable set
- Completed model run (generates data for report)

### Output

Reports are saved to `output/{scenario}/report/`:
- `tariff_report.md` - Markdown source
- `tariff_report.docx` - Word document (via Pandoc)

See `reports/report_instructions.md` for detailed report customization options.
