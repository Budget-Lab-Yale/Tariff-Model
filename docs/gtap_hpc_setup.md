# Running the Tariff Model on the Yale HPC (GTAP via Wine)

This doc explains how the model — including the GTAP/GEMPACK step — runs on the
Yale HPC (Linux), and how to reproduce the setup from scratch.

## TL;DR

- GTAP's GEMPACK binaries are **Windows-only and licensed**. We don't port them;
  we run the existing Windows `.exe`s under **Wine**, inside an **Apptainer**
  container, on the HPC. The existing GEMPACK licence (`GEI-0666`, executable-image,
  RunGTAP course/site) is reused as-is — no purchase, no source-code edition.
- Everything GTAP-related lives **outside the repo** in
  `/nfs/roberts/project/pi_nrs36/jar335/gempack_linux/`.
- The model calls thin wrapper scripts (`bin/gtapv7`, `bin/sltoht`) exactly as if
  they were native binaries; `config/gtap_config.yaml` points at them.
- Run via **Slurm** (compute node), never the login node.

## Layout of `gempack_linux/`

```
gempack_linux/
  wine.def              # Apptainer definition (Ubuntu 22.04 + Wine)
  wine.sif              # built container image
  win/                  # the Windows GEMPACK install, copied off a laptop
    GP/                 #   GEMPACK utilities (sltoht.exe, gemsim.exe, ...) + licen.gem
    GTAP/               #   gtapv7.exe, the .tab model source, and tbltarv7/ (the GTAP DB)
  wineprefix/           # persistent Wine prefix (drive_c/GP -> ../win/GP symlink for the licence)
  bin/gtapv7, bin/sltoht# Wine wrapper scripts the model calls
  work/                 # gtap_work_dir scratch
  run_model.sbatch      # Slurm submit script
```

## One-time setup (reproduce from scratch)

### 1. Copy the Windows GEMPACK install to the HPC

From the machine that has a working RunGTAP/GEMPACK install, copy the GEMPACK
utilities dir and the GTAP model+data dir into `gempack_linux/win/`:

```
# from the Windows machine (paths are examples — use your install's locations)
scp -r "C:\GP"                          you@hpc:/.../gempack_linux/win/
scp -r "C:\Users\<you>\Documents\GTAP"  you@hpc:/.../gempack_linux/win/
```

You need: `win/GP/sltoht.exe`, `win/GP/licen.gem`, `win/GTAP/gtapv7.exe`, and
`win/GTAP/tbltarv7/{sets.har,basedata.har,default.prm}` (plus the rest of those
dirs). The binaries are 64-bit PE32+.

### 2. Build the Wine container

```
cd gempack_linux
apptainer build --fakeroot wine.sif wine.def
```

`wine.def` installs Wine (+ `wine64`, 32-bit libs, `libgcc`) on Ubuntu 22.04.

### 3. Initialize the Wine prefix + licence symlink

```
export APPTAINERENV_WINEARCH=win64 APPTAINERENV_WINEPREFIX=$PWD/wineprefix
apptainer exec --bind /nfs/roberts/project/pi_nrs36 wine.sif wine64 wineboot --init
# GEMPACK looks for the licence at literal C:\GP\licen.gem; make that resolve:
ln -sfn "$PWD/win/GP" wineprefix/drive_c/GP
```

Verify: running `wine64 .../gtapv7.exe` should print the licence banner with
`Licence No. GEI-0666` (NOT "model size limited").

### 4. Point the model at the wrappers

Copy `config/gtap_config.yaml.example` to `config/gtap_config.yaml` (gitignored)
and set the Linux paths:

```yaml
gtap_executable:   '/.../gempack_linux/bin/gtapv7'
gtap_work_dir:     '/.../gempack_linux/work'
gtap_data_dir:     '/.../gempack_linux/win/GTAP/tbltarv7'
gtap_aux_dir:      '/.../gempack_linux/win/GTAP'
sltoht_executable: '/.../gempack_linux/bin/sltoht'
```

The `bin/gtapv7` / `bin/sltoht` wrappers run `apptainer exec ... wine64 <exe> "$@"`
with the env tuning below baked in.

### 5. R environment

```
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export R_LIBS_USER=/nfs/roberts/project/pi_nrs36/jar335/R_libs/4.4   # persistent user lib
Rscript -e 'install.packages("HARr", lib=Sys.getenv("R_LIBS_USER"), repos="https://cloud.r-project.org")'
```

The CRAN bundle provides tidyverse/yaml/jsonlite/openxlsx/scales/stringi/arrow;
only `HARr` (reads GEMPACK `.sol`/`.slc`/`.sl4`) needs installing.

## Running the model

Paths and dependencies follow the Budget Lab interface convention in
`config/interfaces/` (see [Interface outputs](#interface-outputs) below), so
scenario YAMLs no longer hardcode the tracker path — they run unchanged on laptop
and HPC. Edit `config/interfaces/output_roots.yaml` for your machine instead:

- `production` (`/nfs/.../shared`) — the model reads the tracker bundle from
  `<production>/model_data/Tariff-Rate-Tracker` and publishes its own outputs under
  `<production>/model_data/Tariff-Model/v1/`.
- `interface_versions.yaml` sets the default tracker vintage + scenario; a scenario
  may pin its own via `rate_panel.vintage` / `rate_panel.tracker_scenario`.
- `TARIFF_RATE_TRACKER_ROOT` is an optional env-var override for the tracker root
  (escape hatch; normally unnecessary).

Submit via Slurm (the login node will OOM-kill the job at a per-user cgroup cap —
Step 0a reads ~52 snapshots × ~4.8M rows, peaking ~29 GB):

```
sbatch gempack_linux/run_model.sbatch tracker_actual_2026-06-13
sbatch gempack_linux/run_model.sbatch my_scenario --write-local   # publish to scratch, not production
# or interactively on a compute node:
#   salloc -p day -A pi_nrs36 --mem=128G -c 8 -t 2:00:00
```

Working artifacts land in `output/<scenario>/` (gitignored): `results/*.csv`, the
GTAP solution under `gtap/`, the rate inputs under `rate_inputs/`.

## Interface outputs

Each run also **publishes** to the shared, versioned model_data tree (the Budget
Lab interface convention), so other analyses can consume tariff results the same
way they consume any sibling model:

```
<production>/model_data/Tariff-Model/v1/<vintage>/
  <scenario>/
    dependencies.csv   # ID, interface, version, vintage, scenario  (lineage stamp)
    manifest.json      # model version, vintage, git commit, run options, deps
    *.csv              # the run's result files
```

`vintage` is a `YYYYMMDDHHMM` run timestamp (override with `--vintage`). The lineage
stamp lives **inside the scenario dir** (not at the vintage root) so it travels with
the output and concurrent one-scenario-per-job runs can't clobber a shared file.
`dependencies.csv` records exactly which tariff-rate-tracker vintage + scenario
produced the run — e.g. `Tariff-Rate-Tracker, 2, 2026-06-25-14, actual` — so any
published tariff output is traceable to its tracker input. Pass `--write-local` to
publish under the `local` scratch root instead of `production`.

## Why the Wine settings are what they are

These are baked into the wrapper scripts; each was load-bearing for a clean run:

- **`wine64` + `WINEARCH=win64`** — the binaries are 64-bit; the generic `wine`
  launcher tries 32-bit mode and fails loading `ntdll.dll`.
- **`drive_c/GP -> win/GP` symlink** — GEMPACK reads the licence from the literal
  path `C:\GP\licen.gem`. Without it, it silently drops to demo "size limited" mode,
  which cannot solve a full GTAP model.
- **`LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libgcc_s.so.1` + `OMP_NUM_THREADS=1`** —
  threaded OpenBLAS under Wine otherwise spams `libgcc_s.so.1 ... pthread_exit` /
  `Bad file descriptor` and can corrupt the exit code.
- Wine maps `/` to drive `Z:`, so the **absolute forward-slash paths the model
  generates resolve correctly** — no path translation needed in the model.

## Known gaps / gotchas

- **Solution-path bug (fixed):** `src/00b_run_gtap.R` built the GTAP `Solution file`
  with `normalizePath(..., mustWork=FALSE)`, which absolutizes a non-existent path
  on Windows but leaves it **relative on Linux** — GEMPACK then couldn't create the
  `.sl4` from `gtap_work_dir`. Now absolutized via the (created) `output_dir`.
- **Tracker weights per vintage:** the model requires same-vintage import weights at
  `<root>/<vintage>/weights/import_weights_hs10_country.{parquet,csv.gz}` (no
  fallback). Some shared-bundle vintages were published **without** weights (e.g.
  `2026-06-23-10`); use a vintage that has them (e.g. `2026-06-25-14`) or have the
  tracker publish weights for that vintage.
- **Calibration outputs:** scenarios that set `noncompliance.eta_file` /
  `substitution.alpha_file` need those CSVs under `output/calibration/<scenario>/`.
  They come from the standalone `calibrate.R` harness (heavy: IMDB ZIPs + many GTAP
  solves) or are copied from a machine that has run it. The `_no_eta_alpha` bridge
  variants only need a neutral zeros eta file.
- **`GEMPACK_OS` banner:** the run log shows `Operating System is Unknwon` — that's
  GEMPACK failing to name the OS under Wine (its own typo); harmless.

## First validated run

`tracker_actual_2026-06-25_no_eta_alpha` (vintage `2026-06-25-14`, neutral eta):
all 11 steps, exit 0, GTAP solved under Wine in-pipeline, ~4 min, ~29 GB peak,
33 output files.
