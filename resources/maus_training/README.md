# MAUS Training Data

Place MAUS training runs here. Each run should be in its own subdirectory with:

```
run_XX/
  etr.txt         # Single line: the post-substitution ETR increase as decimal (e.g., 0.057)
  quarterly.csv   # MAUS output: year, quarter, GDP, LEB, LURC
```

## Example

```
run_01/
  etr.txt         # contains: 0.057
  quarterly.csv   # MAUS output for 5.7% ETR scenario

run_02/
  etr.txt         # contains: 0.12
  quarterly.csv   # MAUS output for 12% ETR scenario
```

## quarterly.csv format

```csv
year,quarter,GDP,LEB,LURC
2024,3,20707.94,133.00,4.17
2024,4,20805.11,133.35,4.19
...
```

After adding training runs, run:
```r
source('src/estimation/estimate_maus_surrogate.R')
estimate_maus_surrogate()
```
