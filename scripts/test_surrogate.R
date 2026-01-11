# Test MAUS surrogate predictions
library(readr)

message('=== Testing MAUS Surrogate ===')

# Load surrogate
surrogate <- readRDS('resources/maus_surrogate/interpolators.rds')
message(sprintf('Loaded surrogate with %d training runs', surrogate$n_training_runs))
message(sprintf('ETR range: %.2f%% to %.2f%%', surrogate$etr_range[1] * 100, surrogate$etr_range[2] * 100))

# Load baseline
baseline <- read_csv('resources/baselines/maus_baseline.csv', show_col_types = FALSE)

# Test prediction at ETR = 10% (between training points)
test_etr <- 0.10
message(sprintf('\nPredicting for ETR = %.2f%%...', test_etr * 100))

quarters <- surrogate$quarters
n_quarters <- nrow(quarters)

gdp_pred <- numeric(n_quarters)
emp_pred <- numeric(n_quarters)
ur_pred <- numeric(n_quarters)

for (i in seq_len(n_quarters)) {
  key <- paste0(quarters$year[i], '_Q', quarters$quarter[i])
  interp <- surrogate$interpolators[[key]]
  gdp_pred[i] <- interp$gdp(test_etr)
  emp_pred[i] <- interp$emp(test_etr)
  ur_pred[i] <- interp$ur(test_etr)
}

# Show Q4 values for 2025 and 2026
message('\nPredicted Q4 values:')
message(sprintf('  2025 Q4: GDP = %.1f, Emp = %.3f, UR = %.3f',
                gdp_pred[which(quarters$year == 2025 & quarters$quarter == 4)],
                emp_pred[which(quarters$year == 2025 & quarters$quarter == 4)],
                ur_pred[which(quarters$year == 2025 & quarters$quarter == 4)]))
message(sprintf('  2026 Q4: GDP = %.1f, Emp = %.3f, UR = %.3f',
                gdp_pred[which(quarters$year == 2026 & quarters$quarter == 4)],
                emp_pred[which(quarters$year == 2026 & quarters$quarter == 4)],
                ur_pred[which(quarters$year == 2026 & quarters$quarter == 4)]))

# Compare to baseline
base_2025_q4 <- baseline[baseline$year == 2025 & baseline$quarter == 4, ]
base_2026_q4 <- baseline[baseline$year == 2026 & baseline$quarter == 4, ]

message('\nBaseline Q4 values:')
message(sprintf('  2025 Q4: GDP = %.1f, Emp = %.3f, UR = %.3f',
                base_2025_q4$GDP, base_2025_q4$LEB, base_2025_q4$LURC))
message(sprintf('  2026 Q4: GDP = %.1f, Emp = %.3f, UR = %.3f',
                base_2026_q4$GDP, base_2026_q4$LEB, base_2026_q4$LURC))

message('\nDifferences from baseline:')
idx_2025 <- which(quarters$year == 2025 & quarters$quarter == 4)
idx_2026 <- which(quarters$year == 2026 & quarters$quarter == 4)
message(sprintf('  2025 Q4: GDP = %.1f, Emp = %.3f, UR = %+.3f',
                gdp_pred[idx_2025] - base_2025_q4$GDP,
                emp_pred[idx_2025] - base_2025_q4$LEB,
                ur_pred[idx_2025] - base_2025_q4$LURC))
message(sprintf('  2026 Q4: GDP = %.1f, Emp = %.3f, UR = %+.3f',
                gdp_pred[idx_2026] - base_2026_q4$GDP,
                emp_pred[idx_2026] - base_2026_q4$LEB,
                ur_pred[idx_2026] - base_2026_q4$LURC))

message('\nSurrogate test complete!')
