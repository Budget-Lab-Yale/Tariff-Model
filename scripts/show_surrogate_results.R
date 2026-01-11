# Show MAUS surrogate interpolation results
library(readr)

message('=== MAUS Surrogate Interpolation Results ===\n')

# Load surrogate
surrogate <- readRDS('resources/maus_surrogate/interpolators.rds')

# Load baseline for computing deltas
baseline <- read_csv('resources/baselines/maus_baseline.csv', show_col_types = FALSE)

# Training ETRs
etrs <- surrogate$training_etrs
message(sprintf('Training ETRs: %s\n', paste(sprintf('%.2f%%', etrs * 100), collapse = ', ')))

# Show training data for key quarters (Q4 2025, Q4 2026)
key_quarters <- list(
  c(2025, 4),
  c(2026, 4),
  c(2027, 4),
  c(2028, 4)
)

for (yq in key_quarters) {
  yr <- yq[1]
  qtr <- yq[2]
  key <- paste0(yr, '_Q', qtr)

  # Get baseline values
  base_row <- baseline[baseline$year == yr & baseline$quarter == qtr, ]
  base_gdp <- base_row$GDP
  base_emp <- base_row$LEB
  base_ur <- base_row$LURC

  message(sprintf('=== %d Q%d ===', yr, qtr))
  message(sprintf('Baseline: GDP=%.1f, Emp=%.3f, UR=%.3f\n', base_gdp, base_emp, base_ur))

  # Get interpolator
  interp <- surrogate$interpolators[[key]]

  # Show training points and predictions
  message(sprintf('%-10s %12s %12s %12s %12s %12s %12s',
                  'ETR', 'GDP', 'GDP Δ', 'Emp', 'Emp Δ', 'UR', 'UR Δ'))
  message(paste(rep('-', 82), collapse = ''))

  for (etr in etrs) {
    gdp <- interp$gdp(etr)
    emp <- interp$emp(etr)
    ur <- interp$ur(etr)

    message(sprintf('%-10s %12.1f %12.1f %12.3f %12.3f %12.3f %+12.3f',
                    sprintf('%.2f%%', etr * 100),
                    gdp, gdp - base_gdp,
                    emp, emp - base_emp,
                    ur, ur - base_ur))
  }
  message('')
}

# Show implied "slopes" (approximate marginal effects)
message('=== Implied Marginal Effects (per 1pp ETR increase) ===\n')
message(sprintf('%-10s %12s %12s %12s', 'Quarter', 'GDP/pp', 'Emp/pp', 'UR/pp'))
message(paste(rep('-', 50), collapse = ''))

for (yq in key_quarters) {
  yr <- yq[1]
  qtr <- yq[2]
  key <- paste0(yr, '_Q', qtr)
  interp <- surrogate$interpolators[[key]]

  # Compute slope between min and max ETR
  etr_low <- min(etrs)
  etr_high <- max(etrs)
  etr_diff <- (etr_high - etr_low) * 100  # in pp

  gdp_slope <- (interp$gdp(etr_high) - interp$gdp(etr_low)) / etr_diff
  emp_slope <- (interp$emp(etr_high) - interp$emp(etr_low)) / etr_diff
  ur_slope <- (interp$ur(etr_high) - interp$ur(etr_low)) / etr_diff

  message(sprintf('%-10s %12.2f %12.4f %+12.4f',
                  sprintf('%d Q%d', yr, qtr),
                  gdp_slope, emp_slope, ur_slope))
}

message('\nNote: These are average slopes across the ETR range.')
message('Actual interpolation may vary locally if relationship is nonlinear.')
