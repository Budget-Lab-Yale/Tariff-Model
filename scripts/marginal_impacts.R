# Calculate marginal GDP impacts at each ETR level
library(readr)

# Load surrogate
surrogate <- readRDS('resources/maus_surrogate/interpolators.rds')
baseline <- read_csv('resources/baselines/maus_baseline.csv', show_col_types = FALSE)

etrs <- surrogate$training_etrs

# Key quarters to analyze
key_quarters <- list(
  list(year = 2025, quarter = 4, label = '2025 Q4'),
  list(year = 2026, quarter = 4, label = '2026 Q4')
)

for (kq in key_quarters) {
  key <- paste0(kq$year, '_Q', kq$quarter)
  interp <- surrogate$interpolators[[key]]

  base_gdp <- baseline$GDP[baseline$year == kq$year & baseline$quarter == kq$quarter]

  message(sprintf('\n=== %s (Baseline GDP = %.1f) ===\n', kq$label, base_gdp))
  message(sprintf('%-10s %12s %12s %18s',
                  'ETR', 'GDP Δ', 'Marginal Δ', 'Marginal per 1pp'))
  message(paste(rep('-', 55), collapse = ''))

  prev_etr <- 0
  prev_gdp_delta <- 0

  for (i in seq_along(etrs)) {
    etr <- etrs[i]
    gdp <- interp$gdp(etr)
    gdp_delta <- gdp - base_gdp

    # Marginal change from previous ETR level
    if (i == 1) {
      marginal_delta <- gdp_delta  # from baseline (ETR=0)
      marginal_per_pp <- gdp_delta / (etr * 100)
    } else {
      marginal_delta <- gdp_delta - prev_gdp_delta
      etr_change <- (etr - prev_etr) * 100  # in pp
      marginal_per_pp <- marginal_delta / etr_change
    }

    message(sprintf('%-10s %12.1f %12.1f %18.2f',
                    sprintf('%.2f%%', etr * 100),
                    gdp_delta,
                    marginal_delta,
                    marginal_per_pp))

    prev_etr <- etr
    prev_gdp_delta <- gdp_delta
  }
}

# Also show continuous marginal effect curve
message('\n\n=== Continuous Marginal Effect (dGDP/dETR) ===')
message('GDP change per 1pp ETR increase, evaluated at each ETR level\n')

for (kq in key_quarters) {
  key <- paste0(kq$year, '_Q', kq$quarter)
  interp <- surrogate$interpolators[[key]]

  message(sprintf('%s:', kq$label))
  message(sprintf('%-10s %15s', 'ETR', 'dGDP/d(1pp ETR)'))
  message(paste(rep('-', 28), collapse = ''))

  # Evaluate derivative at fine grid
  test_etrs <- seq(0.03, 0.23, by = 0.02)

  for (etr in test_etrs) {
    # Numerical derivative (central difference)
    eps <- 0.001
    slope <- (interp$gdp(etr + eps) - interp$gdp(etr - eps)) / (2 * eps * 100)
    message(sprintf('%-10s %15.2f', sprintf('%.0f%%', etr * 100), slope))
  }
  message('')
}
