# =============================================================================
# validate_dynamic_revenue.R - Validate dynamic revenue calculations
# =============================================================================

library(tidyverse)

# -------------------------------------------------------------------------
# Load MAUS quarterly data
# -------------------------------------------------------------------------

maus <- read_csv('config/scenarios/example/maus_outputs/quarterly.csv', show_col_types = FALSE)

cat('MAUS data range:', min(maus$year), 'Q', min(maus$quarter[maus$year == min(maus$year)]),
    'to', max(maus$year), 'Q', max(maus$quarter[maus$year == max(maus$year)]), '\n\n')

# -------------------------------------------------------------------------
# Load CBO sensitivity data
# -------------------------------------------------------------------------

cbo_sens <- read_csv('resources/cbo_rules/revenue_sensitivity.csv', show_col_types = FALSE)

cat('CBO sensitivity data:\n')
print(cbo_sens)
cat('\n')

# -------------------------------------------------------------------------
# Calculate calendar year averages
# -------------------------------------------------------------------------

annual_gdp <- maus %>%
  group_by(year) %>%
  summarise(
    real_gdp_baseline = mean(gdp_baseline),
    real_gdp_tariff = mean(gdp_tariff),
    .groups = 'drop'
  )

cat('Calendar year real GDP averages:\n')
print(annual_gdp)
cat('\n')

# -------------------------------------------------------------------------
# Convert to fiscal year (FY = 0.75 * CY + 0.25 * prior CY)
# -------------------------------------------------------------------------

fy_gdp <- annual_gdp %>%
  arrange(year) %>%
  mutate(
    prior_real_baseline = lag(real_gdp_baseline),
    prior_real_tariff = lag(real_gdp_tariff),
    fiscal_year = year,
    fy_real_baseline = 0.75 * real_gdp_baseline + 0.25 * coalesce(prior_real_baseline, real_gdp_baseline),
    fy_real_tariff = 0.75 * real_gdp_tariff + 0.25 * coalesce(prior_real_tariff, real_gdp_tariff),
    fy_real_change = fy_real_tariff - fy_real_baseline
  ) %>%
  select(fiscal_year, fy_real_baseline, fy_real_tariff, fy_real_change)

cat('Fiscal year real GDP:\n')
print(fy_gdp)
cat('\n')

# -------------------------------------------------------------------------
# Join with CBO data and convert to nominal
# -------------------------------------------------------------------------

fy_gdp <- fy_gdp %>%
  left_join(cbo_sens, by = 'fiscal_year') %>%
  filter(!is.na(revenue_per_gdp)) %>%
  mutate(
    # Calculate deflator (nominal/real ratio from CBO baseline)
    deflator = cbo_gdp_base / fy_real_baseline,

    # Convert real GDP to nominal
    fy_nominal_baseline = fy_real_baseline * deflator,
    fy_nominal_tariff = fy_real_tariff * deflator,

    # Calculate nominal GDP change
    nominal_gdp_change = fy_nominal_tariff - fy_nominal_baseline,

    # Apply CBO sensitivity
    dynamic_effect = revenue_per_gdp * nominal_gdp_change
  )

cat('Fiscal year with deflator and dynamic effect:\n')
fy_gdp %>%
  select(fiscal_year, fy_real_change, deflator, nominal_gdp_change, revenue_per_gdp, dynamic_effect) %>%
  print()
cat('\n')

# -------------------------------------------------------------------------
# Compare to expected values
# -------------------------------------------------------------------------

cat('==========================================================\n')
cat('VALIDATION RESULTS\n')
cat('==========================================================\n\n')

# Excel shows: FY2026 dynamic effect = -41
# Our calculation:
fy2026 <- fy_gdp %>% filter(fiscal_year == 2026)
cat(sprintf('FY2026 Validation:\n'))
cat(sprintf('  Real GDP baseline:     $%.2fB\n', fy2026$fy_real_baseline))
cat(sprintf('  Real GDP tariff:       $%.2fB\n', fy2026$fy_real_tariff))
cat(sprintf('  Real GDP change:       $%.2fB\n', fy2026$fy_real_change))
cat(sprintf('  Deflator:              %.4f\n', fy2026$deflator))
cat(sprintf('  Nominal GDP change:    $%.2fB\n', fy2026$nominal_gdp_change))
cat(sprintf('  CBO sensitivity:       %.6f\n', fy2026$revenue_per_gdp))
cat(sprintf('  Calculated effect:     $%.2fB\n', fy2026$dynamic_effect))
cat(sprintf('  Expected (Excel):      $-41B\n'))
cat(sprintf('  Difference:            $%.2fB (%.1f%%)\n',
            fy2026$dynamic_effect - (-41),
            (fy2026$dynamic_effect - (-41)) / (-41) * 100))
cat('\n')

# Sum for available years
available_years <- fy_gdp %>%
  filter(fiscal_year >= 2026, fiscal_year <= 2029)

cat('Dynamic effects for available years (FY2026-2029):\n')
available_years %>%
  select(fiscal_year, dynamic_effect) %>%
  print()

cat(sprintf('\nSum of FY2026-2029 dynamic effects: $%.2fB\n', sum(available_years$dynamic_effect)))

# Note: Excel shows 10-year dynamic effect = -$386.6B
# But we only have MAUS data through 2029
cat('\n')
cat('Note: Excel model has dynamic effects for FY2026-2035 (10 years)\n')
cat('Our MAUS data only covers through 2029 (4 years)\n')
cat('To match Excel, we need either:\n')
cat('  1. Extended MAUS projections through 2035, OR\n')
cat('  2. Pre-calculated dynamic effects for outer years\n')
