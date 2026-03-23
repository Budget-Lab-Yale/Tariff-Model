# =============================================================================
# Compute import share of PCE, decomposed into direct and indirect
#
# Replicates the Boston Fed methodology (Barbiero & Stein 2025, Figure 2):
#   - Direct: omega_M (fraction of commodity consumption directly imported)
#   - Indirect: omega_D * (L_D' %*% B_MD' %*% 1) (import content of domestic
#     production propagated through Leontief inverse)
#
# Setting tau = vector of 1s in the price formula gives import shares:
#   P = [omega_M * 1 + omega_D * (L_D' %*% B_MD' %*% 1)] * C
#     = [direct_import_share + indirect_import_share] * C
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))
source('src/io_price_model.R')

# ---- Configuration ----
markup_assumption <- 'constant_percentage'
io_data_dir <- 'resources/io/detail'

# ---- Load data ----
message('Loading BEA I-O data...')
use_import <- load_bea_use_data('import', io_data_dir)
use_domestic <- load_bea_use_data('domestic', io_data_dir)
industry_output <- load_bea_industry_output(io_data_dir)
leontief_domestic <- load_bea_requirements('domestic', io_data_dir)
pce_bridge <- load_pce_bridge(io_data_dir)
commodity_use_totals <- load_bea_commodity_use_totals(io_data_dir)
industry_variable_cost <- load_bea_industry_variable_cost(io_data_dir)

# ---- Build matrices ----
message('Building I-O matrices...')
io_mats <- build_io_matrices(
  use_import, use_domestic, industry_output,
  markup_assumption = markup_assumption,
  commodity_use_totals = commodity_use_totals,
  industry_variable_cost = industry_variable_cost
)

B_MD <- io_mats$B_MD
omega_M <- io_mats$omega_M
omega_D <- io_mats$omega_D
commodities <- rownames(B_MD)

# ---- Compute import shares per commodity ----
# Direct: omega_M (fraction of commodity that is imported)
direct <- omega_M

# Indirect: omega_D * (L_D' %*% B_MD' %*% 1)
# Step 1: B_MD' %*% 1 = column sums of B_MD = total import input per industry
tau_ones <- rep(1, length(commodities))
names(tau_ones) <- commodities
imported_input_cost <- as.numeric(t(B_MD) %*% tau_ones)
names(imported_input_cost) <- colnames(B_MD)

# Step 2: L_D' %*% imported_input_cost -> propagated import content per commodity
leontief_industries <- rownames(leontief_domestic)
bmd_industries <- colnames(B_MD)
common_industries <- intersect(bmd_industries, leontief_industries)

cost_aligned <- rep(0, length(leontief_industries))
names(cost_aligned) <- leontief_industries
cost_aligned[common_industries] <- imported_input_cost[common_industries]

propagated <- as.numeric(t(leontief_domestic) %*% cost_aligned)
names(propagated) <- colnames(leontief_domestic)

# Align to commodity dimension
propagated_aligned <- rep(0, length(commodities))
names(propagated_aligned) <- commodities
prop_matched <- intersect(names(propagated), commodities)
propagated_aligned[prop_matched] <- propagated[prop_matched]

indirect <- omega_D * propagated_aligned

# Total import share per commodity
total_import <- direct + indirect

# ---- Map to PCE categories via bridge ----
message('\nMapping to PCE categories...')

# Join commodity-level import shares to bridge
price_df <- tibble(
  bea_code = commodities,
  direct = as.numeric(direct),
  indirect = as.numeric(indirect),
  total = as.numeric(total_import)
)

bridge_joined <- pce_bridge %>%
  left_join(price_df, by = 'bea_code') %>%
  mutate(
    direct = if_else(is.na(direct), 0, direct),
    indirect = if_else(is.na(indirect), 0, indirect),
    total = if_else(is.na(total), 0, total)
  )

# Weight: use purchasers_value for constant-percentage, producers_value for constant-dollar
if (markup_assumption == 'constant_dollar') {
  weight_col <- 'producers_value'
} else {
  weight_col <- 'purchasers_value'
}

# Aggregate to NIPA category level
pce_import_shares <- bridge_joined %>%
  group_by(nipa_line, pce_category) %>%
  summarise(
    direct_share = sum(direct * .data[[weight_col]]) / sum(purchasers_value),
    indirect_share = sum(indirect * .data[[weight_col]]) / sum(purchasers_value),
    total_share = sum(total * .data[[weight_col]]) / sum(purchasers_value),
    purchasers_value = sum(purchasers_value),
    .groups = 'drop'
  ) %>%
  mutate(
    pce_weight = purchasers_value / sum(purchasers_value),
    # Contribution to total PCE import share
    direct_pce_contrib = direct_share * pce_weight,
    indirect_pce_contrib = indirect_share * pce_weight,
    total_pce_contrib = total_share * pce_weight
  ) %>%
  arrange(desc(total_pce_contrib))

total_pce <- sum(pce_import_shares$purchasers_value)

# ---- Aggregate results ----
agg_direct <- sum(pce_import_shares$direct_pce_contrib)
agg_indirect <- sum(pce_import_shares$indirect_pce_contrib)
agg_total <- agg_direct + agg_indirect

message('\n==========================================================')
message('IMPORT SHARE OF PCE (Boston Fed methodology)')
message(sprintf('Markup assumption: %s', markup_assumption))
message(sprintf('BEA I-O level: summary (2024)'))
message('==========================================================')
message(sprintf('Direct import share:   %.1f%% of PCE', agg_direct * 100))
message(sprintf('Indirect import share: %.1f%% of PCE', agg_indirect * 100))
message(sprintf('Total import share:    %.1f%% of PCE', agg_total * 100))
message(sprintf('Total PCE: $%.0fB', total_pce / 1000))

# ---- Core PCE (exclude food and energy) ----
# Food NIPA lines: 1-18 (Food and beverages purchased for off-premises consumption)
# and 19 (Purchased meals and beverages)
# Energy: 50 (Electricity), 51-52 would be gas/fuel but let's identify them
# Actually, BLS/BEA core PCE excludes food and energy. Let me identify by category name.
food_keywords <- c('food', 'meal', 'beverage', 'cereal', 'meat', 'poultry',
                   'fish', 'egg', 'dairy', 'fruit', 'vegetable', 'sugar',
                   'fat', 'coffee', 'tea', 'snack')
energy_keywords <- c('gasoline', 'fuel oil', 'electricity', 'natural gas',
                     'energy')

pce_import_shares <- pce_import_shares %>%
  mutate(
    is_food = grepl(paste(food_keywords, collapse = '|'), tolower(pce_category)),
    is_energy = grepl(paste(energy_keywords, collapse = '|'), tolower(pce_category))
  )

core <- pce_import_shares %>% filter(!is_food & !is_energy)
core_pce <- sum(core$purchasers_value)
core_direct <- sum(core$direct_share * core$purchasers_value) / core_pce
core_indirect <- sum(core$indirect_share * core$purchasers_value) / core_pce
core_total <- core_direct + core_indirect

message(sprintf('\nCore PCE (excl food & energy):'))
message(sprintf('Direct import share:   %.1f%% of core PCE', core_direct * 100))
message(sprintf('Indirect import share: %.1f%% of core PCE', core_indirect * 100))
message(sprintf('Total import share:    %.1f%% of core PCE', core_total * 100))
message(sprintf('Core PCE: $%.0fB (%.1f%% of total)',
                core_pce / 1000, core_pce / total_pce * 100))

# ---- Top categories by import content ----
message('\n----------------------------------------------------------')
message('TOP 20 NIPA CATEGORIES BY TOTAL IMPORT SHARE OF PCE')
message('----------------------------------------------------------')
message(sprintf('%-4s %-45s %7s %7s %7s %8s %6s',
                'Line', 'Category', 'Direct', 'Indir.', 'Total', 'PCE($B)', 'PCE%'))
message(paste(rep('-', 90), collapse = ''))

top20 <- pce_import_shares %>% head(20)
for (i in seq_len(nrow(top20))) {
  r <- top20[i, ]
  message(sprintf('%4d %-45s %6.1f%% %6.1f%% %6.1f%% %7.0f %5.1f%%',
                  r$nipa_line,
                  substr(r$pce_category, 1, 45),
                  r$direct_share * 100,
                  r$indirect_share * 100,
                  r$total_share * 100,
                  r$purchasers_value / 1000,
                  r$pce_weight * 100))
}

# ---- All categories sorted by total import content (for reference) ----
message('\n----------------------------------------------------------')
message('ALL NIPA CATEGORIES (sorted by total import share of PCE)')
message('----------------------------------------------------------')
message(sprintf('%-4s %-45s %7s %7s %7s %8s',
                'Line', 'Category', 'Direct', 'Indir.', 'Total', 'PCE%'))
message(paste(rep('-', 80), collapse = ''))

all_sorted <- pce_import_shares %>% arrange(desc(total_pce_contrib))
for (i in seq_len(nrow(all_sorted))) {
  r <- all_sorted[i, ]
  if (r$total_pce_contrib > 0.0005) {  # Only show categories contributing >0.05%
    message(sprintf('%4d %-45s %6.1f%% %6.1f%% %6.1f%% %5.1f%%',
                    r$nipa_line,
                    substr(r$pce_category, 1, 45),
                    r$direct_share * 100,
                    r$indirect_share * 100,
                    r$total_share * 100,
                    r$pce_weight * 100))
  }
}
