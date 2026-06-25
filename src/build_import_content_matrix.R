# =============================================================================
# build_import_content_matrix.R
# =============================================================================
# Produces a STATIC, tariff-independent import-content matrix mapping consumer
# categories to BEA commodities, for the AI-training / distributional-incidence
# task. Reuses the production I-O machinery in src/io_price_model.R.
#
# The object built here is the linear operator that the Boston Fed price model
# applies to a tariff vector, but with the behavioral pass-through scaling
# stripped out (scaling = 1), so each cell is a pure structural share:
#
#   A[k, c] = fraction of consumer category k's final-use (purchaser's-value)
#             dollar that is import content of BEA commodity c, counting both
#               - direct imported final goods           (omega_M)
#               - imported intermediates embodied via    (omega_D * L_D' B_MD')
#                 domestic production
#
#   row_sum_c A[k, c] = total import-content share of category k
#
# To recover the model's CONSUMER PRICE IMPACT from an ETR vector tau (fractional
# tariff by BEA commodity):
#
#   price_impact_k (pp) = SCALING * 100 * sum_c A[k, c] * tau_c
#   SCALING = 1 + domestic_pricing - usd_offset   (= 1.326 at current config)
#
# Commodity grain : BEA summary (~73 commodities, 2024 annual)
# Category grain  : 76 native NIPA PCE categories  -> aggregated to the 19
#                   CEX/CES categories used in resources/distribution/ces_categories.csv
#
# Outputs (output/import_content_matrix/):
#   import_content_matrix_pce.csv   76 NIPA PCE  x 73 BEA commodities (+ total)
#   import_content_matrix_ces.csv   19 CEX       x 73 BEA commodities (+ total)
#   pce_to_ces_crosswalk.csv        the 76->19 bridge (auditable)
#   bea_commodity_import_content.csv per-commodity direct/indirect/total content
#   README.md                       provenance + the SCALING note
# =============================================================================

library(tidyverse)

# Run from repo root regardless of caller cwd
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA)
repo_root <- 'C:/Users/jar335/Documents/Repositories/Tariff-Model'
setwd(repo_root)

source('src/io_price_model.R')

DATA_DIR <- 'resources/io'   # summary level (73 commodities, 2024)
MARKUP   <- 'constant_dollar' # B normalized by industry output -> technical import coefficients
OUT_DIR  <- 'output/import_content_matrix'
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Config-consistent behavioral scaling (reported, NOT baked into the matrix)
USD_OFFSET       <- 0.174
DOMESTIC_PRICING <- 0.5
SCALING          <- 1 + DOMESTIC_PRICING - USD_OFFSET

# ---- 1. Load BEA I-O data and build structural matrices ---------------------
message('Loading BEA summary I-O tables...')
use_import   <- load_bea_use_data('import',   DATA_DIR)
use_domestic <- load_bea_use_data('domestic', DATA_DIR)
ind_output   <- load_bea_industry_output(DATA_DIR)
use_totals   <- load_bea_commodity_use_totals(DATA_DIR)
ind_varcost  <- tryCatch(load_bea_industry_variable_cost(DATA_DIR),
                         error = function(e) setNames(rep(NA_real_, ncol(use_import)),
                                                      colnames(use_import)))
leontief_D   <- load_bea_requirements('domestic', DATA_DIR)
pce_bridge   <- load_pce_bridge(DATA_DIR)
descriptions <- read_csv(file.path(DATA_DIR, 'bea_commodity_descriptions.csv'),
                         show_col_types = FALSE) %>%
  rename(bea_code = `Commodity Code`, description = Description)

io <- build_io_matrices(use_import, use_domestic, ind_output,
                        markup_assumption = MARKUP,
                        commodity_use_totals = use_totals,
                        industry_variable_cost = ind_varcost)

B_MD    <- io$B_MD          # C x I
omega_M <- io$omega_M       # C
omega_D <- io$omega_D       # C

# ---- 2. Build the commodity-level import-content operator G (C x C) ---------
# G[c, c'] = price/content effect on commodity c per unit tariff on commodity c'
#          = omega_M[c]*1{c=c'}  +  omega_D[c] * (L_D' B_MD')[c, c']
commodities <- rownames(B_MD)
industries  <- colnames(B_MD)

stopifnot(all(industries %in% rownames(leontief_D)))
stopifnot(all(commodities %in% colnames(leontief_D)))

L_D <- leontief_D[industries, commodities, drop = FALSE]   # I x C
P   <- t(L_D) %*% t(B_MD[commodities, industries, drop = FALSE])  # C x C
P   <- P[commodities, commodities, drop = FALSE]

G <- diag(omega_M[commodities]) + diag(omega_D[commodities]) %*% P
rownames(G) <- commodities
colnames(G) <- commodities

# Per-commodity import content (row of G summed over a uniform unit tariff is
# not what we want here; the structural content of commodity c is its own column
# response under unit tariff = colSums? No: content embodied in good c =
# diagonal direct + indirect that lands on c. Report the standard decomposition.)
direct_content   <- omega_M[commodities]
indirect_content <- omega_D[commodities] * diag(P)   # self-propagation share
# Total (direct+indirect) import content of a dollar of commodity c's price base
total_content    <- rowSums(G)   # response of price c to a uniform unit tariff
commodity_ic <- tibble(
  bea_code = commodities,
  direct_import_share   = as.numeric(direct_content),
  indirect_import_share = as.numeric(total_content - direct_content),
  total_import_content  = as.numeric(total_content)
) %>%
  left_join(descriptions, by = 'bea_code')

# ---- 3. Category weights W[k, c] from the PCE bridge ------------------------
# purchaser's-value share of commodity c within category k
build_weights <- function(bridge_with_cat, cat_col) {
  bridge_with_cat %>%
    filter(bea_code %in% commodities) %>%
    group_by(.cat = .data[[cat_col]], bea_code) %>%
    summarise(pv = sum(purchasers_value), .groups = 'drop') %>%
    group_by(.cat) %>%
    mutate(w = pv / sum(pv)) %>%
    ungroup()
}

apply_operator <- function(weights_long, cats) {
  W <- matrix(0, nrow = length(cats), ncol = length(commodities),
              dimnames = list(cats, commodities))
  for (i in seq_len(nrow(weights_long))) {
    W[weights_long$.cat[i], weights_long$bea_code[i]] <- weights_long$w[i]
  }
  A <- W %*% G
  A
}

# ---- 3a. Native 76 NIPA PCE matrix (no judgment) ----------------------------
pce_cats <- pce_bridge %>% distinct(nipa_line, pce_category) %>% arrange(nipa_line)
pce_bridge2 <- pce_bridge %>% mutate(.catkey = pce_category)
W_pce_long <- build_weights(pce_bridge2, 'pce_category')
A_pce <- apply_operator(W_pce_long, sort(unique(W_pce_long$.cat)))

# ---- 4. The 76 -> 19 CEX/CES crosswalk --------------------------------------
# Reasonable BLS-CES-aligned assignment of each NIPA PCE line to a CEX category.
# Ambiguous durables/services flagged; emitted to pce_to_ces_crosswalk.csv for
# audit. Food-at-home (lines 27, 29) -> single "Food at home" bucket, fanned out
# to the 5 CEX food subcategories afterward (identical import content at BEA
# summary grain).
FOOD_AT_HOME <- 'Food at home'
ces_map <- tribble(
  ~nipa_line, ~ces_category,
  5,  'Transportation',          6,  'Transportation',          7,  'Transportation',
  9,  'Household furnishings and equipment', 10, 'Household furnishings and equipment',
  11, 'Household furnishings and equipment', 12, 'Household furnishings and equipment',
  14, 'Entertainment', 15, 'Entertainment', 16, 'Entertainment', 17, 'Entertainment',
  18, 'Entertainment',
  20, 'Apparel and services',    21, 'Healthcare',              22, 'Education',
  23, 'Apparel and services',    24, 'Utilities, fuels, and public services',
  27, FOOD_AT_HOME,              28, 'Alcoholic beverages + Tobacco products and smoking supplies',
  29, FOOD_AT_HOME,
  32, 'Apparel and services',    33, 'Apparel and services',    34, 'Apparel and services',
  35, 'Apparel and services',
  37, 'Transportation',          38, 'Utilities, fuels, and public services',
  40, 'Healthcare',              41, 'Entertainment',
  42, 'Housekeeping supplies',
  43, 'Household operations + Personal care products and services',
  44, 'Alcoholic beverages + Tobacco products and smoking supplies',
  45, 'Entertainment',           46, 'Miscellaneous',
  51, 'Shelter', 52, 'Shelter', 53, 'Shelter', 54, 'Shelter',
  56, 'Utilities, fuels, and public services', 58, 'Utilities, fuels, and public services',
  59, 'Utilities, fuels, and public services',
  62, 'Healthcare', 63, 'Healthcare', 64, 'Healthcare', 66, 'Healthcare', 67, 'Healthcare',
  70, 'Transportation', 71, 'Transportation', 73, 'Transportation', 74, 'Transportation',
  75, 'Transportation',
  77, 'Entertainment', 78, 'Entertainment', 79, 'Entertainment', 80, 'Entertainment',
  83, 'Food Away from Home',     84, 'Food Away from Home',      85, 'Shelter',
  88, 'Miscellaneous',           89, 'Miscellaneous',            91, 'Personal insurance and pensions',
  92, 'Miscellaneous',           93, 'Healthcare',               94, 'Transportation',
  97, 'Utilities, fuels, and public services', 98, 'Utilities, fuels, and public services',
  99, 'Utilities, fuels, and public services',
  101,'Education', 102,'Education', 103,'Education',
  104,'Miscellaneous',
  105,'Household operations + Personal care products and services',
  106,'Miscellaneous',
  107,'Household operations + Personal care products and services',
  109,'Transportation', 110,'Miscellaneous', 111,'Miscellaneous'
)

# sanity: every bridge nipa_line is assigned
unmapped <- setdiff(unique(pce_bridge$nipa_line), ces_map$nipa_line)
if (length(unmapped) > 0) {
  stop('Unmapped NIPA PCE lines in CES crosswalk: ', paste(unmapped, collapse = ', '))
}

crosswalk_out <- pce_cats %>%
  left_join(ces_map, by = 'nipa_line') %>%
  arrange(ces_category, nipa_line)

# ---- 4a. Build the CES matrix (15 buckets incl. single Food at home) --------
bridge_ces <- pce_bridge %>% left_join(ces_map, by = 'nipa_line')
W_ces_long <- build_weights(bridge_ces, 'ces_category')
ces_cats   <- sort(unique(W_ces_long$.cat))
A_ces_raw  <- apply_operator(W_ces_long, ces_cats)

# ---- 4b. Fan the single Food-at-home row across the 5 CEX food subcategories -
ces_target <- read_csv('resources/distribution/ces_categories.csv',
                       show_col_types = FALSE)$category
food_subcats <- c('Cereals and bakery products',
                  'Meats, poultry, fish, and eggs',
                  'Dairy Products',
                  'Fruits and Vegetables',
                  'Other food from home')

food_row <- A_ces_raw[FOOD_AT_HOME, , drop = FALSE]
A_ces <- A_ces_raw[setdiff(rownames(A_ces_raw), FOOD_AT_HOME), , drop = FALSE]
for (fc in food_subcats) {
  r <- food_row; rownames(r) <- fc
  A_ces <- rbind(A_ces, r)
}
# order rows to match ces_categories.csv where possible
ord <- c(food_subcats, setdiff(rownames(A_ces), food_subcats))
A_ces <- A_ces[ord, , drop = FALSE]

# ---- 5. Write outputs -------------------------------------------------------
mat_to_df <- function(M, rowname_col) {
  as_tibble(M, rownames = rowname_col) %>%
    mutate(total_import_content = rowSums(across(all_of(commodities))), .after = 1)
}

write_csv(mat_to_df(A_pce, 'pce_category'),
          file.path(OUT_DIR, 'import_content_matrix_pce.csv'))
write_csv(mat_to_df(A_ces, 'cex_category'),
          file.path(OUT_DIR, 'import_content_matrix_ces.csv'))
write_csv(crosswalk_out, file.path(OUT_DIR, 'pce_to_ces_crosswalk.csv'))
write_csv(commodity_ic,  file.path(OUT_DIR, 'bea_commodity_import_content.csv'))

readme <- sprintf('# Import-content matrix (commodity -> consumer category)

Built by src/build_import_content_matrix.R from BEA summary I-O tables (73
commodities, 2024) via the production Boston Fed price model in
src/io_price_model.R.

## What each cell is
A[k, c] = fraction of consumer category k\'s final-use (purchaser\'s-value)
dollar that is import content of BEA commodity c (direct imported final goods +
imported intermediates embodied through domestic production). Row sums =
total import-content share of the category.

## Recovering a price impact from an ETR vector
This matrix is STRUCTURAL and tariff-independent (behavioral pass-through
removed). Given tau = fractional ETR by BEA commodity:

    price_impact_k (pp) = SCALING * 100 * sum_c A[k, c] * tau_c

SCALING = 1 + domestic_pricing - usd_offset = 1 + %.3f - %.3f = %.3f
(current config/global_assumptions.yaml).

## Food-at-home caveat
At BEA summary grain all food collapses to ~commodity 311FT, and NIPA PCE has a
single food-at-home line. The 5 CEX food-at-home subcategories therefore share
ONE import-content row (identical). Differentiate them distributionally via the
CEX consumption shares in resources/distribution/ces_categories.csv, not via the
price impact. (This matches the task brief, which needs one broken-out food
price impact for the SNAP food-indexed offset.)

## Files
- import_content_matrix_ces.csv   19 CEX x 73 BEA commodities (+ total)
- import_content_matrix_pce.csv   76 NIPA PCE x 73 (no aggregation judgment)
- pce_to_ces_crosswalk.csv        the 76->19 bridge (audit / edit here)
- bea_commodity_import_content.csv per-commodity direct/indirect/total content
',
DOMESTIC_PRICING, USD_OFFSET, SCALING)
writeLines(readme, file.path(OUT_DIR, 'README.md'))

# ---- 6. Console summary -----------------------------------------------------
message('\n==== Import-content matrix built ====')
message(sprintf('  BEA commodities: %d   PCE categories: %d   CEX categories: %d',
                length(commodities), nrow(A_pce), nrow(A_ces)))
message(sprintf('  Behavioral SCALING (reported separately): %.3f', SCALING))
message('\n  Total import-content share by CEX category (row sums):')
ces_tot <- tibble(cex_category = rownames(A_ces),
                  total_import_content = rowSums(A_ces)) %>%
  arrange(desc(total_import_content))
print(ces_tot, n = nrow(ces_tot))
message(sprintf('\n  Outputs written to %s/', OUT_DIR))
