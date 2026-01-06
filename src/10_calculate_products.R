# =============================================================================
# 09_calculate_products.R - Calculate product-level price effects
# =============================================================================
#
# This module reports product-level (commodity) price effects from GTAP results.
#
# Key outputs:
#   - Short-run price effects by product (65 GTAP sectors)
#   - Long-run price effects by product
#   - Food aggregate (weighted average of food products)
#
# Formula for food aggregate:
#   food_price = sum(price_effect * weight * is_food) / sum(weight * is_food)
#
# =============================================================================

library(tidyverse)

#' Calculate product-level price effects
#'
#' @param inputs List containing:
#'   - product_prices: Product price data (gtap_sector, sr_price_effect, lr_price_effect, is_food)
#'   - product_params: Product parameters (gtap_sector, weight) - optional, for weighted aggregates
#'
#' @return List containing:
#'   - products: Data frame with all product-level effects
#'   - food_sr: Short-run food price aggregate
#'   - food_lr: Long-run food price aggregate
calculate_products <- function(inputs) {

  # -------------------------------------------------------------------------
  # Load required data
  # -------------------------------------------------------------------------

  product_data <- inputs$product_prices

  if (is.null(product_data)) {
    stop('Product prices not found in inputs$product_prices')
  }

  # Validate required columns
  required_cols <- c('gtap_sector', 'sr_price_effect', 'lr_price_effect', 'is_food')
  missing_cols <- setdiff(required_cols, names(product_data))
  if (length(missing_cols) > 0) {
    stop('Missing required columns in product_prices: ', paste(missing_cols, collapse = ', '))
  }

  message('  Processing ', nrow(product_data), ' products')

  # -------------------------------------------------------------------------
  # Load weights for aggregation
  # -------------------------------------------------------------------------

  if (!is.null(inputs$product_params) && 'weight' %in% names(inputs$product_params)) {
    weights <- inputs$product_params %>%
      select(gtap_sector, weight)

    product_data <- product_data %>%
      left_join(weights, by = 'gtap_sector')

    message('  Using product weights from product_params')
  } else if ('weight' %in% names(product_data)) {
    message('  Using weights from product_prices')
  } else {
    # Equal weights if none provided
    product_data <- product_data %>%
      mutate(weight = 1)
    message('  Warning: No weights provided, using equal weights')
  }

  # -------------------------------------------------------------------------
  # Calculate food aggregates (weighted average)
  # -------------------------------------------------------------------------

  food_products <- product_data %>%
    filter(is_food == 1)

  if (nrow(food_products) > 0) {
    food_sr <- sum(food_products$sr_price_effect * food_products$weight) /
               sum(food_products$weight)

    food_lr <- sum(food_products$lr_price_effect * food_products$weight) /
               sum(food_products$weight)

    message(sprintf('  Food products: %d', nrow(food_products)))
    message(sprintf('  Food SR price effect: %.4f%%', food_sr))
    message(sprintf('  Food LR price effect: %.4f%%', food_lr))
  } else {
    food_sr <- NA_real_
    food_lr <- NA_real_
    message('  Warning: No food products found')
  }

  # -------------------------------------------------------------------------
  # Summary statistics
  # -------------------------------------------------------------------------

  # Top 5 products by SR price effect
  top_sr <- product_data %>%
    arrange(desc(sr_price_effect)) %>%
    head(5)

  message('\n  Top 5 products by SR price effect:')
  for (i in 1:nrow(top_sr)) {
    message(sprintf('    %s: %.2f%%', top_sr$gtap_sector[i], top_sr$sr_price_effect[i]))
  }

  # -------------------------------------------------------------------------
  # Build results
  # -------------------------------------------------------------------------

  results <- list(
    products = product_data,
    food_sr = food_sr,
    food_lr = food_lr,
    n_products = nrow(product_data),
    n_food = nrow(food_products)
  )

  return(results)
}


#' Print product price summary
#'
#' @param product_results Results from calculate_products()
#' @param top_n Number of top products to show (default 10)
print_product_summary <- function(product_results, top_n = 10) {

  cat('\n----------------------------------------------------------\n')
  cat('PRODUCT PRICE EFFECTS\n')
  cat('----------------------------------------------------------\n')

  cat(sprintf('\nFood Aggregate:\n'))
  cat(sprintf('  Short-run: %.4f%%\n', product_results$food_sr))
  cat(sprintf('  Long-run:  %.4f%%\n', product_results$food_lr))

  cat(sprintf('\nTop %d Products by Short-Run Price Effect:\n', top_n))
  cat(sprintf('%-8s %-35s %10s %10s\n', 'Sector', 'Description', 'SR (%)', 'LR (%)'))
  cat('----------------------------------------------------------\n')

  top_products <- product_results$products %>%
    arrange(desc(sr_price_effect)) %>%
    head(top_n)

  for (i in 1:nrow(top_products)) {
    desc <- substr(top_products$description[i], 1, 35)
    cat(sprintf('%-8s %-35s %10.2f %10.2f\n',
                top_products$gtap_sector[i],
                desc,
                top_products$sr_price_effect[i],
                top_products$lr_price_effect[i]))
  }
  cat('----------------------------------------------------------\n')
}
