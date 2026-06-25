#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(scales)
})

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NULL
)
if (is.null(script_path)) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_path <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[[1]])
  } else {
    file.path(getwd(), "src", "plot_pce_price_correlations.R")
  }
}

repo_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  winslash = "/",
  mustWork = TRUE
)

scenario_paths <- tibble::tribble(
  ~scenario, ~label, ~path,
  "2-21_temp", "2-21 temp", file.path(repo_root, "output", "2-21_temp", "results", "pce_category_prices.csv"),
  "2-21_perm", "2-21 perm", file.path(repo_root, "output", "2-21_perm", "results", "pce_category_prices.csv")
)

load_scenario_prices <- function(path, label) {
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      scenario = label,
      nipa_line,
      pce_category,
      pre_sub,
      pe_postsub,
      ge
    )
}

plot_data <- bind_rows(lapply(seq_len(nrow(scenario_paths)), function(i) {
  load_scenario_prices(scenario_paths$path[[i]], scenario_paths$label[[i]])
})) %>%
  pivot_longer(
    cols = c(pre_sub, pe_postsub),
    names_to = "x_measure",
    values_to = "x_value"
  ) %>%
  mutate(
    panel = recode(
      x_measure,
      pre_sub = "Pre-sub vs GE",
      pe_postsub = "PE-sub vs GE"
    ),
    scenario = factor(scenario, levels = c("2-21 temp", "2-21 perm")),
    panel = factor(panel, levels = c("Pre-sub vs GE", "PE-sub vs GE"))
  )

correlations <- plot_data %>%
  group_by(scenario, panel) %>%
  summarise(
    correlation = cor(x_value, ge, use = "complete.obs"),
    .groups = "drop"
  )

label_positions <- plot_data %>%
  group_by(scenario, panel) %>%
  summarise(
    x = quantile(x_value, 0.05, na.rm = TRUE),
    y = quantile(ge, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

correlation_labels <- correlations %>%
  left_join(label_positions, by = c("scenario", "panel")) %>%
  mutate(label = sprintf("r = %.3f", correlation))

plot_obj <- ggplot(plot_data, aes(x = x_value, y = ge)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "grey80") +
  geom_vline(xintercept = 0, linewidth = 0.25, color = "grey80") +
  geom_point(color = "#1b5e20", alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, color = "#8e2c0b") +
  geom_text(
    data = correlation_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3.6
  ) +
  facet_grid(rows = vars(scenario), cols = vars(panel), scales = "free") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "PCE Category Price Effects: GE vs Partial-Equilibrium Measures",
    x = "Category price effect",
    y = "GE category price effect"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

output_dir <- file.path(repo_root, "output", "comparison")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

plot_path <- file.path(output_dir, "pce_price_correlations_2-21_temp_vs_perm.png")
correlation_path <- file.path(output_dir, "pce_price_correlations_2-21_temp_vs_perm.csv")

ggsave(plot_path, plot_obj, width = 11, height = 8, dpi = 300)
write_csv(correlations, correlation_path)

print(correlations)
message("Wrote plot: ", plot_path)
message("Wrote correlations: ", correlation_path)
