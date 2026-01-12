library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(forcats)

read_sheet_raw <- function(path, sheet) {
  suppressMessages(
    suppressWarnings(
      read_excel(
        path,
        sheet = sheet,
        col_names = FALSE,
        col_types = "text",
        .name_repair = "unique"
      )
    )
  )
}

as_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  readr::parse_number(as.character(x))
}

as_excel_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  num <- as_numeric(x)
  if (all(is.na(num))) {
    return(as.Date(x))
  }
  as.Date(num, origin = "1899-12-30")
}

theme_report_base <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(color = "black"),
      plot.subtitle = element_text(color = "black"),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90")
    )
}

theme_report_table <- function(base_size = 11) {
  theme_void(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      plot.title = element_text(color = "black")
    )
}

resolve_data_path <- function(scenario = NULL) {
  if (!is.null(scenario)) {
    scenario_path <- file.path("output", scenario, "report", "data_download.xlsx")
    if (file.exists(scenario_path)) {
      return(scenario_path)
    }
  }
  file.path("reports", "data_download_template.xlsx")
}

table_plot <- function(df,
                       col_labels,
                       value_format = label_number(),
                       title = NULL,
                       value_x_start = 2,
                       header_size = 3.6,
                       body_size = 3.6) {
  df <- df %>% mutate(row = row_number())
  value_cols <- setdiff(names(df), c(names(df)[1], "row"))
  df <- df %>%
    mutate(across(all_of(value_cols), ~ as_numeric(.)))

  n_rows <- nrow(df)
  section_rows <- which(rowSums(!is.na(df[value_cols])) == 0)

  header_x <- c(1, seq_len(length(col_labels) - 1) + value_x_start - 1)
  header_df <- tibble(
    x = header_x,
    y = n_rows + 1,
    label = col_labels
  )

  label_df <- tibble(
    x = 1,
    y = n_rows - df$row + 1,
    label = df[[1]],
    is_section = df$row %in% section_rows
  )

  value_df <- df %>%
    mutate(y = n_rows - row + 1) %>%
    pivot_longer(cols = all_of(value_cols), names_to = "col", values_to = "value") %>%
    mutate(
      x = match(col, value_cols) + value_x_start - 1,
      is_first = match(col, value_cols) == 1,
      label = ifelse(is.na(value), "", value_format(value))
    )

  ggplot() +
    geom_text(
      data = header_df,
      aes(x = x, y = y, label = label),
      fontface = "bold",
      color = "black",
      size = header_size
    ) +
    geom_text(
      data = label_df,
      aes(x = x, y = y, label = label, fontface = ifelse(is_section, "bold", "plain")),
      hjust = 0,
      color = "black",
      size = body_size
    ) +
    geom_text(
      data = value_df,
      aes(x = x, y = y, label = label, hjust = ifelse(is_first, 0, 0.5)),
      color = "black",
      size = body_size
    ) +
    geom_hline(yintercept = n_rows + 0.5, linewidth = 0.3, color = "grey40") +
    scale_x_continuous(limits = c(0.8, value_x_start + length(col_labels) - 1 + 0.2)) +
    scale_y_continuous(limits = c(0.5, n_rows + 1.5)) +
    coord_cartesian(clip = "off") +
    theme_report_table(base_size = 11) +
    theme(plot.title = element_text(hjust = 0, face = "bold"))
}

plot_t1 <- function(path) {
  raw <- read_sheet_raw(path, "T1")
  header_row <- raw[5, ]
  data <- raw %>% slice(6:n())
  non_empty_cols <- colSums(!is.na(data)) > 0
  data <- data[, non_empty_cols, drop = FALSE]
  if (ncol(data) < 2) {
    stop("T1 data is missing baseline values.")
  }
  col_count <- ncol(data)
  names(data) <- c("Metric", paste0("Value_", seq_len(col_count - 1)))
  header_labels <- header_row[2:col_count] %>%
    unlist() %>%
    as.character() %>%
    str_trim()
  header_labels[is.na(header_labels)] <- ""
  if (length(header_labels) == 1 && header_labels == "") {
    header_labels <- "Baseline"
  }
  table_plot(
    data,
    col_labels = c("", header_labels),
    value_format = label_number(accuracy = 0.01, big.mark = ","),
    title = raw[[1]][1],
    value_x_start = 4.2,
    header_size = 3.6,
    body_size = 3.4
  )
}

plot_t2 <- function(path) {
  raw <- read_sheet_raw(path, "T2")
  header_row_idx <- which(raw[[2]] == "Pre-Substitution")[1]
  header_row <- raw[header_row_idx, ]
  data <- raw %>% slice((header_row_idx + 1):n())
  first_col <- names(data)[1]
  data <- data %>% filter(!is.na(.data[[first_col]]))
  names(data) <- c(
    "Region",
    "Pre_Sub_1", "Post_Sub_1",
    "Pre_Sub_2", "Post_Sub_2",
    "Pre_Sub_3", "Post_Sub_3"
  )
  col_labels <- c(
    "",
    as.character(header_row[[2]]), as.character(header_row[[3]]),
    as.character(header_row[[4]]), as.character(header_row[[5]]),
    as.character(header_row[[6]]), as.character(header_row[[7]])
  )
  col_labels <- str_replace_all(col_labels, "Pre-Substitution", "Pre\nSubstitution")
  col_labels <- str_replace_all(col_labels, "Post-Substitution", "Post\nSubstitution")

  table_plot(
    data,
    col_labels = col_labels,
    value_format = label_number(accuracy = 0.1),
    title = raw[[1]][1],
    value_x_start = 2.8,
    header_size = 3.2,
    body_size = 3.6
  )
}

plot_f1 <- function(path) {
  raw <- read_sheet_raw(path, "F1")
  header_row_idx <- which(raw[[1]] == "Year")[1]
  data <- raw %>% slice((header_row_idx + 1):n())
  names(data) <- raw[header_row_idx, ] %>% unlist() %>% as.character()

  data <- data %>%
    mutate(Year = as_numeric(Year)) %>%
    mutate(across(-Year, ~ as_numeric(.)))

  data_long <- data %>%
    pivot_longer(-Year, names_to = "Series", values_to = "Value")

  ggplot(data_long, aes(x = Year, y = Value, color = Series, linetype = Series)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = raw[[1]][1],
      subtitle = raw[[1]][2],
      y = "Average effective tariff rate",
      x = NULL
    ) +
    theme_report_base(base_size = 11) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.box.just = "center",
      legend.spacing.x = unit(6, "pt"),
      legend.margin = margin(t = 4),
      plot.margin = margin(5.5, 5.5, 34, 5.5)
    )
}

plot_f2 <- function(path) {
  raw <- read_sheet_raw(path, "F2")
  header_row_idx <- which(raw[[1]] == "Date")[1]
  data <- raw %>% slice((header_row_idx + 1):n())
  names(data) <- raw[header_row_idx, ] %>% unlist() %>% as.character()

  value_col <- names(data)[2]
  data <- data %>%
    mutate(
      Date = as_excel_date(Date),
      Value = as_numeric(.data[[value_col]])
    )

  ggplot(data, aes(x = Date, y = Value)) +
    geom_line(linewidth = 0.8, color = "#1d81a2") +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = raw[[1]][1],
      subtitle = raw[[1]][2],
      y = "Percent change in real GDP level",
      x = NULL
    ) +
    theme_report_base(base_size = 11)
}

plot_f3 <- function(path) {
  raw <- read_sheet_raw(path, "F3")
  header_row_idx <- which(raw[[2]] == "All 2025 Tariffs to Date")[1]
  data <- raw %>% slice((header_row_idx + 1):n())
  first_col <- names(data)[1]
  data <- data %>% filter(!is.na(.data[[first_col]]))
  names(data) <- c("Sector", "Value")
  data <- data %>% mutate(Value = as_numeric(Value))

  ggplot(data, aes(x = fct_reorder(Sector, Value), y = Value)) +
    geom_col(fill = "#1d81a2") +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = raw[[1]][1],
      subtitle = raw[[1]][2],
      y = "Percent change in long-run real GDP",
      x = NULL
    ) +
    theme_report_base(base_size = 11)
}

plot_f4 <- function(path) {
  raw <- read_sheet_raw(path, "F4")
  header_row_idx <- which(raw[[2]] == "All 2025 Tariffs to Date")[1]
  data <- raw %>% slice((header_row_idx + 1):n())
  first_col <- names(data)[1]
  data <- data %>% filter(!is.na(.data[[first_col]]))
  names(data) <- c("Region", "Value")
  data <- data %>% mutate(Value = as_numeric(Value))

  ggplot(data, aes(x = fct_reorder(Region, Value), y = Value)) +
    geom_col(fill = "#15607a") +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = raw[[1]][1],
      subtitle = raw[[1]][2],
      y = "Percent change in long-run real GDP",
      x = NULL
    ) +
    theme_report_base(base_size = 11)
}

plot_t3 <- function(path) {
  raw <- read_sheet_raw(path, "T3")
  header_row_idx <- which(as_numeric(raw[[2]]) == 2026)[1]
  header_row <- raw[header_row_idx, ]
  data <- raw %>% slice((header_row_idx + 1):n())
  first_col <- names(data)[1]
  data <- data %>% filter(!is.na(.data[[first_col]]))
  names(data) <- c("Measure", header_row[-1] %>% unlist() %>% as.character())

  table_plot(
    data,
    col_labels = c("", header_row[-1] %>% unlist() %>% as.character()),
    value_format = label_number(accuracy = 0.1, big.mark = ","),
    title = raw[[1]][1],
    value_x_start = 3.3,
    header_size = 3.4,
    body_size = 3.6
  )
}

plot_f5 <- function(path) {
  raw <- read_sheet_raw(path, "F5")
  percent_row <- which(raw[[1]] == "% of ATI")[1]
  dollar_row <- which(raw[[1]] == "2025$")[1]

  deciles <- as_numeric(raw[percent_row, 2:11] %>% unlist())
  percent_values <- as_numeric(raw[percent_row + 2, 2:11] %>% unlist())
  dollar_values <- as_numeric(raw[dollar_row + 2, 2:11] %>% unlist())

  percent_df <- tibble(Decile = deciles, Value = percent_values)
  dollar_df <- tibble(Decile = deciles, Value = dollar_values)

  percent_plot <- ggplot(percent_df, aes(x = factor(Decile), y = Value)) +
    geom_col(fill = "#1d81a2") +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = raw[[1]][1],
      subtitle = raw[[1]][2],
      y = "Percent of after-tax income",
      x = "Income decile"
    ) +
    theme_report_base(base_size = 11)

  dollar_plot <- ggplot(dollar_df, aes(x = factor(Decile), y = Value)) +
    geom_col(fill = "#15607a") +
    scale_y_continuous(labels = label_dollar(accuracy = 1)) +
    labs(
      title = raw[[1]][1],
      subtitle = raw[[1]][2],
      y = "2025 dollars",
      x = "Income decile"
    ) +
    theme_report_base(base_size = 11)

  list(percent_plot = percent_plot, dollar_plot = dollar_plot)
}

plot_f6 <- function(path) {
  raw <- read_sheet_raw(path, "F6")
  header_row_idx <- which(raw[[1]] == "Name")[1]
  data <- raw %>% slice((header_row_idx + 1):n())
  first_col <- names(data)[1]
  data <- data %>% filter(!is.na(.data[[first_col]]))
  names(data) <- c("Commodity", "Short-Run", "Long-Run")

  table_plot(
    data,
    col_labels = c("", "Short-Run", "Long-Run"),
    value_format = label_percent(scale = 1, accuracy = 0.1),
    title = raw[[1]][1]
  )
}

build_mockups <- function(data_path) {
  list(
    T1 = plot_t1(data_path),
    T2 = plot_t2(data_path),
    F1 = plot_f1(data_path),
    F2 = plot_f2(data_path),
    F3 = plot_f3(data_path),
    F4 = plot_f4(data_path),
    T3 = plot_t3(data_path),
    F5 = plot_f5(data_path),
    F6 = plot_f6(data_path)
  )
}

save_mockups <- function(output_dir = NULL, scenario = NULL) {
  if (is.null(output_dir)) {
    output_dir <- if (!is.null(scenario)) {
      file.path("output", scenario, "images")
    } else {
      file.path("output", "images")
    }
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  data_path <- resolve_data_path(scenario)
  plots <- build_mockups(data_path)

  ggsave(file.path(output_dir, "T1.png"), plots$T1, width = 8, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "T2.png"), plots$T2, width = 8, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F1.png"), plots$F1, width = 8, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F2.png"), plots$F2, width = 8, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F3.png"), plots$F3, width = 7, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F4.png"), plots$F4, width = 7, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "T3.png"), plots$T3, width = 9, height = 3, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F5_percent.png"), plots$F5$percent_plot, width = 8, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F5_dollars.png"), plots$F5$dollar_plot, width = 8, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(output_dir, "F6.png"), plots$F6, width = 8, height = 10, dpi = 300, bg = "white")

  invisible(plots)
}
