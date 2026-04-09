###############################
### Trade exclusion figure with sensitivity bounds for all products ####
###############################
library(pacman)
p_load(tidyverse, ggsci, gridExtra, ggtext)

# setwd("../data")

eu_countries <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)

target_countries <- c("BRA", "ARG", "PRY")

# Load sensitivity analysis data for all products
sensitivity_data_soybean <- readRDS("../data/simulation_results_soybean_9scenarios.rds")
sensitivity_data_soyoil <- readRDS("../data/simulation_results_soyoil_9scenarios.rds")
sensitivity_data_soycake <- readRDS("../data/simulation_results_soycake_9scenarios.rds")

# Process export data with sensitivity bounds
process_export_sensitivity <- function(data, product_name) {
  data %>%
    filter(
      exporter != importer,
      exporter %in% target_countries,
      importer %in% eu_countries
    ) %>%
    group_by(exporter, scenario_type, tariff_category, elasticity_category) %>%
    summarize(
      x_bln = sum(tradehat_bln),
      x_cfl = sum(tradehat_cfl),
      .groups = "drop"
    ) %>%
    mutate(
      export_change = ((x_cfl - x_bln) / x_bln) * 100,
      product = product_name
    ) %>%
    mutate(
      exporter = case_when(
        exporter == "ARG" ~ "Argentina",
        exporter == "BRA" ~ "Brazil",
        exporter == "PRY" ~ "Paraguay",
        TRUE ~ exporter
      ),
      scenario = case_when(
        scenario_type == "compliance" ~ "Compliance",
        scenario_type == "non_compliance" ~ "Non-compliance"
      )
    )
}

# Process sensitivity data for all products
processed_soybean <- process_export_sensitivity(sensitivity_data_soybean, "Soybean")
processed_soyoil <- process_export_sensitivity(sensitivity_data_soyoil, "Soybean oil")
processed_soycake <- process_export_sensitivity(sensitivity_data_soycake, "Soybean cake")

# Combine all products
all_processed_sensitivity <- bind_rows(
  processed_soybean,
  processed_soyoil,
  processed_soycake
)

# Create bounds data (min and max across elasticity bounds for each scenario)
bounds_data <- all_processed_sensitivity %>%
  group_by(exporter, scenario, tariff_category, product) %>%
  summarize(
    min_change = min(export_change),
    max_change = max(export_change),
    baseline_change = export_change[elasticity_category == "Baseline"],
    .groups = "drop"
  ) %>%
  mutate(tariff_category = factor(tariff_category,
    levels = c("Low (2%)", "Medium (6%)", "High (10%)")
  ))

# Create export restriction plot with sensitivity bounds for all products
export_sensitivity_plot <- ggplot(bounds_data, aes(y = exporter, group = interaction(product, exporter))) +
  # Error bars showing sensitivity range (horizontal)
  geom_errorbarh(aes(xmin = min_change, xmax = max_change, color = product),
    position = position_dodge(width = 0.4), height = 0.3, size = 1.2
  ) +
  # Points for baseline estimates
  geom_point(aes(x = baseline_change, color = product, shape = product),
    position = position_dodge(width = 0.4), size = 3
  ) +
  # Add text labels for baseline values using ggrepel - CORRECTED
  geom_text_repel(aes(x = baseline_change, label = sprintf("%.1f%%", baseline_change), color = product),
    size = 2.8, fontface = "bold"
  ) + # For reproducible positioning
  facet_grid(scenario ~ tariff_category,
    scales = "free_x",
    labeller = labeller(tariff_category = c(
      "Low (2%)" = "Lower compliance",
      "Medium (6%)" = "Medium compliance",
      "High (10%)" = "Higher compliance"
    ))
  ) +
  scale_color_manual(
    values = c(
      "Soybean" = "#1f77b4",
      "Soybean cake" = "#ff7f0e",
      "Soybean oil" = "#2ca02c"
    ),
    name = "Products"
  ) +
  scale_shape_manual(
    values = c(
      "Soybean" = 16,
      "Soybean cake" = 17,
      "Soybean oil" = 15
    ),
    name = "Products"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.spacing = unit(0.8, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  labs(
    y = "",
    x = "Export Change (%)",
    title = "",
    subtitle = ""
  ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1)
  )

ggsave(
  filename = "../images/export_sensitivity_bounds_all_products.png",
  plot = export_sensitivity_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

geom_text_repel(aes(x = baseline_change, label = sprintf("%.1f%%", baseline_change), color = product),
  size = 2.8, fontface = "bold",
  box.padding = 0.5, # More space around text boxes
  point.padding = 0.3, # More space around points
  segment.color = "grey60", # Lighter connecting lines
  segment.size = 0.2, # Thinner connecting lines
  min.segment.length = 0.1, # Minimum segment length
  max.overlaps = Inf, # Allow all labels
  direction = "both", # Allow labels in all directions
  nudge_x = 0.5, # Slight nudge to the right
  seed = 123
)

####################################################################
### Trade diversion with sensitivity bounds for all products ######
####################################################################

process_trade_matrix_sensitivity <- function(data, product_name) {
  data %>%
    filter(exporter != importer) %>%
    mutate(
      exporter = case_when(
        exporter %in% c("BRA", "USA", "ARG", "CAN", "PRY") ~ exporter,
        exporter %in% eu_countries ~ "EU27",
        exporter %in% c(
          "CHE", "NOR", "GBR", "ISL", "LIE", "AND", "MCO", "SMR",
          "VAT", "ALB", "BIH", "MKD", "MNE", "SRB", "UKR", "BLR",
          "MDA", "RUS"
        ) ~ "ROE",
        exporter %in% c(
          "BOL", "CHL", "COL", "ECU", "GUF", "GUY", "PER", "SUR",
          "URY", "VEN"
        ) ~ "ROSA",
        TRUE ~ "ROW"
      ),
      importer = case_when(
        importer == "CHN" ~ "China",
        importer %in% eu_countries ~ "EU-27",
        TRUE ~ "ROW"
      ),
      scenario = case_when(
        scenario_type == "compliance" ~ "Compliance",
        scenario_type == "non_compliance" ~ "Non-compliance"
      )
    ) %>%
    group_by(exporter, importer, scenario, tariff_category, elasticity_category) %>%
    summarize(
      trade_bln = sum(tradehat_bln),
      trade_cfl = sum(tradehat_cfl),
      .groups = "drop"
    ) %>%
    mutate(
      change = ifelse(trade_bln == 0, NA, (trade_cfl - trade_bln) / trade_bln * 100),
      product = product_name
    )
}

# Process trade matrix with sensitivity for all products
trade_matrix_soybean <- process_trade_matrix_sensitivity(sensitivity_data_soybean, "Soybean")
trade_matrix_soyoil <- process_trade_matrix_sensitivity(sensitivity_data_soyoil, "Soybean oil")
trade_matrix_soycake <- process_trade_matrix_sensitivity(sensitivity_data_soycake, "Soybean cake")

# Combine all products
all_trade_matrix <- bind_rows(
  trade_matrix_soybean,
  trade_matrix_soyoil,
  trade_matrix_soycake
)

# Create bounds for trade diversion (baseline scenario, medium tariff)
trade_bounds_all <- all_trade_matrix %>%
  filter(tariff_category == "Medium (6%)") %>%
  group_by(exporter, importer, scenario, product) %>%
  summarize(
    min_change = min(change, na.rm = TRUE),
    max_change = max(change, na.rm = TRUE),
    baseline_change = change[elasticity_category == "Baseline"],
    range_change = max_change - min_change,
    .groups = "drop"
  ) %>%
  filter(!is.infinite(min_change), !is.infinite(max_change))

# Create trade diversion sensitivity plot for all products
create_trade_sensitivity_plot_all <- function(data, scenario_filter) {
  plot_data <- data %>% filter(scenario == scenario_filter)

  ggplot(plot_data, aes(x = importer, y = exporter)) +
    geom_tile(fill = "lightblue", color = "white", width = 0.95, height = 0.95) +
    geom_richtext(aes(label = sprintf("<b>%.1f</b><br>[%.1f, %.1f]", baseline_change, min_change, max_change)),
      size = 5, color = "black"
    ) +
    facet_wrap(~product, ncol = 3) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      strip.text = element_text(size = 11, face = "bold")
    ) +
    labs(
      x = "Importer",
      y = "Exporter",
      title = "",
      subtitle = ""
    )
}
# Create plots for both scenarios
compliance_plot_all <- create_trade_sensitivity_plot_all(trade_bounds_all, "Compliance")
non_compliance_plot_all <- create_trade_sensitivity_plot_all(trade_bounds_all, "Non-compliance")

# Save individual plots
ggsave(
  filename = "../images/trade_diversion_sensitivity_compliance_all_products.png",
  plot = compliance_plot_all,
  width = 14,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../images/trade_diversion_sensitivity_non_compliance_all_products.png",
  plot = non_compliance_plot_all,
  width = 14,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

############################################################
######## Welfare loss with sensitivity bounds for all products ##############
###########################################################

process_welfare_sensitivity <- function(data, product_name) {
  data %>%
    group_by(importer, scenario_type, tariff_category, elasticity_category) %>%
    summarize(
      e_bln = sum(tradehat_bln),
      e_cfl = sum(tradehat_cfl),
      price_index_bln = mean(price_index_bln),
      price_index_cfl = mean(price_index_cfl),
      hat_price_index = mean(hat_price_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_loss = e_cfl * (1 / price_index_cfl - 1 / price_index_bln),
      price_change = (hat_price_index - 1) * 100,
      scenario = case_when(
        scenario_type == "non_compliance" ~ "Non-compliance",
        TRUE ~ str_to_title(scenario_type)
      )
    ) %>%
    filter(importer %in% eu_countries) %>%
    group_by(scenario, tariff_category, elasticity_category) %>%
    summarize(
      welfare_loss = sum(welfare_loss),
      price_change = mean(price_change),
      .groups = "drop"
    ) %>%
    mutate(product = product_name)
}

# Process welfare sensitivity for all products
welfare_soybean <- process_welfare_sensitivity(sensitivity_data_soybean, "Soybean")
welfare_soyoil <- process_welfare_sensitivity(sensitivity_data_soyoil, "Soybean oil")
welfare_soycake <- process_welfare_sensitivity(sensitivity_data_soycake, "Soybean cake")

# Combine all products
all_welfare_sensitivity <- bind_rows(
  welfare_soybean,
  welfare_soyoil,
  welfare_soycake
)

# Create welfare bounds for all products
welfare_bounds_all <- all_welfare_sensitivity %>%
  group_by(scenario, tariff_category, product) %>%
  summarize(
    min_welfare = min(welfare_loss),
    max_welfare = max(welfare_loss),
    baseline_welfare = welfare_loss[elasticity_category == "Baseline"],
    min_price = min(price_change),
    max_price = max(price_change),
    baseline_price = price_change[elasticity_category == "Baseline"],
    .groups = "drop"
  ) %>%
  mutate(tariff_category = factor(tariff_category,
    levels = c("Low (2%)", "Medium (6%)", "High (10%)")
  ))

# Create welfare sensitivity plot for all products
welfare_sensitivity_plot_all <- welfare_bounds_all %>%
  ggplot(aes(x = scenario, y = tariff_category)) +
  geom_tile(fill = "lightblue", color = "white", width = 0.95, height = 0.95) +
  geom_text(
    aes(label = sprintf(
      "%.2f M\n(%.2f%%)",
      baseline_welfare / 1e3,
      baseline_price
    )),
    size = 3.2,
    color = "black"
  ) +
  facet_wrap(~product, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  labs(
    x = "",
    y = "",
    title = "",
    subtitle = ""
  )

ggsave(
  filename = "../images/welfare_sensitivity_bounds_all_products.png",
  plot = welfare_sensitivity_plot_all,
  width = 9,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)


## Reporting major indexes#####
target_countries <- c("BRA", "ARG", "PRY", "USA", "CAN", "CHN")

process_welfare_sensitivity <- function(data, product_name) {
  data %>%
    group_by(importer, scenario_type, tariff_category, elasticity_category) %>%
    summarize(
      hat_welfare = mean(hat_welfare, na.rm = TRUE),
      hat_price_index = mean(hat_price_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      data %>%
        group_by(exporter, scenario_type, tariff_category, elasticity_category) %>%
        summarize(
          hat_producer_price = mean(hat_producer_price, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("importer" = "exporter", "scenario_type", "tariff_category", "elasticity_category")
    ) %>%
    mutate(
      hat_tot = hat_producer_price / hat_price_index,
      welfare = (hat_welfare - 1) * 100,
      price_index = (hat_price_index - 1) * 100,
      producer_price = (hat_producer_price - 1) * 100,
      TOT = (hat_tot - 1) * 100,
      product = product_name
    )
}

welfare_soybean <- process_welfare_sensitivity(sensitivity_data_soybean, "Soybean")
welfare_soyoil <- process_welfare_sensitivity(sensitivity_data_soyoil, "Soybean oil")
welfare_soycake <- process_welfare_sensitivity(sensitivity_data_soycake, "Soybean cake")

welfare_all <- bind_rows(welfare_soybean, welfare_soyoil, welfare_soycake) %>%
  mutate(importer = ifelse(importer == "0-AUS", "AUS", importer))

sensitivity_plot_data <- welfare_all %>%
  filter(importer %in% target_countries) %>%
  mutate(
    country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name"),
    scenario_label = case_when(
      scenario_type == "compliance" ~ "Compliance",
      scenario_type == "non_compliance" ~ "Non-compliance",
      TRUE ~ scenario_type
    )
  ) %>%
  # Calculate statistics for each metric separately
  group_by(country, scenario_label, product) %>%
  summarize(
    # Welfare
    welfare_baseline = welfare[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    welfare_min = min(welfare, na.rm = TRUE),
    welfare_max = max(welfare, na.rm = TRUE),

    # Price Index
    price_index_baseline = price_index[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    price_index_min = min(price_index, na.rm = TRUE),
    price_index_max = max(price_index, na.rm = TRUE),

    # Producer Price
    producer_price_baseline = producer_price[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    producer_price_min = min(producer_price, na.rm = TRUE),
    producer_price_max = max(producer_price, na.rm = TRUE),

    # Terms of Trade
    TOT_baseline = TOT[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    TOT_min = min(TOT, na.rm = TRUE),
    TOT_max = max(TOT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Remove rows with missing baseline values
  filter(!is.na(welfare_baseline) & !is.na(price_index_baseline) &
    !is.na(producer_price_baseline) & !is.na(TOT_baseline))

# Create separate datasets for each metric
create_metric_data <- function(data, metric_name, baseline_col, min_col, max_col, label) {
  data %>%
    dplyr::select(country, scenario_label, product,
      baseline = all_of(baseline_col),
      min_val = all_of(min_col),
      max_val = all_of(max_col)
    ) %>%
    mutate(metric = metric_name, metric_label = label)
}

# Combine all metrics
plot_data <- bind_rows(
  create_metric_data(sensitivity_plot_data, "welfare", "welfare_baseline", "welfare_min", "welfare_max", "Welfare Change (%)"),
  create_metric_data(sensitivity_plot_data, "price_index", "price_index_baseline", "price_index_min", "price_index_max", "Price Index Change (%)"),
  create_metric_data(sensitivity_plot_data, "producer_price", "producer_price_baseline", "producer_price_min", "producer_price_max", "Producer Price Change (%)"),
  create_metric_data(sensitivity_plot_data, "TOT", "TOT_baseline", "TOT_min", "TOT_max", "Terms of Trade Change (%)")
) %>%
  filter(!is.na(baseline) & is.finite(baseline) &
    !is.na(min_val) & is.finite(min_val) &
    !is.na(max_val) & is.finite(max_val))

# Function to create plot for specific scenario
create_scenario_plot <- function(data, scenario_name) {
  data %>%
    filter(scenario_label == scenario_name) %>%
    ggplot(aes(x = product, y = baseline, color = country)) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbar(
      aes(ymin = min_val, ymax = max_val),
      width = 0.3,
      position = position_dodge(width = 0.6),
      alpha = 0.7
    ) +
    facet_wrap(~metric_label, scales = "free_y", ncol = 2) +
    scale_color_brewer(type = "qual", palette = "Set2", name = "Country") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(
      x = "Product",
      y = "Change (%)",
      title = "",
      subtitle = "",
      caption = ""
    ) +
    guides(color = guide_legend(nrow = 2))
}

# Create separate plots for compliance and non-compliance
compliance_plot <- create_scenario_plot(plot_data, "Compliance")
non_compliance_plot <- create_scenario_plot(plot_data, "Non-compliance")

# Display the plots
print(compliance_plot)
print(non_compliance_plot)

# Save the plots
ggsave(
  filename = "../images/sensitivity_compliance.png",
  plot = compliance_plot,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../images/sensitivity_non_compliance.png",
  plot = non_compliance_plot,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Create summary statistics for each scenario
compliance_summary <- plot_data %>%
  filter(scenario_label == "Compliance") %>%
  group_by(metric_label, product) %>%
  summarize(
    n_countries = n_distinct(country),
    avg_baseline = mean(baseline, na.rm = TRUE),
    avg_sensitivity_range = mean(max_val - min_val, na.rm = TRUE),
    max_sensitivity_range = max(max_val - min_val, na.rm = TRUE),
    .groups = "drop"
  )

non_compliance_summary <- plot_data %>%
  filter(scenario_label == "Non-compliance") %>%
  group_by(metric_label, product) %>%
  summarize(
    n_countries = n_distinct(country),
    avg_baseline = mean(baseline, na.rm = TRUE),
    avg_sensitivity_range = mean(max_val - min_val, na.rm = TRUE),
    max_sensitivity_range = max(max_val - min_val, na.rm = TRUE),
    .groups = "drop"
  )

# Print summaries
cat("COMPLIANCE SCENARIO SUMMARY:\n")
cat("============================\n")
print(compliance_summary)

cat("\n\nNON-COMPLIANCE SCENARIO SUMMARY:\n")
cat("=================================\n")
print(non_compliance_summary)

# Optional: Create a comparison table
comparison_table <- plot_data %>%
  group_by(metric_label, scenario_label) %>%
  summarize(
    avg_baseline = mean(baseline, na.rm = TRUE),
    avg_sensitivity = mean(max_val - min_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = scenario_label,
    values_from = c(avg_baseline, avg_sensitivity),
    names_sep = "_"
  )

cat("\n\nCOMPARISON TABLE:\n")
cat("=================\n")
print(comparison_table)


sensitivity_plot_data <- welfare_all %>%
  filter(importer %in% target_countries) %>%
  mutate(
    country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name"),
    scenario_label = case_when(
      scenario_type == "compliance" ~ "Compliance",
      scenario_type == "non_compliance" ~ "Non-compliance",
      TRUE ~ scenario_type
    )
  ) %>%
  # Calculate statistics for each metric separately
  group_by(country, scenario_label, product) %>%
  summarize(
    # Welfare
    welfare_baseline = welfare[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    welfare_min = min(welfare, na.rm = TRUE),
    welfare_max = max(welfare, na.rm = TRUE),

    # Price Index
    price_index_baseline = price_index[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    price_index_min = min(price_index, na.rm = TRUE),
    price_index_max = max(price_index, na.rm = TRUE),

    # Producer Price
    producer_price_baseline = producer_price[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    producer_price_min = min(producer_price, na.rm = TRUE),
    producer_price_max = max(producer_price, na.rm = TRUE),

    # Terms of Trade
    TOT_baseline = TOT[tariff_category == "Medium (6%)" & elasticity_category == "Baseline"][1],
    TOT_min = min(TOT, na.rm = TRUE),
    TOT_max = max(TOT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Remove rows with missing baseline values
  filter(!is.na(welfare_baseline) & !is.na(price_index_baseline) &
    !is.na(producer_price_baseline) & !is.na(TOT_baseline))

# Create separate datasets for each metric
create_metric_data <- function(data, metric_name, baseline_col, min_col, max_col, label) {
  data %>%
    dplyr::select(country, scenario_label, product,
      baseline = all_of(baseline_col),
      min_val = all_of(min_col),
      max_val = all_of(max_col)
    ) %>%
    mutate(metric = metric_name, metric_label = label)
}

# Combine all metrics
plot_data <- bind_rows(
  create_metric_data(sensitivity_plot_data, "welfare", "welfare_baseline", "welfare_min", "welfare_max", "Welfare Change (%)"),
  create_metric_data(sensitivity_plot_data, "price_index", "price_index_baseline", "price_index_min", "price_index_max", "Price Index Change (%)"),
  create_metric_data(sensitivity_plot_data, "producer_price", "producer_price_baseline", "producer_price_min", "producer_price_max", "Producer Price Change (%)"),
  create_metric_data(sensitivity_plot_data, "TOT", "TOT_baseline", "TOT_min", "TOT_max", "Terms of Trade Change (%)")
) %>%
  filter(!is.na(baseline) & is.finite(baseline) &
    !is.na(min_val) & is.finite(min_val) &
    !is.na(max_val) & is.finite(max_val))

# Create the main plot - 2x2 facets for metrics
sensitivity_plot <- plot_data %>%
  ggplot(aes(x = product, y = baseline, color = country)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(
    aes(ymin = min_val, ymax = max_val),
    width = 0.3,
    position = position_dodge(width = 0.6),
    alpha = 0.7
  ) +
  facet_grid(metric_label ~ scenario_label, scales = "free_y") +
  scale_color_brewer(type = "qual", palette = "Set2", name = "Country") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = "Product",
    y = "Change (%)",
    title = "Sensitivity Analysis: Economic Indicators by Compliance Scenario",
    subtitle = "Points: Medium tariff & baseline elasticity | Error bars: Min-max across all scenarios",
    caption = "Six target countries shown with different colors"
  ) +
  guides(color = guide_legend(nrow = 2))

# Alternative compact version - all in one plot
sensitivity_plot_compact <- plot_data %>%
  mutate(scenario_product = paste(scenario_label, product, sep = "\n")) %>%
  ggplot(aes(x = scenario_product, y = baseline, color = country)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.7)) +
  geom_errorbar(
    aes(ymin = min_val, ymax = max_val),
    width = 0.3,
    position = position_dodge(width = 0.7),
    alpha = 0.7
  ) +
  facet_wrap(~metric_label, scales = "free_y", ncol = 2) +
  scale_color_brewer(type = "qual", palette = "Set2", name = "Country") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = "Scenario and Product",
    y = "Change (%)",
    title = "Sensitivity Analysis: Economic Indicators",
    subtitle = "Points: Medium tariff & baseline elasticity | Error bars: Min-max across all scenarios",
    caption = "Six target countries shown with different colors"
  ) +
  guides(color = guide_legend(nrow = 2))

# Print both plots
print(sensitivity_plot)
print(sensitivity_plot_compact)

# Save the plots
ggsave(
  filename = "../images/sensitivity_analysis_grid.png",
  plot = sensitivity_plot,
  width = 16,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "../images/sensitivity_analysis_compact.png",
  plot = sensitivity_plot_compact,
  width = 14,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Summary statistics
cat("Sensitivity Analysis Summary:\n")
cat("============================\n")
summary_stats <- plot_data %>%
  group_by(metric_label, scenario_label) %>%
  summarize(
    n_observations = n(),
    avg_baseline = mean(baseline, na.rm = TRUE),
    avg_sensitivity_range = mean(max_val - min_val, na.rm = TRUE),
    max_sensitivity_range = max(max_val - min_val, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)


# Function to create sensitivity plot for target countries
create_sensitivity_plot_target <- function(data, metric, title) {
  # Filter for target countries and calculate sensitivity bounds
  sensitivity_data <- data %>%
    filter(importer %in% target_countries) %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(scenario_label = case_when(
      scenario_type == "compliance" ~ "Compliance",
      scenario_type == "non_compliance" ~ "Non-compliance",
      TRUE ~ scenario_type
    )) %>%
    group_by(country, scenario_label, tariff_category, product, elasticity_category) %>%
    summarize(
      min_value = min(.data[[metric]], na.rm = TRUE),
      max_value = max(.data[[metric]], na.rm = TRUE),
      baseline_value = {
        baseline_vals <- .data[[metric]][elasticity_category == "Baseline"]
        if (length(baseline_vals) > 0) baseline_vals[1] else NA
      },
      .groups = "drop"
    ) %>%
    filter(!is.na(baseline_value)) %>%
    # Create combined scenario-tariff label
    mutate(scenario_tariff = paste(scenario_label, tariff_category, sep = "\n"))

  # Create the plot
  sensitivity_data %>%
    ggplot(aes(x = scenario_label, y = country)) +
    geom_tile(aes(fill = baseline_value), color = "white", width = 0.95, height = 0.95) +
    geom_text(aes(label = sprintf("%.2f\n[%.2f, %.2f]", baseline_value, min_value, max_value)),
      size = 3,
      color = "black"
    ) +
    facet_wrap(~product) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 12),
      legend.position = "right",
      panel.grid = element_blank(),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      x = "",
      y = "",
      title = paste(title, "- Target Countries (Sensitivity Analysis)"),
      subtitle = "Baseline value with [minimum, maximum] across elasticity bounds"
    )
}

# Alternative faceted version
create_sensitivity_plot_target_faceted <- function(data, metric, title) {
  sensitivity_data <- data %>%
    filter(importer %in% target_countries) %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(scenario_label = case_when(
      scenario_type == "compliance" ~ "Compliance",
      scenario_type == "non_compliance" ~ "Non-compliance",
      TRUE ~ scenario_type
    )) %>%
    group_by(country, scenario_label, tariff_category, product) %>%
    summarize(
      min_value = min(.data[[metric]], na.rm = TRUE),
      max_value = max(.data[[metric]], na.rm = TRUE),
      baseline_value = {
        baseline_vals <- .data[[metric]][elasticity_category == "Baseline"]
        if (length(baseline_vals) > 0) baseline_vals[1] else NA
      },
      .groups = "drop"
    ) %>%
    filter(!is.na(baseline_value))

  sensitivity_data %>%
    ggplot(aes(x = tariff_category, y = country)) +
    geom_tile(aes(fill = baseline_value), color = "white", width = 0.95, height = 0.95) +
    geom_text(aes(label = sprintf("%.2f\n[%.2f, %.2f]", baseline_value, min_value, max_value)),
      size = 3.5,
      color = "black"
    ) +
    facet_grid(product ~ scenario_label) +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "#F7F7F7",
      high = "#2166AC",
      midpoint = 0,
      name = paste(title, "(%)")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "right",
      panel.grid = element_blank(),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      x = "Tariff Level",
      y = "",
      title = paste(title, "- Target Countries (Sensitivity Analysis)"),
      subtitle = "Baseline value with [minimum, maximum] across elasticity bounds"
    )
}

# Create sensitivity plots for each metric for target countries
target_sensitivity_plots <- list(
  welfare = create_sensitivity_plot_target_faceted(welfare_all, "welfare", "Welfare Change"),
  price_index = create_sensitivity_plot_target_faceted(welfare_all, "price_index", "Price Index Change"),
  producer_price = create_sensitivity_plot_target_faceted(welfare_all, "producer_price", "Producer Price Change"),
  tot = create_sensitivity_plot_target_faceted(welfare_all, "TOT", "Terms of Trade Change")
)

target_sensitivity_plots1 <- list(
  welfare = create_sensitivity_plot_target(welfare_all, "welfare", "Welfare Change"),
  price_index = create_sensitivity_plot_target(welfare_all, "price_index", "Price Index Change"),
  producer_price = create_sensitivity_plot_target(welfare_all, "producer_price", "Producer Price Change"),
  tot = create_sensitivity_plot_target(welfare_all, "TOT", "Terms of Trade Change")
)

# Save target country sensitivity plots
for (metric in names(target_sensitivity_plots)) {
  ggsave(
    filename = paste0("C:\\Users\\Manoj\\OneDrive - Kansas State University\\GRA with Dr. Villoria\\Replicating a paper\\R file\\essay\\images\\", "target_", metric, "_sensitivity.png"),
    plot = target_sensitivity_plots[[metric]],
    width = 14,
    height = 10,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}
