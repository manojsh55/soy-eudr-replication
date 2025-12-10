###############################
### Trade exclusion figure ####
###############################
library(pacman)
p_load(tidyverse, ggsci, gridExtra, cowplot, magick)

# setwd("../data")


## Creation of Figure 1 ###

p1 <- ggdraw() + draw_image("../images/sankeyplots/sankey_HS1201.png")
p2 <- ggdraw() + draw_image("../images/sankeyplots/sankey_HS1507.png")
p3 <- ggdraw() + draw_image("../images/sankeyplots/sankey_HS120810.png")
p4 <- ggdraw() + draw_image("../images/sankeyplots/sankey_HS2304.png")

panel <- plot_grid(
  p1, p2, p3, p4,
  labels = c("a", "b", "c", "d"),
  label_size = 14,
  ncol = 2, align = "hv"
)

final <- plot_grid(panel, caption, ncol = 1, rel_heights = c(1, 0.15))

ggsave("../images/figure_soybean_panel.png", panel, width = 190, height = 140, units = "mm", dpi = 300)


######


eu_countries <- c(
  "AUT", # Austria
  "BEL", # Belgium
  "BGR", # Bulgaria
  "HRV", # Croatia
  "CYP", # Cyprus
  "CZE", # Czech Republic
  "DNK", # Denmark
  "EST", # Estonia
  "FIN", # Finland
  "FRA", # France
  "DEU", # Germany
  "GRC", # Greece
  "HUN", # Hungary
  "IRL", # Ireland
  "ITA", # Italy
  "LVA", # Latvia
  "LTU", # Lithuania
  "LUX", # Luxembourg
  "MLT", # Malta
  "NLD", # Netherlands
  "POL", # Poland
  "PRT", # Portugal
  "ROU", # Romania
  "SVK", # Slovakia
  "SVN", # Slovenia
  "ESP", # Spain
  "SWE" # Sweden
)

target_countries <- c("BRA", "ARG", "PRY")

process_export_data <- function(data, product_name) {
  data %>%
    filter(
      exporter != importer,
      exporter %in% target_countries,
      importer %in% eu_countries
    ) %>%
    group_by(exporter, tariff_scenario) %>%
    summarize(
      x_bln = sum(tradehat_bln),
      x_cfl = sum(tradehat_cfl),
      .groups = "drop"
    ) %>%
    mutate(
      export_rest = (x_cfl - x_bln) / x_bln,
      product = product_name
    ) %>%
    mutate(
      exporter = case_when(
        exporter == "ARG" ~ "Argentina",
        exporter == "BRA" ~ "Brazil",
        exporter == "PRY" ~ "Paraguay",
        TRUE ~ exporter
      ),
      scenario = as.character(tariff_scenario),
      export_change = export_rest * 100
    )
}

# Process data for each product
processed_soybean <- process_export_data(
  readRDS("../data/simulation_results_soybean.rds"),
  "Soybean"
)

processed_soyoil <- process_export_data(
  readRDS("../data/simulation_results_soyoil.rds"),
  "Soybean oil"
)

processed_soycake <- process_export_data(
  readRDS("../data/simulation_results_soycake.rds"),
  "Soybean cake"
)


# Combine all processed data
all_processed_data <- bind_rows(
  processed_soybean,
  processed_soyoil,
  processed_soycake
)

# Create combined plot
export_restriction_plot <- ggplot(
  all_processed_data,
  aes(x = exporter, y = export_change, fill = scenario)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", export_change)),
    position = position_dodge(width = 0.8),
    vjust = ifelse(all_processed_data$export_change < 0, 1.2, -0.5),
    size = 4
  ) +
  facet_wrap(~product, scales = "free_y") +
  scale_fill_manual(
    values = c("#92C5DE", "#B2182B"),
    labels = c("Compliance", "Non-compliance"),
    name = "Scenarios"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 0),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = "Export Change (%)"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  )

ggsave(
  filename = "../images/export_restriction_plot_eu1.png",
  plot = export_restriction_plot,
  width = 11,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)


####################################################################
### Trade diversion plots ##########################################
####################################################################

process_trade_matrix <- function(data, product_name) {
  data %>%
    filter(exporter != importer) %>%
    mutate(
      exporter = toupper(exporter),
      importer = toupper(importer)
    ) %>%
    mutate(
      exporter = case_when(
        exporter == "BRA" ~ "Brazil",
        exporter == "USA" ~ "USA",
        exporter == "ARG" ~ "Argentina",
        exporter == "CAN" ~ "Canada",
        exporter == "PRY" ~ "Paraguay",
        exporter %in% eu_countries ~ "EU-27",
        exporter %in% c(
          "CHE", "NOR", "GBR", "ISL", "LIE", "AND", "MCO", "SMR", "VAT",
          "ALB", "BIH", "MKD", "MNE", "SRB", "UKR", "BLR", "MDA", "RUS"
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
      tariff_scenario = case_when(
        tariff_scenario == "compliance" ~ "Compliance",
        tariff_scenario == "non_compliance" ~ "Non-compliance",
        TRUE ~ tariff_scenario
      )
    ) %>%
    group_by(exporter, importer, tariff_scenario) %>%
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

# Process each product
trade_matrix_soybean <- process_trade_matrix(
  readRDS("../data/simulation_results_soybean.rds"),
  "Soybean"
)

trade_matrix_soyoil <- process_trade_matrix(
  readRDS("../data/simulation_results_soyoil.rds"),
  "Soybean oil"
)

trade_matrix_soycake <- process_trade_matrix(
  readRDS("../data/simulation_results_soycake.rds"),
  "Soybean cake"
)

# Create products list
products <- list(
  soybean = trade_matrix_soybean,
  soyoil = trade_matrix_soyoil,
  soycake = trade_matrix_soycake
)
# Define breaks and colors for the heatmap
breaks <- c(-Inf, -50, -25, -5, 0, 5, 25, 50, 100, Inf)
labels <- c(
  "< -50%", "-50% to -25%", "-25% to -5%",
  "-5% to 0%", "0% to 5%", "5% to 25%", "25% to 50%",
  "50% to 100%", "> 100%"
)

color_mapping <- c(
  "< -50%" = "#B2182B",
  "-50% to -25%" = "#D6604D",
  "-25% to -5%" = "#F4A582",
  "-5% to 0%" = "#FDDBC7",
  "0% to 5%" = "#D1E5F0",
  "5% to 25%" = "#92C5DE",
  "25% to 50%" = "#4393C3",
  "50% to 100%" = "#2166AC",
  "> 100%" = "#003366"
)

colors <- color_mapping[labels]


# Function to create trade diversion plot
create_trade_diversion_plot <- function(data, product_title) {
  ggplot(
    data,
    aes(x = importer, y = exporter)
  ) +
    geom_tile(aes(fill = cut(change, breaks = breaks, labels = labels)),
      color = "white", width = 0.95, height = 0.95
    ) +
    geom_text(
      aes(
        label = sprintf("%.1f%%", change),
        color = ifelse(change > 50 | change < -50, "white", "black")
      ),
      size = 4.5
    ) +
    scale_color_identity() +
    facet_wrap(~tariff_scenario, ncol = 2) +
    scale_fill_manual(
      values = colors,
      name = "Trade Change (%)",
      drop = TRUE
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    ) +
    labs(
      x = "Importer",
      y = "Exporter",
      subtitle = product_title
    )
}

product_titles <- list(
  soybean = "",
  soyoil = "",
  soycake = ""
)

# Create and save plots for each product
for (p in names(products)) {
  plot <- create_trade_diversion_plot(products[[p]], product_titles[[p]])

  ggsave(
    filename = paste0("../images/trade_diversion1_", p, ".png"),
    plot = plot,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

############################################################
######## Welfare loss #######################################
###########################################################

process_welfare_data <- function(data, product_name) {
  data %>%
    group_by(importer, tariff_scenario, tariff) %>%
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
      # Modify the scenario names
      tariff_scenario = case_when(
        tariff_scenario == "non_compliance" ~ "Non-compliance",
        TRUE ~ str_to_title(tariff_scenario)
      )
    ) %>%
    filter(importer %in% eu_countries) %>%
    group_by(tariff_scenario) %>%
    summarize(
      welfare_loss = sum(welfare_loss),
      price_change = mean(price_change)
    ) %>%
    ungroup() %>%
    mutate(product = product_name)
}

# Process each product
welfare_soybean <- process_welfare_data(
  readRDS("../data/simulation_results_soybean.rds"),
  "Soybean"
)

welfare_soyoil <- process_welfare_data(
  readRDS("../data/simulation_results_soyoil.rds"),
  "Soybean oil"
)

welfare_soycake <- process_welfare_data(
  readRDS("../data/simulation_results_soycake.rds"),
  "Soybean cake"
)

welfare_eu <- bind_rows(welfare_soybean, welfare_soycake, welfare_soyoil)


welfare_loss_plot <- welfare_eu %>%
  ggplot(aes(x = tariff_scenario, y = product, fill = welfare_loss)) +
  geom_tile(color = "white", width = 0.95, height = 0.95) +
  geom_text(
    aes(label = sprintf(
      "%.2f M\n(%.2f%%)",
      welfare_loss / 1e3,
      price_change
    )),
    size = 5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    name = expression("Expenditure Change \n(million USD)"),
    labels = function(x) sprintf("%.1f", x / 1e3)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  labs(
    x = "",
    y = ""
  )


ggsave(
  filename = "../images/welfare_loss_eu1.png",
  plot = welfare_loss_plot,
  width = 8,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)

############################################################
######## Report indexes for all #######################################
###########################################################

target_countries <- c("BRA", "ARG", "PRY", "USA", "CAN", "CHN")

process_welfare_data <- function(data, product_name) {
  data %>%
    group_by(importer, tariff_scenario, tariff) %>%
    summarize(
      hat_welfare = mean(hat_welfare, na.rm = TRUE),
      hat_price_index = mean(hat_price_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      data %>%
        group_by(exporter, tariff_scenario, tariff) %>%
        summarize(
          hat_producer_price = mean(hat_producer_price, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("importer" = "exporter", "tariff_scenario", "tariff")
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

welfare_soybean <- process_welfare_data(
  readRDS("../data/simulation_results_soybean.rds"),
  "Soybean"
)

welfare_soyoil <- process_welfare_data(
  readRDS("../data/simulation_results_soyoil.rds"),
  "Soybean oil"
)

welfare_soycake <- process_welfare_data(
  readRDS("../data/simulation_results_soycake.rds"),
  "Soybean cake"
)


welfare_all <- bind_rows(welfare_soybean, welfare_soyoil, welfare_soycake) %>%
  mutate(importer = ifelse(importer == "0-AUS", "AUS", importer))


# Function to create plot for  all countries ########
create_metric_plot <- function(data, metric, title) {
  data %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(tariff_scenario = case_when(
      tariff_scenario == "compliance" ~ "Compliance",
      tariff_scenario == "non_compliance" ~ "Non-compliance",
      TRUE ~ tariff_scenario
    )) %>%
    group_by(tariff_scenario, product) %>%
    mutate(
      quartile = factor(
        ntile(.data[[metric]], 4),
        levels = 1:4,
        labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
      )
    ) %>%
    ungroup() %>%
    filter(!(importer %in% target_countries | importer %in% eu_countries)) %>%
    ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
    geom_tile(color = "white", width = 0.95, height = 0.95) +
    geom_text(aes(label = sprintf("%.2f%%", .data[[metric]])),
      size = 5,
      color = "black"
    ) +
    facet_wrap(~product) +
    scale_fill_simpsons(name = "Quartile") +
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
    labs(x = "", y = "")
}

# Create plots for each metric
plots <- list(
  welfare = create_metric_plot(welfare_all, "welfare", "Welfare Change"),
  price_index = create_metric_plot(welfare_all, "price_index", "Price Index Change"),
  producer_price = create_metric_plot(welfare_all, "producer_price", "Producer Price Change"),
  tot = create_metric_plot(welfare_all, "TOT", "Terms of Trade Change")
)

# Save plots
for (metric in names(plots)) {
  ggsave(
    filename = paste0("../images/", "all_", metric, "_changes1.png"),
    plot = plots[[metric]],
    width = 11,
    height = 13,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

panel <- plot_grid(
  plots[["price_index"]], plots[["producer_price"]], plots[["welfare"]], plots[["tot"]],
  labels = c("a", "b", "c", "d"),
  label_size = 14,
  ncol = 1, align = "hv"
)


# Function to create plot by products for the rest of the world:

create_product_plot_improved <- function(data, product_name, title_suffix = "") {
  plot_data <- data %>%
    filter(product == product_name) %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(tariff_scenario = case_when(
      tariff_scenario == "compliance" ~ "Com.",
      tariff_scenario == "non_compliance" ~ "Non-com.",
      TRUE ~ tariff_scenario
    )) %>%
    filter(!(importer %in% target_countries | importer %in% eu_countries)) %>%
    pivot_longer(
      cols = c("welfare", "price_index", "producer_price", "TOT"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = case_when(
        metric == "welfare" ~ "Welfare",
        metric == "price_index" ~ "Price Index",
        metric == "producer_price" ~ "Producer Price",
        metric == "TOT" ~ "Terms of Trade",
        TRUE ~ metric
      ),
      # Order metrics for better display
      metric = factor(metric, levels = c(
        "Welfare", "Price Index",
        "Producer Price", "Terms of Trade"
      ))
    ) %>%
    group_by(metric) %>%
    mutate(
      quartile = factor(
        ntile(value, 4),
        levels = 1:4,
        labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
      )
    ) %>%
    ungroup()

  ggplot(plot_data, aes(x = tariff_scenario, y = country, fill = quartile)) +
    geom_tile(color = "white", width = 0.95, height = 0.95) +
    geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f%%", value))),
      size = 3.5,
      color = "black"
    ) +
    facet_wrap(~metric, ncol = 4) +
    scale_fill_simpsons(name = "Quartile") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      panel.grid = element_blank(),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      legend.box = "horizontal"
    ) +
    labs(
      x = "",
      y = ""
    ) +
    guides(fill = guide_legend(nrow = 1))
}

# Create improved plots for each product
product_plots_improved <- list(
  soybean = create_product_plot_improved(welfare_all, "Soybean"),
  soybean_oil = create_product_plot_improved(welfare_all, "Soybean oil"),
  soybean_cake = create_product_plot_improved(welfare_all, "Soybean cake")
)

# Save the product-based plots
for (product in names(product_plots_improved)) {
  ggsave(
    filename = paste0("../images/", product, "_all_indicators.png"),
    plot = product_plots_improved[[product]],
    width = 7,
    height = 11,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}


# Function to create plot for  only eu countries ########

quartile_colors <- c(
  "Q1 (Lowest)" = "#d6604d",
  "Q2" = "#f4a582",
  "Q3" = "#92c5de",
  "Q4 (Highest)" = "#4393c3"
)


create_metric_plot <- function(data, metric) {
  data %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(tariff_scenario = case_when(
      tariff_scenario == "compliance" ~ "Compliance",
      tariff_scenario == "non_compliance" ~ "Non-compliance",
      TRUE ~ tariff_scenario
    )) %>%
    group_by(tariff_scenario, product) %>%
    mutate(
      quartile = factor(
        ntile(.data[[metric]], 4),
        levels = 1:4,
        labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
      )
    ) %>%
    ungroup() %>%
    filter(importer %in% eu_countries) %>% # Keep consistent
    ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
    geom_tile(color = "white", width = 0.95, height = 0.95) +
    geom_text(aes(label = sprintf("%.2f%%", .data[[metric]])),
      size = 6,
      color = "black"
    ) +
    facet_wrap(~product) +
    scale_fill_manual(
      name = "Quartile",
      values = quartile_colors,
      drop = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid = element_blank(),
      legend.position = "none", # Changed to "none"
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(x = "", y = "")
}

# Create plots for each metric
plots <- list(
  welfare = create_metric_plot(welfare_all, "welfare"),
  price_index = create_metric_plot(welfare_all, "price_index"),
  producer_price = create_metric_plot(welfare_all, "producer_price"),
  tot = create_metric_plot(welfare_all, "TOT")
)

combined_plot <- plot_grid(
  plots[["welfare"]], plots[["price_index"]], plots[["producer_price"]], plots[["tot"]],
  labels = c("a", "b", "c", "d"),
  label_size = 14,
  ncol = 2, align = "hv"
)

# Create legend plot using same logic as main plots
plot_with_legend <- welfare_all %>%
  mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
  mutate(tariff_scenario = case_when(
    tariff_scenario == "compliance" ~ "Compliance",
    tariff_scenario == "non_compliance" ~ "Non-compliance",
    TRUE ~ tariff_scenario
  )) %>%
  group_by(tariff_scenario, product) %>%
  mutate(
    quartile = factor(
      ntile(TOT, 4),
      levels = 1:4,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
    )
  ) %>%
  ungroup() %>%
  filter(importer %in% eu_countries) %>% # Changed to eu_countries
  ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
  geom_tile(color = "white", width = 0.99, height = 0.99) +
  scale_fill_manual(
    name = "Quartile",
    values = quartile_colors,
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )

# Function to extract legend from plot (from GeeksforGeeks)
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# Extract legend using the function
legend <- get_only_legend(plot_with_legend)

# Final combined plot with shared legend (4 plots in 1 column + legend at bottom)
plot1 <- grid.arrange(combined_plot, legend, nrow = 2, heights = c(20, 1))

ggsave(
  filename = "../images/eu_metrics_change.png",
  plot = plot1,
  width = 17, # Slightly wider for better text readability
  height = 17, # Slightly taller
  units = "in",
  dpi = 300,
  bg = "white"
)


# Save plots
for (metric in names(plots)) {
  ggsave(
    filename = paste0("../images/", "eu_", metric, "_changes1.png"),
    plot = plots[[metric]],
    width = 10,
    height = 8,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

# Function to create plot for  target countries ########

target_countries <- c("BRA", "ARG", "PRY")

quartile_colors <- c(
  "Q1 (Lowest)" = "#d6604d",
  "Q2" = "#f4a582",
  "Q3" = "#92c5de",
  "Q4 (Highest)" = "#4393c3"
)


create_metric_plot <- function(data, metric) {
  data %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(tariff_scenario = case_when(
      tariff_scenario == "compliance" ~ "Compliance",
      tariff_scenario == "non_compliance" ~ "Non-compliance",
      TRUE ~ tariff_scenario
    )) %>%
    group_by(tariff_scenario, product) %>%
    mutate(
      quartile = factor(
        ntile(.data[[metric]], 4),
        levels = 1:4,
        labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
      )
    ) %>%
    ungroup() %>%
    filter(importer %in% target_countries) %>%
    ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
    geom_tile(color = "white", width = 0.99, height = 0.99) +
    geom_text(aes(label = sprintf("%.2f%%", .data[[metric]])),
      size = 6,
      color = "black"
    ) +
    facet_wrap(~product) +
    scale_fill_manual(
      name = "Quartile",
      values = quartile_colors,
      drop = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13),
      axis.text.y = element_text(size = 13),
      legend.position = "none", # Remove individual legends
      panel.grid = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(x = "", y = "", title = "")
}

# Create individual plots without legends
plot_a <- create_metric_plot(welfare_all, "welfare")
plot_b <- create_metric_plot(welfare_all, "price_index")
plot_c <- create_metric_plot(welfare_all, "producer_price")
plot_d <- create_metric_plot(welfare_all, "TOT")

# Combine plots without legend using grid.arrange() in 1 column
combined_plot <- plot_grid(
  plot_a, plot_b, plot_c, plot_d,
  labels = c("a", "b", "c", "d"),
  label_size = 14,
  ncol = 1, align = "hv"
)

# Create one plot with legend to extract the legend
plot_with_legend <- welfare_all %>%
  mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
  mutate(tariff_scenario = case_when(
    tariff_scenario == "compliance" ~ "Compliance",
    tariff_scenario == "non_compliance" ~ "Non-compliance",
    TRUE ~ tariff_scenario
  )) %>%
  group_by(tariff_scenario, product) %>%
  mutate(
    quartile = factor(
      ntile(welfare, 4),
      levels = 1:4,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
  geom_tile(color = "white", width = 0.99, height = 0.99) +
  scale_fill_manual(
    name = "Quartile",
    values = quartile_colors,
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )

# Function to extract legend from plot (from GeeksforGeeks)
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# Extract legend using the function
legend <- get_only_legend(plot_with_legend)

# Final combined plot with shared legend (4 plots in 1 column + legend at bottom)
plot1 <- grid.arrange(combined_plot, legend, nrow = 2, heights = c(20, 1))

ggsave(
  filename = "C:\\Users\\Manoj\\OneDrive - Kansas State University\\GRA with Dr. Villoria\\Replicating a paper\\R file\\essay\\images\\target_changes1.png",
  plot = plot1,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Save plots
for (metric in names(plots)) {
  ggsave(
    filename = paste0("C:\\Users\\Manoj\\OneDrive - Kansas State University\\GRA with Dr. Villoria\\Replicating a paper\\R file\\essay\\images\\", metric, "_changes1.png"),
    plot = plots[[metric]],
    width = 11,
    height = 3,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}


# Function to create plot for  target countries ########

CCU <- c("USA", "CAN", "CHN", "VNM", "BOL", "IDN", "IND", "MEX", "THA", "PHL", "MOR", "PER", "ALG", "JPN", "EGY", "RUS", "UKR", "COL", "NOR", "CHE", "MKD")

quartile_colors <- c(
  "Q1 (Lowest)" = "#d6604d",
  "Q2" = "#f4a582",
  "Q3" = "#92c5de",
  "Q4 (Highest)" = "#4393c3"
)


create_metric_plot <- function(data, metric) {
  data %>%
    mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
    mutate(tariff_scenario = case_when(
      tariff_scenario == "compliance" ~ "Compliance",
      tariff_scenario == "non_compliance" ~ "Non-compliance",
      TRUE ~ tariff_scenario
    )) %>%
    group_by(tariff_scenario, product) %>%
    mutate(
      quartile = factor(
        ntile(.data[[metric]], 4),
        levels = 1:4,
        labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
      )
    ) %>%
    ungroup() %>%
    filter(importer %in% CCU) %>%
    ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
    geom_tile(color = "white", width = 0.99, height = 0.99) +
    geom_text(aes(label = sprintf("%.2f%%", .data[[metric]])),
      size = 6,
      color = "black"
    ) +
    facet_wrap(~product) +
    scale_fill_manual(
      name = "Quartile",
      values = quartile_colors,
      drop = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 13),
      legend.position = "none", # Remove individual legends
      panel.grid = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(x = "", y = "", title = "")
}

# Create individual plots without legends
plot_a <- create_metric_plot(welfare_all, "welfare")
plot_b <- create_metric_plot(welfare_all, "price_index")
plot_c <- create_metric_plot(welfare_all, "producer_price")
plot_d <- create_metric_plot(welfare_all, "TOT")

# Combine plots without legend using grid.arrange() in 1 column
combined_plot <- plot_grid(
  plot_a, plot_b, plot_c, plot_d,
  labels = c("a", "b", "c", "d"),
  label_size = 14,
  ncol = 2, align = "hv"
)

# Create one plot with legend to extract the legend
plot_with_legend <- welfare_all %>%
  mutate(country = countrycode::countrycode(importer, origin = "iso3c", destination = "country.name")) %>%
  mutate(tariff_scenario = case_when(
    tariff_scenario == "compliance" ~ "Compliance",
    tariff_scenario == "non_compliance" ~ "Non-compliance",
    TRUE ~ tariff_scenario
  )) %>%
  group_by(tariff_scenario, product) %>%
  mutate(
    quartile = factor(
      ntile(welfare, 4),
      levels = 1:4,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(x = tariff_scenario, y = country, fill = quartile)) +
  geom_tile(color = "white", width = 0.99, height = 0.99) +
  scale_fill_manual(
    name = "Quartile",
    values = quartile_colors,
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )

# Function to extract legend from plot (from GeeksforGeeks)
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# Extract legend using the function
legend <- get_only_legend(plot_with_legend)

# Final combined plot with shared legend (4 plots in 1 column + legend at bottom)
plot1 <- grid.arrange(combined_plot, legend, nrow = 2, heights = c(20, 1))

ggsave(
  filename = "C:\\Users\\Manoj\\OneDrive - Kansas State University\\GRA with Dr. Villoria\\Replicating a paper\\R file\\essay\\images\\CCU_changes1.png",
  plot = plot1,
  width = 18,
  height = 17,
  units = "in",
  dpi = 300,
  bg = "white"
)


############################################################################
### Increase in total exports from Brazil, Argentina, Paraguay, and USA ####
############################################################################
processed_soybean <- readRDS("Counterfactual trade flow/simulation_results_soybean.rds")
processed_soyoil <- readRDS("Counterfactual trade flow/simulation_results_soyoil.rds")
processed_soycake <- readRDS("Counterfactual trade flow/simulation_results_soycake.rds")

processed_soybean$product <- "Soybean"
processed_soyoil$product <- "Soybean oil"
processed_soycake$product <- "Soybean cake"

calculate_export_changes <- function(trade_matrix_data) {
  trade_matrix_data %>%
    filter(exporter != importer) %>%
    filter(exporter %in% c("BRA", "ARG", "PRY")) %>%
    group_by(exporter, tariff_scenario, product) %>%
    summarize(
      total_exports_bln = sum(tradehat_bln, na.rm = TRUE),
      total_exports_cfl = sum(tradehat_cfl, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      export_change_pct = ifelse(total_exports_bln == 0, NA,
        (total_exports_cfl - total_exports_bln) / total_exports_bln * 100
      ),
      export_change_value = round(total_exports_cfl - total_exports_bln, 2)
    ) %>%
    mutate(
      country_name = case_when(
        exporter == "BRA" ~ "Brazil",
        exporter == "ARG" ~ "Argentina",
        exporter == "PRY" ~ "Paraguay",
        TRUE ~ exporter
      ),
      # Fix tariff scenario names
      tariff_scenario = case_when(
        tariff_scenario == "compliance" ~ "Compliance",
        tariff_scenario == "non_compliance" ~ "Non-compliance",
        TRUE ~ tariff_scenario
      )
    )
}

export_changes_soybean <- calculate_export_changes(processed_soybean)
export_changes_soyoil <- calculate_export_changes(processed_soyoil)
export_changes_soycake <- calculate_export_changes(processed_soycake)

all_export_changes <- bind_rows(
  export_changes_soybean,
  export_changes_soyoil,
  export_changes_soycake
)

export_summary <- all_export_changes %>%
  select(
    country_name, product, tariff_scenario,
    total_exports_bln, total_exports_cfl,
    export_change_pct, export_change_value
  ) %>%
  arrange(country_name, product, tariff_scenario)


plot1 <- ggplot(all_export_changes, aes(
  x = country_name, y = export_change_pct,
  fill = tariff_scenario
)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(
    aes(label = ifelse(is.na(export_change_pct), "",
      paste0(round(export_change_pct, 1), "%")
    )),
    position = position_dodge(width = 0.7),
    vjust = ifelse(all_export_changes$export_change_pct >= 0, -0.3, 1.2),
    size = 3.5,
    fontface = "bold"
  ) +
  facet_wrap(~product) +
  labs(
    x = "",
    y = "Export Change (%)",
    fill = "Scenarios"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

ggsave(
  filename = "C:\\Users\\Manoj\\OneDrive - Kansas State University\\GRA with Dr. Villoria\\Replicating a paper\\R file\\essay\\images\\export_change_plot_new1.png",
  plot = plot1,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)
