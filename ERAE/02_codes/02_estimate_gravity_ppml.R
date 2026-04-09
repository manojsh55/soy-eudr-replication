library(pacman)
p_load(data.table, tidyverse, writexl, fixest, readxl, tradepolicy, ggplot2, alpaca, modelsummary)

# Country selection
iso_codes <- c(
  "ARE", "ARG", "AUS", "AUT", "BEL", "BGR", "BOL", "BRA", "CAN", "CHE", "CHL", "CHN",
  "COL", "CRI", "CYP", "CZE", "DEU", "DNK", "DZA", "EGY", "ESP", "EST", "FIN", "FRA",
  "GAB", "GBR", "GRC", "HKG", "HRV", "HUN", "IDN", "IND", "IRL", "IRN", "ISL", "ITA",
  "JOR", "JPN", "KEN", "KHM", "KOR", "LAO", "LBN", "LKA", "LTU", "LUX", "LVA", "MAR",
  "MDG", "MEX", "MKD", "MLT", "MMR", "MNE", "MOZ", "MUS", "MYS", "NIC", "NLD", "NOR",
  "NZL", "OMN", "PAN", "PER", "PHL", "POL", "PRT", "PRY", "ROU", "RUS", "SAU", "SGP",
  "SLV", "SRB", "SVK", "SVN", "SWE", "THA", "TTO", "TUR", "TWN", "UKR", "URY", "USA",
  "VEN", "VNM", "ZAF", "ZWE", "KAZ", "ECU"
)

# Load and prepare data
ch2_application2 <- readRDS("../data/soy_gravity4.rds") %>%
  filter(year %in% 2007:2022) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v) %>%
  filter(exporter %in% iso_codes) %>%
  filter(importer %in% iso_codes) %>%
  group_by(exporter, year) %>%
  mutate(
    export1 = sum(trade1[exporter != importer]),
    export2 = sum(trade2[exporter != importer]),
    export3 = sum(trade3[exporter != importer])
  ) %>%
  group_by(importer, year) %>%
  mutate(
    import1 = sum(trade1[exporter != importer]),
    import2 = sum(trade2[exporter != importer]),
    import3 = sum(trade3[exporter != importer])
  ) %>%
  ungroup() %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    pair_id  = paste0(exporter, importer),
    log_tariff1 = log(tariff1 + 1),
    log_tariff2 = log(tariff2 + 1),
    log_tariff3 = log(tariff3 + 1)
  )

###############################################################
# Table 2: PPML estimates of tariff effects on soy products
###############################################################

model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017), cluster = "pair_id"
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017), cluster = "pair_id"
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017), cluster = "pair_id"
)

var_labels <- c(
  "log_tariff1" = "Log(1 + Tariff)",
  "log_tariff2" = "Log(1 + Tariff)",
  "log_tariff3" = "Log(1 + Tariff)"
)

gof_map <- data.frame(
  raw   = c("nobs", "r.squared", "FE: exp_year", "FE: imp_year", "FE: pair_id"),
  clean = c("Observations", "R-squared", "Exporter-Year FE", "Importer-Year FE", "Pair FE"),
  fmt   = c(0, 3, 0, 0, 0)
)

table2 <- modelsummary(
  list(
    "Soybean"      = model1,
    "Soybean Oil"  = model2,
    "Soybean Cake" = model3
  ),
  coef_map  = var_labels,
  stars     = TRUE,
  gof_map   = gof_map,
  statistic = "({std.error})",
  output    = "flextable"
)

saveRDS(table2, "../data/ppml_results.rds")

###############################################################
# Figure A3.1: Armington elasticities
###############################################################

coef_data <- data.frame(
  model = c("Soybean", "Soybean oil", "Soybean cake"),
  coefficient = c(
    coef(model1)["log_tariff1"],
    coef(model2)["log_tariff2"],
    coef(model3)["log_tariff3"]
  ),
  std_error = c(
    se(model1)["log_tariff1"],
    se(model2)["log_tariff2"],
    se(model3)["log_tariff3"]
  )
) %>%
  mutate(
    t_stat   = coefficient / std_error,
    p_value  = 2 * (1 - pnorm(abs(t_stat))),
    stars    = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    eos         = 1 - coefficient,
    ci_lower    = 1 - (coefficient + 1.96 * std_error),
    ci_upper    = 1 - (coefficient - 1.96 * std_error),
    coef_label  = paste0(round(eos, 3), stars),
    bounds_label = paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
  )

fig_a3 <- ggplot(coef_data, aes(y = model, x = eos)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2, color = "blue") +
  geom_text(aes(label = coef_label),  vjust = -0.8, size = 4, fontface = "bold") +
  geom_text(aes(label = bounds_label), vjust = 1.2, size = 5, color = "gray50") +
  labs(title = "", subtitle = "", y = "", x = "Armington elasticities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("../images/armington_elasticity.png",
  plot = fig_a3, width = 5, height = 3, units = "in", dpi = 300, bg = "white"
)
