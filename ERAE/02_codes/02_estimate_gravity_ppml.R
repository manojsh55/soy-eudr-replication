library(pacman)
p_load(data.table, tidyverse, writexl, zip, fixest, readxl, tradepolicy, ggplot2, alpaca, modelsummary)

# setwd("../data") # Commented out, using relative paths directly

select_1 <- ch2_application2 %>% filter(export1 == 0 & import1 == 0)
select_2 <- ch2_application2 %>% filter(export2 == 0 & import2 == 0)
select_3 <- ch2_application2 %>% filter(export3 == 0 & import3 == 0)

### create additional variables

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

cake <- c("ARE", "ISL", "KEN", "MUS", "MNE", "OMN", "DZA")

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
  ungroup()


ch2_application2 <- ch2_application2 %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    pair_id = paste0(exporter, importer)
  ) %>%
  mutate(
    log_tariff3 = log(tariff3 + 1),
    log_tariff1 = log(tariff1 + 1),
    log_tariff2 = log(tariff2 + 1),
    log_tariff4 = log(tariff4 + 1)
  ) %>%
  mutate(log_ret = ifelse(year %in% c(2018, 2019), 1, 0))

model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% c(2007:2017)), cluster = "pair_id"
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% c(2007:2017)), cluster = "pair_id"
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% c(2007:2017)), cluster = "pair_id"
)

etable(model1, model2, model3)

var_labels <- c(
  "log_tariff1" = "Log(1 + Tariff)",
  "log_tariff2" = "Log(1 + Tariff)",
  "log_tariff3" = "Log(1 + Tariff)"
)

# Custom model names
model_names <- c(
  "Soybean" = "model1",
  "Soybean Oil" = "model2",
  "Soybean Cake" = "model3"
)
gof_map <- data.frame(
  raw = c("nobs", "r.squared", "FE: exp_year", "FE: imp_year", "FE: pair_id"),
  clean = c("Observations", "R-squared", "Exporter-Year FE", "Importer-Year FE", "Pair FE"),
  fmt = c(0, 3, 0, 0, 0)
)

# Create the table
a <- modelsummary(
  list(
    "Soybean" = model1,
    "Soybean Oil" = model2,
    "Soybean Cake" = model3
  ),
  coef_map = var_labels,
  stars = TRUE,
  gof_map = gof_map,
  statistic = "({std.error})",
  output = "flextable"
)

saveRDS(a, "../data/ppml_results.rds")


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
    t_stat = coefficient / std_error,
    p_value = 2 * (1 - pnorm(abs(t_stat))),
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    eos = 1 - coefficient,
    # Fixed confidence intervals for eos = 1 - coefficient
    ci_lower = 1 - (coefficient + 1.96 * std_error),
    ci_upper = 1 - (coefficient - 1.96 * std_error),
    coef_label = paste0(round(eos, 3), stars),
    # Add bounds labels
    bounds_label = paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
  )

a <- ggplot(coef_data, aes(y = model, x = eos)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper),
    width = 0.2, color = "blue"
  ) +
  geom_text(aes(label = coef_label), vjust = -0.8, size = 4, fontface = "bold") +
  geom_text(aes(label = bounds_label), vjust = 1.2, size = 5, color = "gray50") + # Add bounds
  labs(
    title = "",
    subtitle = "",
    y = "",
    x = "Armington elasticities"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


ggsave("../images/armington_elasticity.png",
  plot = a, width = 5, height = 3, units = "in", dpi = 300, bg = "white"
)


model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes)
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes)
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes)
)

model4 <- fixest::fepois(trade ~ log_tariff4_a | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes)
)

model5 <- fixest::fepois(trade ~ log_tariff4_w | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes)
)

etable(model1, model2, model3, model4, model5)


model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017)
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017)
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017)
)

model4 <- fixest::fepois(trade ~ log_tariff4 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, year %in% 2007:2017)
)


etable(model1, model2, model3, model4)


model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

model4 <- fixest::fepois(trade ~ log_tariff4 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

etable(model1, model2, model3, model4)

model1 <- fixest::fepois(trade1 ~ log_tariff1 + log_ret | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2022), cluster = ("pair_id")
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2022), cluster = ("pair_id")
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2022), cluster = ("pair_id")
)

model4 <- fixest::fepois(trade ~ log_tariff4 + log_ret | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2022), cluster = ("pair_id")
)

etable(model1, model2, model3, model4)

etable(model1, model2, model3, model4)


model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

model4 <- fixest::fepois(trade ~ log_tariff4 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017), cluster = ("pair_id")
)

etable(model1, model2, model3, model4)

model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021), cluster = ("pair_id")
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021), cluster = ("pair_id")
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021), cluster = ("pair_id")
)

model4 <- fixest::fepois(trade ~ log_tariff4 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021), cluster = ("pair_id")
)

etable(model1, model2, model3, model4)

model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017, exporter != importer), cluster = ("pair_id")
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017, exporter != importer), cluster = ("pair_id")
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017, exporter != importer), cluster = ("pair_id")
)

model4 <- fixest::fepois(trade ~ log_tariff4 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2017, exporter != importer), cluster = ("pair_id")
)

etable(model1, model2, model3, model4)

model1 <- fixest::fepois(trade1 ~ log_tariff1 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021, exporter != importer), cluster = ("pair_id")
)

model2 <- fixest::fepois(trade2 ~ log_tariff2 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021, exporter != importer), cluster = ("pair_id")
)

model3 <- fixest::fepois(trade3 ~ log_tariff3 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021, exporter != importer), cluster = ("pair_id")
)

model4 <- fixest::fepois(trade ~ log_tariff4 | exp_year + imp_year + pair_id,
  data = filter(ch2_application2, exporter %in% iso_codes & importer %in% iso_codes, year %in% 2007:2021, exporter != importer), cluster = ("pair_id")
)

etable(model1, model2, model3, model4)


####################################################################
################################## Import BAP area #################
####################################################################

bap_area <- read.csv("../data/bap_area_harvested.csv") %>%
  mutate(exporter = countrycode::countrycode(Area, origin = "country.name", destination = "iso3c")) %>%
  select(exporter, year = Year, area_harvested = Value) %>%
  pivot_wider(names_from = exporter, values_from = area_harvested)

## 1.1 Measuring effect of exporters' export on EU imports #############

data_model2 <- readRDS("../data/soy_gravity4.rds") %>%
  filter(year %in% 2007:2022) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v, trade = soy) %>%
  filter(exporter %in% iso_codes) %>%
  filter(importer %in% iso_codes) %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    pair_id = paste0(exporter, importer)
  ) %>%
  left_join(bap_area, by = "year") %>%
  mutate(
    eu_import_BRA = I(exporter != "BRA" & !exporter %in% eu_codes & importer %in% eu_codes) * log(BRA),
    eu_import_ARG = I(exporter != "ARG" & !exporter %in% eu_codes & importer %in% eu_codes) * log(ARG),
    eu_import_PRY = I(exporter != "PRY" & !exporter %in% eu_codes & importer %in% eu_codes) * log(PRY),
    eu_import_BRA1 = I(exporter == "BRA" & importer %in% eu_codes) * log(BRA),
    eu_import_ARG1 = I(exporter == "ARG" & importer %in% eu_codes) * log(ARG),
    eu_import_PRY1 = I(exporter == "PRY" & importer %in% eu_codes) * log(PRY),
    CHN_import_BRA1 = I(exporter == "BRA" & importer == "CHN") * log(BRA),
    CHN_import_ARG1 = I(exporter == "ARG" & importer == "CHN") * log(ARG),
    CHN_import_PRY1 = I(exporter == "PRY" & importer == "CHN") * log(PRY),
    intra_BRA = I(exporter == "BRA" & importer == "BRA") * log(BRA),
    intra_ARG = I(exporter == "ARG" & importer == "ARG") * log(ARG),
    intra_PRY = I(exporter == "PRY" & importer == "PRY") * log(PRY)
  ) %>%
  mutate(
    log_tariff3 = log(tariff3 + 1),
    log_tariff1 = log(tariff1 + 1),
    log_tariff2 = log(tariff2 + 1),
    log_tariff4 = log(tariff4 + 1)
  )

model2.1 <- fepois(
  trade1 ~ eu_import_BRA1 + eu_import_ARG1 + eu_import_PRY1 + CHN_import_BRA1 + CHN_import_ARG1 + CHN_import_PRY1 + eu_import_BRA + eu_import_ARG + eu_import_PRY + intra_BRA + intra_ARG + intra_PRY |
    pair_id + exp_year + imp_year,
  data = filter(data_model2, year %in% 2007:2017), cluster = "pair_id"
)
model2.2 <- fepois(
  trade2 ~ eu_import_BRA1 + eu_import_ARG1 + eu_import_PRY1 + CHN_import_BRA1 + CHN_import_ARG1 + CHN_import_PRY1 + eu_import_BRA + eu_import_ARG + eu_import_PRY + intra_BRA + intra_ARG + intra_PRY |
    pair_id + exp_year + imp_year,
  data = filter(data_model2, year %in% 2007:2017), cluster = "pair_id"
)
model2.3 <- fepois(
  trade3 ~ eu_import_BRA1 + eu_import_ARG1 + eu_import_PRY1 + CHN_import_BRA1 + CHN_import_ARG1 + CHN_import_PRY1 + eu_import_BRA + eu_import_ARG + eu_import_PRY + intra_BRA + intra_ARG + intra_PRY |
    pair_id + exp_year + imp_year,
  data = filter(data_model2, year %in% 2007:2017), cluster = "pair_id"
)
model2.4 <- fepois(
  trade ~ eu_import_BRA1 + eu_import_ARG1 + eu_import_PRY1 + CHN_import_BRA1 + CHN_import_ARG1 + CHN_import_PRY1 + eu_import_BRA + eu_import_ARG + eu_import_PRY + intra_BRA + intra_ARG + intra_PRY |
    pair_id + exp_year + imp_year,
  data = filter(data_model2, year %in% 2007:2017), cluster = "pair_id"
)

etable(model2.1, model2.2, model2.3, model2.4)

var_labels <- c(
  "eu_import_BRA1" = "I(BRA × EU) × log(BRA area)",
  "eu_import_ARG1" = "I(ARG × EU) × log(ARG area)",
  "eu_import_PRY1" = "I(PRY × EU) × log(PRY area)",
  "CHN_import_BRA1" = "I(BRA × CHN) × log(BRA area)",
  "CHN_import_ARG1" = "I(ARG × CHN) × log(ARG area)",
  "CHN_import_PRY1" = "I(PRY × CHN) × log(PRY area)",
  "intra_BRA" = "I(BRA × BRA) × log(BRA area)",
  "intra_ARG" = "I(ARG × ARG) × log(ARG area)",
  "intra_PRY" = "I(PRY × PRY) × log(PRY area)",
  "eu_import_BRA" = "I(All but BRA and EU × EU) × log(BRA area)",
  "eu_import_ARG" = "I(All but ARG and EU × EU) × log(ARG area)",
  "eu_import_PRY" = "I(All but PRY and EU × EU) × log(PRY area)"
)

# Custom model names for your 8 models
model_names <- c(
  "Soybean" = "model2.1",
  "Soybean Oil" = "model2.2",
  "Soybean Cake" = "model2.3",
  "Soy Total" = "model2.4"
)

# Goodness-of-fit mapping
gof_map <- data.frame(
  raw = c("nobs", "r.squared", "FE: exp_year", "FE: imp_year", "FE: pair_id"),
  clean = c("Observations", "R-squared", "Exporter-Year FE", "Importer-Year FE", "Pair FE"),
  fmt = c(0, 3, 0, 0, 0)
)

# Create the summary table
a <- modelsummary(
  list(
    "Soybean" = model2.1,
    "Soybean Oil" = model2.2,
    "Soybean Cake" = model2.3,
    "Soy Total" = model2.4
  ),
  coef_map = var_labels,
  stars = TRUE,
  gof_map = gof_map,
  statistic = "({std.error})",
  output = "flextable"
)

saveRDS(a, "../data/ppml_results_land_expansion.rds")


## 1.2 Measuring effect of EU consumption on EU consumption #############

eu_codes <- c(
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

eu_consumption <- readRDS("../data/soy_gravity4.rds") %>%
  filter(year %in% 2007:2022) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v, trade = soy) %>%
  filter(exporter %in% iso_codes) %>%
  filter(importer %in% iso_codes) %>%
  mutate(
    exporter = ifelse(exporter %in% eu_codes, "EU", "non-EU"),
    importer = ifelse(importer %in% eu_codes, "EU", "non-EU")
  ) %>%
  group_by(exporter, importer, year) %>%
  summarize(
    trade1 = sum(trade1, na.rm = TRUE),
    trade2 = sum(trade2, na.rm = TRUE),
    trade3 = sum(trade3, na.rm = TRUE),
    trade  = sum(trade, na.rm = TRUE)
  ) %>%
  group_by(year) %>%
  summarize(
    # Imports: from non-EU to EU
    import1 = sum(trade1[importer == "EU" & exporter == "non-EU"], na.rm = TRUE),
    import2 = sum(trade2[importer == "EU" & exporter == "non-EU"], na.rm = TRUE),
    import3 = sum(trade3[importer == "EU" & exporter == "non-EU"], na.rm = TRUE),
    import = sum(trade[importer == "EU" & exporter == "non-EU"], na.rm = TRUE),

    # Exports: from EU to non-EU
    export1 = sum(trade1[exporter == "EU" & importer == "non-EU"], na.rm = TRUE),
    export2 = sum(trade2[exporter == "EU" & importer == "non-EU"], na.rm = TRUE),
    export3 = sum(trade3[exporter == "EU" & importer == "non-EU"], na.rm = TRUE),
    export = sum(trade[exporter == "EU" & importer == "non-EU"], na.rm = TRUE),

    # Domestic sales: intra-EU trade (optional, or use production if available)
    domesticsale1 = sum(trade1[importer == "EU" & exporter == "EU"], na.rm = TRUE),
    domesticsale2 = sum(trade2[importer == "EU" & exporter == "EU"], na.rm = TRUE),
    domesticsale3 = sum(trade3[importer == "EU" & exporter == "EU"], na.rm = TRUE),
    domesticsale = sum(trade[importer == "EU" & exporter == "EU"], na.rm = TRUE)
  ) %>%
  mutate(
    # Apparent consumption
    consumption1 = import1 + domesticsale1 - export1,
    consumption2 = import2 + domesticsale2 - export2,
    consumption3 = import3 + domesticsale3 - export3,
    consumption  = import + domesticsale - export
  )

chinese_consumption <- readRDS("../data/soy_gravity4.rds") %>%
  filter(year %in% 2007:2022) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v, trade = soy) %>%
  filter(exporter %in% iso_codes) %>%
  filter(importer %in% iso_codes) %>%
  group_by(year) %>%
  summarize(
    import1 = sum(trade1[importer == "CHN" & exporter != "CHN"], na.rm = TRUE),
    import2 = sum(trade2[importer == "CHN" & exporter != "CHN"], na.rm = TRUE),
    import3 = sum(trade3[importer == "CHN" & exporter != "CHN"], na.rm = TRUE),
    import = sum(trade[importer == "CHN" & exporter != "CHN"], na.rm = TRUE),

    # Exports: from CHN to non-CHN
    export1 = sum(trade1[exporter == "CHN" & importer == "!CHN"], na.rm = TRUE),
    export2 = sum(trade2[exporter == "CHN" & importer == "!CHN"], na.rm = TRUE),
    export3 = sum(trade3[exporter == "CHN" & importer == "!CHN"], na.rm = TRUE),
    export = sum(trade[exporter == "CHN" & importer == "!CHN"], na.rm = TRUE),

    # Domestic sales: intra-China trade (optional, or use production if available)
    domesticsale1 = sum(trade1[importer == "CHN" & exporter == "CHN"], na.rm = TRUE),
    domesticsale2 = sum(trade2[importer == "CHN" & exporter == "CHN"], na.rm = TRUE),
    domesticsale3 = sum(trade3[importer == "CHN" & exporter == "CHN"], na.rm = TRUE),
    domesticsale = sum(trade[importer == "CHN" & exporter == "CHN"], na.rm = TRUE)
  ) %>%
  mutate(
    # Apparent consumption
    chn_consumption1 = import1 + domesticsale1 - export1,
    chn_consumption2 = import2 + domesticsale2 - export2,
    chn_consumption3 = import3 + domesticsale3 - export3,
    chn_consumption = import + domesticsale - export,
    ch_check = chn_consumption1 + chn_consumption2 + chn_consumption
  )

data_model3 <- readRDS("../data/soy_gravity4.rds") %>%
  filter(year %in% 2007:2022) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v, trade = soy) %>%
  # filter(exporter %in% iso_codes) %>%
  # filter(importer %in% iso_codes) %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    pair_id = paste0(exporter, importer)
  ) %>%
  left_join(eu_consumption, by = "year") %>%
  left_join(chinese_consumption, by = "year") %>%
  mutate(
    brazil_export_eu1 = I(exporter == "BRA" & importer %in% eu_codes) * log(consumption1),
    brazil_export_eu2 = I(exporter == "BRA" & importer %in% eu_codes) * log(consumption2),
    brazil_export_eu3 = I(exporter == "BRA" & importer %in% eu_codes) * log(consumption3),
    brazil_export_eu = I(exporter == "BRA" & importer %in% eu_codes) * log(consumption),
    arg_export_eu1 = I(exporter == "ARG" & importer %in% eu_codes) * log(consumption1),
    arg_export_eu2 = I(exporter == "ARG" & importer %in% eu_codes) * log(consumption2),
    arg_export_eu3 = I(exporter == "ARG" & importer %in% eu_codes) * log(consumption3),
    arg_export_eu = I(exporter == "ARG" & importer %in% eu_codes) * log(consumption),
    pry_export_eu1 = I(exporter == "PRY" & importer %in% eu_codes)**log(consumption1),
    pry_export_eu2 = I(exporter == "PRY" & importer %in% eu_codes) * log(consumption2),
    pry_export_eu3 = I(exporter == "PRY" & importer %in% eu_codes) * log(consumption3),
    pry_export_eu = I(exporter == "PRY" & importer %in% eu_codes) * log(consumption),
    usa_export_eu1 = I(exporter == "USA" & importer %in% eu_codes) * log(consumption1),
    usa_export_eu2 = I(exporter == "USA" & importer %in% eu_codes) * log(consumption2),
    usa_export_eu3 = I(exporter == "USA" & importer %in% eu_codes) * log(consumption3),
    usa_export_eu = I(exporter == "USA" & importer %in% eu_codes) * log(consumption),
    can_export_eu1 = I(exporter == "CAN" & importer %in% eu_codes)**log(consumption1),
    can_export_eu2 = I(exporter == "CAN" & importer %in% eu_codes) * log(consumption2),
    can_export_eu3 = I(exporter == "CAN" & importer %in% eu_codes) * log(consumption3),
    can_export_eu = I(exporter == "CAN" & importer %in% eu_codes) * log(consumption)
  ) %>%
  mutate(
    brazil_export1 = I(exporter == "BRA" & importer != "BRA") * log(consumption1),
    brazil_export2 = I(exporter == "BRA" & importer != "BRA") * log(consumption2),
    brazil_export3 = I(exporter == "BRA" & importer != "BRA") * log(consumption3),
    brazil_export = I(exporter == "BRA" & importer != "BRA") * log(consumption),
    arg_export1 = I(exporter == "ARG" & importer != "ARG") * log(consumption1),
    arg_export2 = I(exporter == "ARG" & importer != "ARG") * log(consumption2),
    arg_export3 = I(exporter == "ARG" & importer != "ARG") * log(consumption3),
    arg_export = I(exporter == "ARG" & importer != "ARG") * log(consumption),
    pry_export1 = I(exporter == "PRY" & importer != "PRY") * log(consumption1),
    pry_export2 = I(exporter == "PRY" & importer != "PRY") * log(consumption2),
    pry_export3 = I(exporter == "PRY" & importer != "PRY") * log(consumption3),
    pry_export = I(exporter == "PRY" & importer != "PRY") * log(consumption),
    usa_export1 = I(exporter == "USA" & importer != "USA") * log(consumption1),
    usa_export2 = I(exporter == "USA" & importer != "USA") * log(consumption2),
    usa_export3 = I(exporter == "USA" & importer != "USA") * log(consumption3),
    usa_export = I(exporter == "USA" & importer != "USA") * log(consumption),
    can_export1 = I(exporter == "CAN" & importer != "CAN") * log(consumption1),
    can_export2 = I(exporter == "CAN" & importer != "CAN") * log(consumption2),
    can_export3 = I(exporter == "CAN" & importer != "CAN") * log(consumption3),
    can_export = I(exporter == "CAN" & importer != "CAN") * log(consumption)
  ) %>%
  mutate(
    brazil_export_row1 = I(exporter == "BRA" & importer != "BRA" & !importer %in% eu_codes) * log(consumption1),
    brazil_export_row2 = I(exporter == "BRA" & importer != "BRA" & !importer %in% eu_codes) * log(consumption2),
    brazil_export_row3 = I(exporter == "BRA" & importer != "BRA" & !importer %in% eu_codes) * log(consumption3),
    brazil_export_row = I(exporter == "BRA" & importer != "BRA" & !importer %in% eu_codes) * log(consumption),
    arg_export_row1 = I(exporter == "ARG" & importer != "ARG" & !importer %in% eu_codes) * log(consumption1),
    arg_export_row2 = I(exporter == "ARG" & importer != "ARG" & !importer %in% eu_codes) * log(consumption2),
    arg_export_row3 = I(exporter == "ARG" & importer != "ARG" & !importer %in% eu_codes) * log(consumption3),
    arg_export_row = I(exporter == "ARG" & importer != "ARG" & !importer %in% eu_codes) * log(consumption),
    pry_export_row1 = I(exporter == "PRY" & importer != "PRY" & !importer %in% eu_codes) * log(consumption1),
    pry_export_row2 = I(exporter == "PRY" & importer != "PRY" & !importer %in% eu_codes) * log(consumption2),
    pry_export_row3 = I(exporter == "PRY" & importer != "PRY" & !importer %in% eu_codes) * log(consumption3),
    pry_export_row = I(exporter == "PRY" & importer != "PRY" & !importer %in% eu_codes) * log(consumption),
    usa_export_row1 = I(exporter == "USA" & importer != "USA" & !importer %in% eu_codes) * log(consumption1),
    usa_export_row2 = I(exporter == "USA" & importer != "USA" & !importer %in% eu_codes) * log(consumption2),
    usa_export_row3 = I(exporter == "USA" & importer != "USA" & !importer %in% eu_codes) * log(consumption3),
    usa_export_row = I(exporter == "USA" & importer != "USA" & !importer %in% eu_codes) * log(consumption),
    can_export_row1 = I(exporter == "CAN" & importer != "CAN" & !importer %in% eu_codes) * log(consumption1),
    can_export_row2 = I(exporter == "CAN" & importer != "CAN" & !importer %in% eu_codes) * log(consumption2),
    can_export_row3 = I(exporter == "CAN" & importer != "CAN" & !importer %in% eu_codes) * log(consumption3),
    can_export_row = I(exporter == "CAN" & importer != "CAN" & !importer %in% eu_codes) * log(consumption)
  ) %>%
  mutate(
    row_BRA_export_eu1 = I(exporter != "BRA" & importer %in% eu_codes) * log(consumption1),
    row_BRA_export_eu2 = I(exporter != "BRA" & importer %in% eu_codes) * log(consumption2),
    row_BRA_export_eu3 = I(exporter != "BRA" & importer %in% eu_codes) * log(consumption3),
    row_BRA_export_eu  = I(exporter != "BRA" & importer %in% eu_codes) * log(consumption),
    row_ARG_export_eu1 = I(exporter != "ARG" & importer %in% eu_codes) * log(consumption1),
    row_ARG_export_eu2 = I(exporter != "ARG" & importer %in% eu_codes) * log(consumption2),
    row_ARG_export_eu3 = I(exporter != "ARG" & importer %in% eu_codes) * log(consumption3),
    row_ARG_export_eu  = I(exporter != "ARG" & importer %in% eu_codes) * log(consumption),
    row_PRY_export_eu1 = I(exporter != "PRY" & importer %in% eu_codes) * log(consumption1),
    row_PRY_export_eu2 = I(exporter != "PRY" & importer %in% eu_codes) * log(consumption2),
    row_PRY_export_eu3 = I(exporter != "PRY" & importer %in% eu_codes) * log(consumption3),
    row_PRY_export_eu  = I(exporter != "PRY" & importer %in% eu_codes) * log(consumption),
    row_USA_export_eu1 = I(exporter != "USA" & importer %in% eu_codes) * log(consumption1),
    row_USA_export_eu2 = I(exporter != "USA" & importer %in% eu_codes) * log(consumption2),
    row_USA_export_eu3 = I(exporter != "USA" & importer %in% eu_codes) * log(consumption3),
    row_USA_export_eu  = I(exporter != "USA" & importer %in% eu_codes) * log(consumption),
    row_CAN_export_eu1 = I(exporter != "CAN" & importer %in% eu_codes) * log(consumption1),
    row_CAN_export_eu2 = I(exporter != "CAN" & importer %in% eu_codes) * log(consumption2),
    row_CAN_export_eu3 = I(exporter != "CAN" & importer %in% eu_codes) * log(consumption3),
    row_CAN_export_eu  = I(exporter != "CAN" & importer %in% eu_codes) * log(consumption)
  ) %>%
  mutate(
    brazil_export_chn1 = I(exporter == "BRA" & importer == "CHN") * log(chn_consumption1),
    brazil_export_chn2 = I(exporter == "BRA" & importer == "CHN") * log(chn_consumption2),
    brazil_export_chn3 = I(exporter == "BRA" & importer == "CHN") * log(chn_consumption3),
    brazil_export_chn = I(exporter == "BRA" & importer == "CHN") * log(chn_consumption),
    arg_export_chn1 = I(exporter == "ARG" & importer == "CHN") * log(chn_consumption1),
    arg_export_chn2 = I(exporter == "ARG" & importer == "CHN") * log(chn_consumption2),
    arg_export_chn3 = I(exporter == "ARG" & importer == "CHN") * log(chn_consumption3),
    arg_export_chn = I(exporter == "ARG" & importer == "CHN") * log(chn_consumption),
    pry_export_chn1 = I(exporter == "PRY" & importer == "CHN") * log(chn_consumption1),
    pry_export_chn2 = I(exporter == "PRY" & importer == "CHN") * log(chn_consumption2),
    pry_export_chn3 = I(exporter == "PRY" & importer == "CHN") * log(chn_consumption3),
    pry_export_chn = I(exporter == "PRY" & importer == "CHN") * log(chn_consumption),
    usa_export_chn1 = I(exporter == "USA" & importer == "CHN") * log(chn_consumption1),
    usa_export_chn2 = I(exporter == "USA" & importer == "CHN") * log(chn_consumption2),
    usa_export_chn3 = I(exporter == "USA" & importer == "CHN") * log(chn_consumption3),
    usa_export_chn = I(exporter == "USA" & importer == "CHN") * log(chn_consumption),
    can_export_chn1 = I(exporter == "CAN" & importer == "CHN") * log(chn_consumption1),
    can_export_chn2 = I(exporter == "CAN" & importer == "CHN") * log(chn_consumption2),
    can_export_chn3 = I(exporter == "CAN" & importer == "CHN") * log(chn_consumption3),
    can_export_chn = I(exporter == "CAN" & importer == "CHN") * log(chn_consumption)
  ) %>%
  mutate(
    bap_export_eu1 = I(exporter %in% c("PRY", "BRA", "ARG") & importer %in% eu_codes) * log(consumption1),
    bap_export_eu2 = I(exporter %in% c("PRY", "BRA", "ARG") & importer %in% eu_codes) * log(consumption2),
    bap_export_eu3 = I(exporter %in% c("PRY", "BRA", "ARG") & importer %in% eu_codes) * log(consumption3),
    bap_export_eu = I(exporter %in% c("PRY", "BRA", "ARG") & importer %in% eu_codes) * log(consumption),
    bap_export_chn1 = I(exporter %in% c("PRY", "BRA", "ARG") & importer == "CHN") * log(chn_consumption1),
    bap_export_chn2 = I(exporter %in% c("PRY", "BRA", "ARG") & importer == "CHN") * log(chn_consumption2),
    bap_export_chn3 = I(exporter %in% c("PRY", "BRA", "ARG") & importer == "CHN") * log(chn_consumption3),
    bap_export_chn = I(exporter %in% c("PRY", "BRA", "ARG") & importer == "CHN") * log(chn_consumption)
  ) %>%
  mutate(
    log_tariff3 = log(tariff3 + 1),
    log_tariff1 = log(tariff1 + 1),
    log_tariff2 = log(tariff2 + 1),
    log_tariff4 = log(tariff4 + 1)
  )

model3.1 <- fepois(
  trade1 ~ bap_export_chn1 + bap_export_eu1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model3.2 <- fepois(
  trade2 ~ bap_export_chn2 + bap_export_eu2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model3.3 <- fepois(
  trade3 ~ bap_export_chn3 + bap_export_eu3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model3.4 <- fepois(
  trade ~ bap_export_chn + bap_export_eu |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

etable(model3.1, model3.2, model3.3, model3.4)

model3.1 <- fepois(
  trade1 ~ brazil_export_eu1 + arg_export_eu1 + pry_export_eu1 + usa_export_eu1 + can_export_eu1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model3.2 <- fepois(
  trade2 ~ brazil_export_eu2 + arg_export_eu2 + pry_export_eu2 + usa_export_eu2 + can_export_eu2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model3.3 <- fepois(
  trade3 ~ brazil_export_eu3 + arg_export_eu3 + pry_export_eu3 + usa_export_eu3 + can_export_eu3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model3.4 <- fepois(
  trade ~ brazil_export_eu + arg_export_eu + pry_export_eu + usa_export_eu + can_export_eu |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

etable(model3.1, model3.2, model3.3, model3.4)

model4.1 <- fepois(
  trade1 ~ brazil_export1 + arg_export1 + pry_export1 + usa_export1 + can_export1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017), cluster = "pair_id"
)

model4.2 <- fepois(
  trade2 ~ brazil_export2 + arg_export2 + pry_export2 + usa_export2 + can_export2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017), cluster = "pair_id"
)
model4.3 <- fepois(
  trade3 ~ brazil_export3 + arg_export3 + pry_export3 + usa_export3 + can_export3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017), cluster = "pair_id"
)
model4.4 <- fepois(
  trade ~ brazil_export + arg_export + pry_export + usa_export + can_export |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017), cluster = "pair_id"
)

etable(model3.1, model3.2, model3.3, model3.4, model4.1, model4.2, model4.3, model4.4)


model5.1 <- fepois(
  trade1 ~ brazil_export_eu1 + arg_export_eu1 + pry_export_eu1 + usa_export_eu1 + can_export_eu1 + brazil_export1 + arg_export1 + pry_export1 + usa_export1 + can_export1 + log_tariff1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2022),
  cluster = "pair_id"
)

model5.2 <- fepois(
  trade2 ~ brazil_export_eu2 + arg_export_eu2 + pry_export_eu2 + usa_export_eu2 + can_export_eu2 +
    brazil_export2 + arg_export2 + pry_export2 + usa_export2 + can_export2 + log_tariff2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2022),
  cluster = "pair_id"
)

model5.3 <- fepois(
  trade3 ~ brazil_export_eu3 + arg_export_eu3 + pry_export_eu3 + usa_export_eu3 + can_export_eu3 +
    brazil_export3 + arg_export3 + pry_export3 + usa_export3 + can_export3 + log_tariff3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2022),
  cluster = "pair_id"
)

model5.4 <- fepois(
  trade ~ brazil_export_eu + arg_export_eu + pry_export_eu + usa_export_eu + can_export_eu +
    brazil_export + arg_export + pry_export + usa_export + can_export + log_tariff4 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2022),
  cluster = "pair_id"
)

etable(model5.1, model5.2, model5.3, model5.4)

model6.1 <- fepois(
  trade1 ~ brazil_export_row1 + arg_export_row1 + pry_export_row1 + usa_export_row1 + can_export_row1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model6.2 <- fepois(
  trade2 ~ brazil_export_row2 + arg_export_row2 + pry_export_row2 + usa_export_row2 + can_export_row2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model6.3 <- fepois(
  trade3 ~ pry_export3 + pry_export_row3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model6.4 <- fepois(
  trade ~ pry_export_eu + pry_export_row + usa_export_row + can_export_row |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

etable(model6.1, model6.2, model6.3, model6.4)

model7.1 <- fepois(
  trade1 ~ row_BRA_export_eu1 + row_ARG_export_eu1 + row_PRY_export_eu1 + row_USA_export_eu1 + row_CAN_export_eu1 + log_tariff1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model7.2 <- fepois(
  trade2 ~ row_BRA_export_eu2 + row_ARG_export_eu2 + row_PRY_export_eu2 + row_USA_export_eu2 + row_CAN_export_eu2 + log_tariff2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model7.3 <- fepois(
  trade3 ~ row_BRA_export_eu3 + row_ARG_export_eu3 + row_PRY_export_eu3 + row_USA_export_eu3 + row_CAN_export_eu3 + log_tariff3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model7.4 <- fepois(
  trade ~ row_BRA_export_eu + row_ARG_export_eu + row_PRY_export_eu + row_USA_export_eu + row_CAN_export_eu + log_tariff4 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)
etable(model7.1, model7.2, model7.3, model7.4)

model8.1 <- fepois(
  trade1 ~ brazil_export_chn1 + arg_export_chn1 + pry_export_chn1 + usa_export_chn1 + can_export_chn1 + log_tariff1 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model8.2 <- fepois(
  trade2 ~ brazil_export_chn2 + arg_export_chn2 + pry_export_chn2 + usa_export_chn2 + can_export_chn2 + log_tariff2 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model8.3 <- fepois(
  trade3 ~ brazil_export_chn3 + arg_export_chn3 + pry_export_chn3 + usa_export_chn3 + can_export_chn3 + log_tariff3 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

model8.4 <- fepois(
  trade ~ brazil_export_chn + arg_export_chn + pry_export_chn + usa_export_chn + can_export_chn + log_tariff4 |
    pair_id + exp_year + imp_year,
  data = filter(data_model3, year %in% 2007:2017),
  cluster = "pair_id"
)

etable(model8.1, model8.2, model8.3, model8.4)


var_labels <- c(
  # EU-specific interaction terms
  "brazil_export_eu1" = "I(BRA × Importer) × log(consumption)",
  "arg_export_eu1" = "I(ARG × Importer) × log(consumption)",
  "pry_export_eu1" = "I(PRY × Importer) × log(consumption)",
  "usa_export_eu1" = "I(USA × Importer) × log(consumption)",
  "can_export_eu1" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_eu2" = "I(BRA × Importer) × log(consumption)",
  "arg_export_eu2" = "I(ARG × Importer) × log(consumption)",
  "pry_export_eu2" = "I(PRY × Importer) × log(consumption)",
  "usa_export_eu2" = "I(USA × Importer) × log(consumption)",
  "can_export_eu2" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_eu3" = "I(BRA × Importer) × log(consumption)",
  "arg_export_eu3" = "I(ARG × Importer) × log(consumption)",
  "pry_export_eu3" = "I(PRY × Importer) × log(consumption)",
  "usa_export_eu3" = "I(USA × Importer) × log(consumption)",
  "can_export_eu3" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_eu" = "I(BRA × Importer) × log(consumption)",
  "arg_export_eu" = "I(ARG × Importer) × log(consumption)",
  "pry_export_eu" = "I(PRY × Importer) × log(consumption)",
  "usa_export_eu" = "I(USA × Importer) × log(consumption)",
  "can_export_eu" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_chn1" = "I(BRA × Importer) × log(consumption)",
  "arg_export_chn1" = "I(ARG × Importer) × log(consumption)",
  "pry_export_chn1" = "I(PRY × Importer) × log(consumption)",
  "usa_export_chn1" = "I(USA × Importer) × log(consumption)",
  "can_export_chn1" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_chn2" = "I(BRA × Importer) × log(consumption)",
  "arg_export_chn2" = "I(ARG × Importer) × log(consumption)",
  "pry_export_chn2" = "I(PRY × Importer) × log(consumption)",
  "usa_export_chn2" = "I(USA × Importer) × log(consumption)",
  "can_export_chn2" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_chn3" = "I(BRA × Importer) × log(consumption)",
  "arg_export_chn3" = "I(ARG × Importer) × log(consumption)",
  "pry_export_chn3" = "I(PRY × Importer) × log(consumption)",
  "usa_export_chn3" = "I(USA × Importer) × log(consumption)",
  "can_export_chn3" = "I(CAN × Importer) × log(consumption)",
  "brazil_export_chn" = "I(BRA × Importer) × log(consumption)",
  "arg_export_chn" = "I(ARG × Importer) × log(consumption)",
  "pry_export_chn" = "I(PRY × Importer) × log(consumption)",
  "usa_export_chn" = "I(USA × Importer) × log(consumption)",
  "can_export_chn" = "I(CAN × Importer) × log(consumption)"
)

# Custom model names for your 8 models
model_names <- c(
  "Soybean (Importer = EU)" = "model3.1",
  "Soybean Oil (Importer = EU)" = "model3.2",
  "Soybean Cake (Importer = EU)" = "model3.3",
  "Soy Total (Importer = EU)" = "model3.4",
  "Soybean (Importer = CHN)" = "model8.1",
  "Soybean Oil (Importer = CHN)" = "model8.2",
  "Soybean Cake (Importer = CHN)" = "model8.3",
  "Soy Total (Importer = CHN)" = "model8.4"
)

# Goodness-of-fit mapping
gof_map <- data.frame(
  raw = c("nobs", "r.squared", "FE: exp_year", "FE: imp_year", "FE: pair_id"),
  clean = c("Observations", "R-squared", "Exporter-Year FE", "Importer-Year FE", "Pair FE"),
  fmt = c(0, 3, 0, 0, 0)
)

# Create the summary table
a <- modelsummary(
  list(
    "Soybean (Importer = EU)" = model3.1,
    "Soybean Oil (Importer = EU)" = model3.2,
    "Soybean Cake (Importer = EU)" = model3.3,
    "Soy Total (Importer = EU)" = model3.4,
    "Soybean (Importer = CHN)" = model8.1,
    "Soybean Oil (Importer = CHN)" = model8.2,
    "Soybean Cake (Importer = CHN)" = model8.3,
    "Soy Total (Importer = CHN)" = model8.4
  ),
  coef_map = var_labels,
  stars = TRUE,
  gof_map = gof_map,
  statistic = "({std.error})",
  output = "flextable"
)

saveRDS(a, "C:\\Users\\Manoj\\OneDrive - Kansas State University\\GRA with Dr. Villoria\\Replicating a paper\\R file\\R files for manuscript\\ppml_results_eu_consumption.rds")

##### Chnage in EU consumption vs change in Brazilian export to soybean oil and soybean cake ###


change_eu_consumption <- eu_consumption %>%
  filter(year %in% c(2007, 2017)) %>%
  select(year, consumption1, consumption2, consumption3, consumption) %>%
  pivot_longer(cols = -year, names_to = "commodity", values_to = "consumption_value") %>%
  group_by(commodity) %>%
  summarise(
    change = (last(consumption_value) - first(consumption_value)) / first(consumption_value) * 100,
    .groups = "drop"
  )


change_export_eu <- readRDS("gravity data//soy_gravity4.rds") %>%
  filter(year %in% c(2007, 2017)) %>%
  filter(exporter %in% c("USA", "CAN", "BRA", "ARG", "PRY") & importer %in% eu_codes) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v, trade = soy) %>%
  group_by(exporter, year) %>%
  summarize(
    trade1 = sum(trade1, na.rm = TRUE),
    trade2 = sum(trade2, na.rm = TRUE),
    trade3 = sum(trade3, na.rm = TRUE),
    trade = sum(trade, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(trade1, trade2, trade3, trade),
    names_to = "commodity",
    values_to = "export_value"
  ) %>%
  group_by(exporter, commodity) %>%
  summarise(
    export_2007 = export_value[year == 2007],
    export_2017 = export_value[year == 2017],
    change = (export_2017 - export_2007) / export_2007 * 100,
    .groups = "drop"
  )


country_exports <- data_model1 %>%
  filter(exporter != importer) %>%
  group_by(exporter, year) %>%
  summarise(
    total_export1 = sum(trade1, na.rm = TRUE),
    total_export2 = sum(trade2, na.rm = TRUE),
    total_export3 = sum(trade3, na.rm = TRUE),
    total_export = sum(trade, na.rm = TRUE),
    .groups = "drop"
  )


brazil_exports <- country_exports %>%
  filter(exporter == "BRA") %>%
  select(year,
    brazil_export1 = total_export1, brazil_export2 = total_export2,
    brazil_export3 = total_export3, brazil_export = total_export
  )

argentina_exports <- country_exports %>%
  filter(exporter == "ARG") %>%
  select(year,
    arg_export1 = total_export1, arg_export2 = total_export2,
    arg_export3 = total_export3, arg_export = total_export
  )

paraguay_exports <- country_exports %>%
  filter(exporter == "PRY") %>%
  select(year,
    pry_export1 = total_export1, pry_export2 = total_export2,
    pry_export3 = total_export3, pry_export = total_export
  )

usa_exports <- country_exports %>%
  filter(exporter == "USA") %>%
  select(year,
    usa_export1 = total_export1, usa_export2 = total_export2,
    usa_export3 = total_export3, usa_export = total_export
  )

canada_exports <- country_exports %>%
  filter(exporter == "CAN") %>%
  select(year,
    can_export1 = total_export1, can_export2 = total_export2,
    can_export3 = total_export3, can_export = total_export
  )


data_model2 <- data_model1 %>%
  left_join(brazil_exports, by = "year") %>%
  left_join(argentina_exports, by = "year") %>%
  left_join(paraguay_exports, by = "year") %>%
  left_join(usa_exports, by = "year") %>%
  left_join(canada_exports, by = "year")


BAP_area <- read_csv("C:/Users/Manoj/OneDrive - Kansas State University/GRA with Dr. Villoria/Data/BAP soybean area.csv") %>%
  group_by(Year) %>%
  summarise(bap_area = sum(Value), .groups = "drop")


data_model2 <- readRDS("gravity data//soy_gravity4.rds") %>%
  filter(year %in% 2007:2022) %>%
  rename(trade1 = soybean_v, trade2 = soyoil_v, trade3 = soycake_v, trade = soy) %>%
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
    pair_id = paste0(exporter, importer)
  ) %>%
  group_by(year) %>%
  arrange(importer) %>%
  mutate(
    log_tariff3 = log(tariff3 + 1),
    log_tariff1 = log(tariff1 + 1),
    log_tariff2 = log(tariff2 + 1),
    log_tariff4 = log(tariff4 + 1)
  ) %>%
  ungroup() %>%
  left_join(BAP_area, by = c("year" = "Year")) %>%
  mutate(
    exporter_i = ifelse(exporter %in% c("BRA", "ARG", "PRY"), 1, 0),
    importer_j = ifelse(eu_d == 1, 1, 0)
  ) %>%
  group_by(importer, year) %>%
  mutate(
    consumption_1 = log(sum(trade1)),
    consumption_2 = log(sum(trade2)),
    consumption_3 = log(sum(trade3)),
    consumption = log(sum(trade))
  ) %>%
  ungroup() %>%
  group_by(exporter, year) %>%
  mutate(
    export_1 = log(sum(trade1[exporter != importer])),
    export_2 = log(sum(trade2[exporter != importer])),
    export_3 = log(sum(trade3[exporter != importer])),
    export = log(sum(trade[exporter != importer]))
  ) %>%
  ungroup() %>%
  mutate(
    inter_con1 = consumption_1 * exporter_i * importer_j,
    inter_con2 = consumption_2 * exporter_i * importer_j,
    inter_con3 = consumption_3 * exporter_i * importer_j,
    inter_con = consumption * exporter_i * importer_j,
    inter_export1 = export_1 * exporter_i * importer_j,
    inter_export2 = export_2 * exporter_i * importer_j,
    inter_export3 = export_3 * exporter_i * importer_j,
    inter_export = export * exporter_i * importer_j
  ) %>%
  mutate(inter_production1 = log(bap_area) * exporter_i * importer_j)

model2.1 <- fixest::fepois(trade1 ~ inter_con1 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model2.2 <- fixest::fepois(trade2 ~ inter_con2 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model2.3 <- fixest::fepois(trade3 ~ inter_con3 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model2.4 <- fixest::fepois(trade ~ inter_con | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)


etable(model2.1, model2.2, model2.3, model2.4)


model3.1 <- fixest::fepois(trade1 ~ inter_export1 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model3.2 <- fixest::fepois(trade2 ~ inter_export2 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model3.3 <- fixest::fepois(trade3 ~ inter_export3 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model3.4 <- fixest::fepois(trade ~ inter_export | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)


etable(model3.1, model3.2, model3.3, model3.4)

model4.1 <- fixest::fepois(trade1 ~ inter_production1 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model4.2 <- fixest::fepois(trade2 ~ inter_production1 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model4.3 <- fixest::fepois(trade3 ~ inter_production1 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)
model4.4 <- fixest::fepois(trade ~ inter_production1 | exp_year + imp_year + pair_id,
  data = filter(data_model2, year %in% c(2007:2017)), cluster = c("pair_id")
)


etable(model4.1, model4.2, model4.3, model4.4)
